const std = @import("std");
const Pool = @import("pool.zig").Pool;
const Buffer = @import("buffer.zig").Buffer;
const Reader = std.Io.Reader;

pub const Format = enum {
    float32_le,
    uint8,
    int16_le,

    pub fn byteLength(self: Format) usize {
        return switch (self) {
            .float32_le => 4,
            .uint8 => 1,
            .int16_le => 2,
        };
    }
};

pub const Mux = struct {
    sample_rate: u32,
    channel_count: u8,
    format: Format,
    players: std.array_list.Managed(*Player),
    allocator: std.mem.Allocator,
    mutex: std.Thread.Mutex = .{},
    condition: std.Thread.Condition = .{},
    shutdown: bool = false,

    pub fn init(allocator: std.mem.Allocator, sample_rate: u32, channel_count: u8, format: Format) !*Mux {
        const self = try allocator.create(Mux);
        self.* = Mux{
            .sample_rate = sample_rate,
            .channel_count = channel_count,
            .format = format,
            .allocator = allocator,
            .players = std.array_list.Managed(*Player).init(allocator),
        };
        const thread = try std.Thread.spawn(.{}, muxLoop, .{self});
        thread.detach();
        return self;
    }

    pub fn deinit(self: *Mux) void {
        // Signal shutdown and wake up the mux loop
        self.mutex.lock();
        self.shutdown = true;
        self.condition.signal();
        self.mutex.unlock();

        // Give the loop time to exit
        std.Thread.sleep(std.time.ns_per_ms * 10);

        self.players.deinit();
        self.allocator.destroy(self);
    }

    pub fn newPlayer(self: *Mux, src: *Reader) !*Player {
        const player = try self.allocator.create(Player);
        player.* = Player{
            .mux = self,
            .src = src,
            .previous_volume = 1.0,
            .volume = 1.0,
            .buffer = std.array_list.Managed(u8).init(self.allocator),
            .buffer_size = self.defaultBufferSize(),
        };
        return player;
    }

    pub fn addPlayer(self: *Mux, player: *Player) !void {
        self.mutex.lock();
        defer self.mutex.unlock();

        // Check if player is already in the list to prevent duplicates
        for (self.players.items) |p| {
            if (p == player) {
                // Player is already in the list, don't add it again
                return;
            }
        }

        try self.players.append(player);
        self.condition.signal();
    }

    pub fn readFloat32s(self: *Mux, dst: []f32) !void {
        self.mutex.lock();

        const players = try self.players.clone();
        defer players.deinit();
        self.mutex.unlock();

        @memset(dst, 0);

        for (players.items) |player| {
            _ = player.readBufferAndAdd(dst);
        }

        self.condition.signal();
    }

    pub fn removePlayer(self: *Mux, player: *Player) void {
        self.mutex.lock();
        defer self.mutex.unlock();

        for (self.players.items, 0..) |p, i| {
            if (p == player) {
                _ = self.players.orderedRemove(i);
                break;
            }
        }
        self.condition.signal();
    }

    fn defaultBufferSize(self: *Mux) usize {
        const bytes_per_sample = @as(usize, @intCast(self.channel_count)) * self.format.byteLength();
        const s = self.sample_rate * bytes_per_sample / 2;
        return (s / bytes_per_sample) * bytes_per_sample;
    }

    fn wait(self: *Mux) void {
        self.mutex.lock();
        defer self.mutex.unlock();

        // Loop until we should proceed
        while (self.shouldWait()) {
            // Atomically release the mutex and block until signaled,
            // then re-acquire before returning
            self.condition.wait(&self.mutex);
        }
    }

    fn shouldWait(self: *Mux) bool {
        if (self.shutdown) {
            return false;
        }
        for (self.players.items) |player| {
            if (player.canReadSourceToBuffer()) {
                return false;
            }
        }
        return true;
    }
};

fn muxLoop(self: *Mux) !void {
    var players = std.array_list.Managed(*Player).init(self.allocator);
    defer players.deinit();

    while (true) {
        self.wait();

        // Check if shutdown was requested
        self.mutex.lock();
        const should_shutdown = self.shutdown;
        if (should_shutdown) {
            self.mutex.unlock();
            break;
        }

        players.clearRetainingCapacity();
        try players.appendSlice(self.players.items);
        self.mutex.unlock();

        var all_zero = true;
        for (players.items) |player| {
            const n = try player.readSourceToBuffer();
            if (n != 0) {
                all_zero = false;
            }
        }

        // Sleeping is necessary especially on browsers.
        // Sometimes a player continues to read 0 bytes from the source and this loop can be a busy loop in such case.
        if (all_zero) {
            std.Thread.sleep(std.time.ns_per_ms);
        }
    }
}

const PlayerState = enum {
    paused,
    play,
    closed,
};

pub const Player = struct {
    mux: *Mux,
    src: *Reader,
    previous_volume: f64,
    volume: f64,
    state: PlayerState = .paused,
    buffer_pool: ?Pool = null,
    buffer: std.array_list.Managed(u8),
    eof: bool = false,
    buffer_size: usize,
    mutex: std.Thread.Mutex = .{},

    pub fn play(self: *Player) !void {
        const thread = try std.Thread.spawn(.{}, Player.playThread, .{self});
        thread.join();
    }

    pub fn pause(self: *Player) void {
        self.mutex.lock();
        defer self.mutex.unlock();

        if (self.state != .play) {
            return;
        }
        self.state = .paused;
    }

    pub fn setBufferSize(self: *Player, buffer_size: usize) void {
        self.mutex.lock();
        defer self.mutex.unlock();

        const original_size = self.buffer_size;
        self.buffer_size = buffer_size;
        if (buffer_size == 0) {
            self.buffer_size = self.mux.defaultBufferSize();
        }
        if (original_size != self.buffer_size) {
            if (self.buffer_pool) |p| p.deinit();
            self.buffer_pool = null;
        }
    }

    pub fn reset(self: *Player) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        self.resetImpl();
    }

    pub fn isPlaying(self: *Player) bool {
        self.mutex.lock();
        defer self.mutex.unlock();
        return self.state == .play;
    }

    pub fn getVolume(self: *Player) f64 {
        self.mutex.lock();
        defer self.mutex.unlock();
        return self.volume;
    }

    pub fn setVolume(self: *Player, volume: f64) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        self.volume = volume;
        if (self.state != .play) {
            self.previous_volume = volume;
        }
    }

    pub fn bufferedSize(self: *Player) usize {
        self.mutex.lock();
        defer self.mutex.unlock();
        return self.buffer.items.len;
    }

    pub fn close(self: *Player) !void {
        self.mutex.lock();
        defer self.mutex.unlock();
        try self.closeImpl();
    }

    pub fn deinit(self: *Player) void {
        self.buffer.clearAndFree();
        if (self.buffer_pool) |*p| {
            p.deinit();
        }
        self.mux.allocator.destroy(self);
    }

    fn playThread(ctx: *anyopaque) !void {
        var self: *Player = @ptrCast(@alignCast(ctx));
        self.mutex.lock();
        defer self.mutex.unlock();
        try self.playImpl();
    }

    fn playImpl(self: *Player) !void {
        if (self.state != .paused) {
            return;
        }
        self.state = .play;

        if (!self.eof) {
            const buf = try self.getTempBuffer();
            defer {
                if (self.buffer_pool) |*p| {
                    p.*.release(buf);
                }
            }

            while (self.buffer.items.len < self.buffer_size) {
                const bytes_read = try self.read(buf.buf);
                try self.buffer.appendSlice(buf.buf[0..bytes_read]);
                if (bytes_read == 0) {
                    self.eof = true;
                    break;
                }
            }
        }

        if (self.eof and self.buffer.items.len == 0) {
            self.state = .paused;
        }

        try self.addToPlayers();
    }

    fn resetImpl(self: *Player) void {
        if (self.state == .closed) {
            return;
        }
        self.state = .paused;
        self.buffer.clearAndFree();
        self.eof = false;
    }

    fn closeImpl(self: *Player) !void {
        self.removeFromPlayers();

        if (self.state == .closed) {
            return error.PlayerAlreadyClosed;
        }
        self.state = .closed;
        self.buffer.clearAndFree();
        if (self.buffer_pool) |*p| {
            p.deinit();
        }
    }

    fn addToPlayers(self: *Player) !void {
        // Note: This function assumes the caller already holds the mutex
        try self.mux.addPlayer(self);
    }

    fn removeFromPlayers(self: *Player) void {
        // Note: This function assumes the caller already holds the mutex
        self.mux.removePlayer(self);
    }

    fn read(self: *Player, buf: []u8) !usize {
        // Note: This function assumes the caller already holds the mutex
        return self.src.readSliceShort(buf) catch |err| {
            std.log.err("Player.read: src.read failed with error: {s}", .{@errorName(err)});
            return err;
        };
    }

    fn canReadSourceToBuffer(self: *Player) bool {
        self.mutex.lock();
        defer self.mutex.unlock();

        if (self.eof) {
            return false;
        }

        return self.buffer.items.len < self.buffer_size;
    }

    fn readBufferAndAdd(self: *Player, dst: []f32) usize {
        self.mutex.lock();
        defer self.mutex.unlock();

        if (self.state != .play) {
            return 0;
        }

        const format = self.mux.format;
        const bit_depth_in_bytes = format.byteLength();
        var n = dst.len;
        const bytes_needed = n * bit_depth_in_bytes;
        if (bytes_needed > self.buffer.items.len) {
            n = self.buffer.items.len / bit_depth_in_bytes;
        }

        const previous_volume: f32 = @floatCast(self.previous_volume);
        const volume: f32 = @floatCast(self.volume);

        const channel_count = self.mux.channel_count;
        const rate_denominator: f32 = @as(f32, @floatFromInt(n)) / @as(f32, @floatFromInt(channel_count));

        const src = self.buffer.items[0 .. n * bit_depth_in_bytes];

        for (0..n) |i| {
            const v: f32 = switch (format) {
                .float32_le => @bitCast(@as(u32, src[4 * i]) |
                    (@as(u32, src[4 * i + 1]) << 8) |
                    (@as(u32, src[4 * i + 2]) << 16) |
                    (@as(u32, src[4 * i + 3]) << 24)),
                .uint8 => blk: {
                    const v8 = src[i];
                    break :blk @as(f32, @floatFromInt(v8 - (1 << 7))) / (1 << 7);
                },
                .int16_le => blk: {
                    const v16_unsigned = @as(u16, src[2 * i]) | (@as(u16, src[2 * i + 1]) << 8);
                    const v16_signed = @as(i16, @bitCast(v16_unsigned));
                    break :blk @as(f32, @floatFromInt(v16_signed)) / (1 << 15);
                },
            };
            if (volume == previous_volume) {
                dst[i] += v * volume;
            } else {
                var rate = @as(f32, @floatFromInt(i)) / @as(f32, @floatFromInt(channel_count)) / rate_denominator;
                if (rate > 1) {
                    rate = 1;
                }
                dst[i] += v * (volume * rate + previous_volume * (1 - rate));
            }
        }

        self.previous_volume = volume;

        const consumed_bytes = n * bit_depth_in_bytes;
        const remaining_bytes = self.buffer.items.len - consumed_bytes;

        if (remaining_bytes > 0) {
            // Copy remaining data to the front of the buffer
            std.mem.copyForwards(u8, self.buffer.items[0..remaining_bytes], self.buffer.items[consumed_bytes..]);
        }

        // Resize buffer to only include remaining data
        self.buffer.items = self.buffer.items[0..remaining_bytes];

        if (self.eof and self.buffer.items.len == 0) {
            self.state = .paused;
        }

        return n;
    }

    fn readSourceToBuffer(self: *Player) !usize {
        self.mutex.lock();
        defer self.mutex.unlock();

        if (self.state == .closed) {
            return 0;
        }

        if (self.buffer.items.len >= self.buffer_size) {
            return 0;
        }

        const buf = try self.getTempBuffer();
        defer {
            if (self.buffer_pool) |*p| {
                p.*.release(buf);
            }
        }
        const n = try self.read(buf.buf);
        try self.buffer.appendSlice(buf.buf[0..n]);
        if (n == 0) {
            self.eof = true;
            if (self.buffer.items.len == 0) {
                self.state = .paused;
            }
        }
        return n;
    }

    fn getTempBuffer(self: *Player) !*Buffer {
        if (self.buffer_pool == null) {
            // Create a pool with a reasonable number of buffers (e.g., 10)
            // and use the actual buffer_size for the buffer size
            self.buffer_pool = try Pool.init(self.mux.allocator, 1, self.buffer_size);
        }

        const buffer = try self.buffer_pool.?.acquire();
        return buffer;
    }
};

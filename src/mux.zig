const std = @import("std");
const builtin = @import("builtin");
const Pool = @import("pool.zig").Pool;
const Buffer = @import("buffer.zig").Buffer;
const Reader = std.Io.Reader;

const buffer_pool_initial_size: u16 = 4;

const CompatMutex = if (builtin.single_threaded) struct {} else struct {
    inner: std.Io.Mutex = .init,

    fn lock(self: *@This()) void {
        self.inner.lockUncancelable(std.Options.debug_io);
    }

    fn unlock(self: *@This()) void {
        self.inner.unlock(std.Options.debug_io);
    }
};

const CompatCondition = if (builtin.single_threaded) struct {} else struct {
    inner: std.Io.Condition = .init,

    fn wait(self: *@This(), mutex: *CompatMutex) void {
        self.inner.waitUncancelable(std.Options.debug_io, &mutex.inner);
    }

    fn signal(self: *@This()) void {
        self.inner.signal(std.Options.debug_io);
    }
};

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
    buffer_pool: Pool,
    allocator: std.mem.Allocator,
    mutex: CompatMutex = .{},
    condition: CompatCondition = .{},
    shutdown: bool = false,
    thread: if (builtin.single_threaded) ?void else ?std.Thread = null,
    ready: bool = false,
    err: ?anyerror = null,

    pub fn init(allocator: std.mem.Allocator, sample_rate: u32, channel_count: u8, format: Format) !*Mux {
        const self = try allocator.create(Mux);
        self.* = Mux{
            .sample_rate = sample_rate,
            .channel_count = channel_count,
            .format = format,
            .allocator = allocator,
            .players = std.array_list.Managed(*Player).init(allocator),
            .buffer_pool = undefined,
        };
        self.buffer_pool = Pool.init(self.allocator, buffer_pool_initial_size, self.defaultBufferSize()) catch |err| {
            self.players.deinit();
            self.allocator.destroy(self);
            return err;
        };
        errdefer {
            self.buffer_pool.deinit();
            self.players.deinit();
            self.allocator.destroy(self);
        }

        // For single-threaded WASM, don't spawn a thread - process synchronously in readFloat32s
        if (builtin.single_threaded) {
            self.thread = null;
        } else {
            self.thread = try std.Thread.spawn(.{}, muxLoop, .{self});
        }
        return self;
    }

    pub fn deinit(self: *Mux) void {
        if (!builtin.single_threaded) {
            // Signal shutdown and wake up the mux loop
            self.mutex.lock();
            self.shutdown = true;
            self.condition.signal();
            self.mutex.unlock();

            // Wait for the thread to finish properly
            if (self.thread) |thread| {
                thread.join();
            }
        } else {
            self.shutdown = true;
        }

        self.players.deinit();
        self.buffer_pool.deinit();
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
        if (!builtin.single_threaded) self.mutex.lock();
        defer if (!builtin.single_threaded) self.mutex.unlock();

        try self.addPlayerLocked(player);
    }

    /// Caller holds the mixer mutex in multithreaded builds.
    fn addPlayerLocked(self: *Mux, player: *Player) !void {
        // Check if player is already in the list to prevent duplicates
        for (self.players.items) |p| {
            if (p == player) {
                // Player is already in the list, don't add it again
                return;
            }
        }

        try self.players.append(player);
        if (!builtin.single_threaded) {
            self.condition.signal();
        }
    }

    pub fn readFloat32s(self: *Mux, dst: []f32) !void {
        var players = std.array_list.Managed(*Player).init(self.allocator);
        defer players.deinit();
        {
            if (!builtin.single_threaded) self.mutex.lock();
            defer if (!builtin.single_threaded) self.mutex.unlock();
            try self.retainPlayersLocked(&players);
        }
        defer self.releasePlayers(&players);

        // Without a worker thread, fill source buffers synchronously before mixing.
        if (builtin.single_threaded) {
            for (players.items) |player| {
                _ = try player.readSourceToBuffer();
            }
        }

        @memset(dst, 0);
        for (players.items) |player| {
            _ = player.readBufferAndAdd(dst);
        }

        if (!builtin.single_threaded) self.condition.signal();
    }

    pub fn removePlayer(self: *Mux, player: *Player) void {
        if (!builtin.single_threaded) self.mutex.lock();
        defer if (!builtin.single_threaded) self.mutex.unlock();

        self.removePlayerLocked(player);
    }

    /// Caller holds the mixer mutex in multithreaded builds.
    fn removePlayerLocked(self: *Mux, player: *Player) void {
        for (self.players.items, 0..) |p, i| {
            if (p == player) {
                _ = self.players.orderedRemove(i);
                break;
            }
        }
        if (!builtin.single_threaded) {
            self.condition.signal();
        }
    }

    pub fn waitForReady(self: *Mux) void {
        if (builtin.single_threaded) {
            return;
        }

        self.mutex.lock();
        defer self.mutex.unlock();

        while (!self.ready) {
            self.condition.wait(&self.mutex);
        }
    }

    pub fn setReady(self: *Mux, ready: bool) void {
        if (!builtin.single_threaded) self.mutex.lock();
        defer if (!builtin.single_threaded) self.mutex.unlock();

        self.ready = ready;

        if (!builtin.single_threaded) {
            self.condition.signal();
        }
    }

    pub fn getErr(self: *Mux) ?anyerror {
        if (!builtin.single_threaded) self.mutex.lock();
        defer if (!builtin.single_threaded) self.mutex.unlock();

        return self.err;
    }

    pub fn setErr(self: *Mux, err: ?anyerror) void {
        if (!builtin.single_threaded) self.mutex.lock();
        defer if (!builtin.single_threaded) self.mutex.unlock();

        self.err = err;
    }

    fn defaultBufferSize(self: *Mux) usize {
        const bytes_per_sample = @as(usize, @intCast(self.channel_count)) * self.format.byteLength();
        const s = self.sample_rate * bytes_per_sample / 2;
        return (s / bytes_per_sample) * bytes_per_sample;
    }

    fn wait(self: *Mux) void {
        if (builtin.single_threaded) {
            // Single-threaded: no waiting needed
            return;
        }
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

    fn acquireBuffer(self: *Mux, size: usize) !*Buffer {
        const buf = try self.buffer_pool.acquire();
        errdefer self.buffer_pool.release(buf);
        if (buf.buf.len < size) {
            try buf.ensureTotalCapacity(size);
        }
        return buf;
    }

    fn releaseBuffer(self: *Mux, buffer: *Buffer) void {
        self.buffer_pool.release(buffer);
    }

    /// Caller holds the mixer mutex. An empty snapshot is required.
    fn retainPlayersLocked(
        self: *Mux,
        players: *std.array_list.Managed(*Player),
    ) !void {
        std.debug.assert(players.items.len == 0);
        // Allocate before retaining so failure leaves no outstanding borrows.
        try players.appendSlice(self.players.items);
        for (players.items) |player| player.snapshot_refs += 1;
    }

    fn releasePlayers(
        self: *Mux,
        players: *std.array_list.Managed(*Player),
    ) void {
        if (!builtin.single_threaded) self.mutex.lock();
        defer if (!builtin.single_threaded) self.mutex.unlock();

        for (players.items) |player| {
            std.debug.assert(player.snapshot_refs > 0);
            player.snapshot_refs -= 1;
            if (!builtin.single_threaded and player.snapshot_refs == 0) {
                player.snapshots_drained.signal();
            }
        }
        players.clearRetainingCapacity();
    }
};

fn muxLoop(self: *Mux) !void {
    var players = std.array_list.Managed(*Player).init(self.allocator);
    defer players.deinit();

    while (true) {
        self.wait();

        // Check if shutdown was requested
        {
            self.mutex.lock();
            defer self.mutex.unlock();
            if (self.shutdown) break;
            try self.retainPlayersLocked(&players);
        }
        // Runs on every iteration, including a source-read error.
        defer self.releasePlayers(&players);

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
            std.Io.sleep(std.Options.debug_io, .fromNanoseconds(std.time.ns_per_ms), .awake) catch {};
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
    buffer: std.array_list.Managed(u8),
    eof: bool = false,
    buffer_size: usize,
    mutex: CompatMutex = .{},
    // Protected by mux.mutex, including the condition wait.
    snapshot_refs: usize = 0,
    snapshots_drained: CompatCondition = .{},

    pub fn play(self: *Player) !void {
        // Eensure the first player lock is released before acquiring the mixer lock
        {
            if (!builtin.single_threaded) self.mutex.lock();
            defer if (!builtin.single_threaded) self.mutex.unlock();

            if (self.state != .paused) return;
            try self.prepareBuffer();
        }
        if (!builtin.single_threaded) self.mux.mutex.lock();
        defer if (!builtin.single_threaded) self.mux.mutex.unlock();

        if (!builtin.single_threaded) self.mutex.lock();
        defer if (!builtin.single_threaded) self.mutex.unlock();

        // Another control call may have changed state during preparation.
        if (self.state != .paused) return;

        try self.mux.addPlayerLocked(self);
        self.state =
            if (self.eof and self.buffer.items.len == 0) .paused else .play;
    }

    pub fn pause(self: *Player) void {
        if (!builtin.single_threaded) {
            self.mutex.lock();
        }
        defer if (!builtin.single_threaded) {
            self.mutex.unlock();
        };

        if (self.state != .play) {
            return;
        }
        self.state = .paused;
    }

    pub fn setBufferSize(self: *Player, buffer_size: usize) void {
        if (!builtin.single_threaded) {
            self.mutex.lock();
        }
        defer if (!builtin.single_threaded) {
            self.mutex.unlock();
        };

        self.buffer_size = buffer_size;
        if (self.buffer_size == 0) {
            self.buffer_size = self.mux.defaultBufferSize();
        }
    }

    pub fn reset(self: *Player) void {
        if (!builtin.single_threaded) {
            self.mutex.lock();
        }
        defer if (!builtin.single_threaded) {
            self.mutex.unlock();
        };
        self.resetImpl();
    }

    pub fn isPlaying(self: *Player) bool {
        if (!builtin.single_threaded) {
            self.mutex.lock();
        }
        defer if (!builtin.single_threaded) {
            self.mutex.unlock();
        };
        return self.state == .play;
    }

    pub fn getVolume(self: *Player) f64 {
        if (!builtin.single_threaded) {
            self.mutex.lock();
        }
        defer if (!builtin.single_threaded) {
            self.mutex.unlock();
        };
        return self.volume;
    }

    pub fn setVolume(self: *Player, volume: f64) void {
        if (!builtin.single_threaded) {
            self.mutex.lock();
        }
        defer if (!builtin.single_threaded) {
            self.mutex.unlock();
        };
        self.volume = volume;
        if (self.state != .play) {
            self.previous_volume = volume;
        }
    }

    pub fn bufferedSize(self: *Player) usize {
        if (!builtin.single_threaded) {
            self.mutex.lock();
        }
        defer if (!builtin.single_threaded) {
            self.mutex.unlock();
        };
        return self.buffer.items.len;
    }

    pub fn close(self: *Player) !void {
        if (!builtin.single_threaded) self.mux.mutex.lock();
        defer if (!builtin.single_threaded) self.mux.mutex.unlock();

        if (!builtin.single_threaded) self.mutex.lock();
        defer if (!builtin.single_threaded) self.mutex.unlock();

        try self.closeImpl();
    }

    /// The caller must stop all other public calls on this player first.
    /// Keep the source reader and mixer alive until this returns. Do not call
    /// from a source read callback; this waits for outstanding mixer uses.
    pub fn deinit(self: *Player) void {
        {
            if (!builtin.single_threaded) self.mux.mutex.lock();
            defer if (!builtin.single_threaded) self.mux.mutex.unlock();

            self.mux.removePlayerLocked(self);
            if (!builtin.single_threaded) {
                while (self.snapshot_refs != 0) {
                    self.snapshots_drained.wait(&self.mux.mutex);
                }
            } else {
                std.debug.assert(self.snapshot_refs == 0);
            }
        }

        self.buffer.clearAndFree();
        self.mux.allocator.destroy(self);
    }

    /// Caller holds the player mutex; this function never takes the mixer mutex.
    fn prepareBuffer(self: *Player) !void {
        if (!self.eof) {
            const buf = try self.getTempBuffer();
            defer self.mux.releaseBuffer(buf);

            const chunk = buf.buf[0..self.buffer_size];

            while (self.buffer.items.len < self.buffer_size) {
                const bytes_read = try self.read(chunk);
                try self.buffer.appendSlice(chunk[0..bytes_read]);
                if (bytes_read == 0) {
                    self.eof = true;
                    break;
                }
            }
        }
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
        self.mux.removePlayerLocked(self);

        if (self.state == .closed) {
            return error.PlayerAlreadyClosed;
        }
        self.state = .closed;
        self.buffer.clearAndFree();
    }

    fn read(self: *Player, buf: []u8) !usize {
        // Note: This function assumes the caller already holds the mutex
        return self.src.readSliceShort(buf) catch |err| {
            std.log.err("Player.read: src.read failed with error: {s}", .{@errorName(err)});
            return err;
        };
    }

    fn canReadSourceToBuffer(self: *Player) bool {
        if (!builtin.single_threaded) {
            self.mutex.lock();
        }
        defer if (!builtin.single_threaded) {
            self.mutex.unlock();
        };

        if (self.eof) {
            return false;
        }

        return self.buffer.items.len < self.buffer_size;
    }

    fn readBufferAndAdd(self: *Player, dst: []f32) usize {
        if (!builtin.single_threaded) {
            self.mutex.lock();
        }
        defer if (!builtin.single_threaded) {
            self.mutex.unlock();
        };

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
        if (!builtin.single_threaded) {
            self.mutex.lock();
        }
        defer if (!builtin.single_threaded) {
            self.mutex.unlock();
        };

        if (self.state == .closed) {
            return 0;
        }

        if (self.buffer.items.len >= self.buffer_size) {
            return 0;
        }

        const buf = try self.getTempBuffer();
        defer self.mux.releaseBuffer(buf);
        const chunk = buf.buf[0..self.buffer_size];
        const n = try self.read(chunk);
        try self.buffer.appendSlice(chunk[0..n]);
        if (n == 0) {
            self.eof = true;
            if (self.buffer.items.len == 0) {
                self.state = .paused;
            }
        }
        return n;
    }

    fn getTempBuffer(self: *Player) !*Buffer {
        return try self.mux.acquireBuffer(self.buffer_size);
    }
};

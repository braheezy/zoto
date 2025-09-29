const std = @import("std");
const mux = @import("mux.zig");
const Mux = mux.Mux;
const Player = mux.Player;
const Format = mux.Format;

const c = @cImport({
    @cInclude("alsa/asoundlib.h");
});

const float32_size_in_bytes: usize = @sizeOf(f32);
const period_count: usize = 2;

const DriverError = error{
    AlsaFailure,
    DeviceOpenFailed,
    BufferAllocationFailed,
};

const OpenError = struct {
    name: []u8,
    code: c_int,
};

pub const Context = struct {
    allocator: std.mem.Allocator,
    channel_count: u32,
    mux: *Mux,
    handle: ?*c.snd_pcm_t = null,
    mutex: std.Thread.Mutex = .{},
    condition: std.Thread.Condition = .{},
    suspended: bool = false,
    shutdown: bool = false,
    ready: bool = false,
    err: ?anyerror = null,
    buf32: ?[]f32 = null,
    bytes_per_frame: usize,
    period_size_frames: usize = 0,

    pub fn init(allocator: std.mem.Allocator, sample_rate: u32, channel_count: u32, format: Format, buffer_size_in_bytes: u32) !*Context {
        const mux_ptr = try Mux.init(
            allocator,
            sample_rate,
            @intCast(channel_count),
            format,
        );
        errdefer mux_ptr.deinit();

        const ctx = try allocator.create(Context);
        errdefer allocator.destroy(ctx);

        ctx.* = Context{
            .allocator = allocator,
            .channel_count = channel_count,
            .mux = mux_ptr,
            .bytes_per_frame = @as(usize, @intCast(channel_count)) * float32_size_in_bytes,
        };

        const thread = std.Thread.spawn(.{}, audioContextWorker, .{ ctx, sample_rate, channel_count, buffer_size_in_bytes }) catch |err| {
            allocator.destroy(ctx);
            mux_ptr.deinit();
            return err;
        };
        thread.detach();

        return ctx;
    }

    pub fn deinit(self: *Context) void {
        self.mutex.lock();
        self.shutdown = true;
        self.condition.signal();
        self.mutex.unlock();

        // Give the worker thread a moment to exit cleanly
        std.Thread.sleep(std.time.ns_per_ms * 100);

        if (self.buf32) |buf| {
            self.allocator.free(buf);
        }

        if (self.handle) |handle| {
            _ = c.snd_pcm_drop(handle);
            _ = c.snd_pcm_close(handle);
        }

        self.mux.deinit();
        self.allocator.destroy(self);
    }

    pub fn waitForReady(self: *Context) void {
        self.mutex.lock();
        defer self.mutex.unlock();

        while (!self.ready) {
            self.condition.wait(&self.mutex);
        }
    }

    pub fn pause(self: *Context) !void {
        self.mutex.lock();
        defer self.mutex.unlock();

        if (self.err) |stored_err| return stored_err;

        self.suspended = true;
        self.condition.signal();
    }

    pub fn play(self: *Context) !void {
        self.mutex.lock();
        defer self.mutex.unlock();

        if (self.err) |stored_err| return stored_err;

        self.suspended = false;
        self.condition.signal();
    }

    pub fn getErr(self: *Context) ?anyerror {
        self.mutex.lock();
        defer self.mutex.unlock();
        return self.err;
    }

    pub fn newPlayer(self: *Context, reader: *std.Io.Reader) !*Player {
        return try self.mux.newPlayer(reader);
    }

    fn setup(self: *Context, sample_rate: u32, channel_count: u32, buffer_size_in_bytes: u32) !void {
        try self.openDevice();

        var period_size: c.snd_pcm_uframes_t = 1024;
        if (buffer_size_in_bytes != 0) {
            const desired_total = @as(usize, buffer_size_in_bytes);
            const denom = self.bytes_per_frame * period_count;
            const frames_per_period = if (denom != 0) desired_total / denom else 0;
            period_size = @max(1, @as(c.snd_pcm_uframes_t, @intCast(frames_per_period)));
        }
        var buffer_size: c.snd_pcm_uframes_t = period_size * @as(c.snd_pcm_uframes_t, @intCast(period_count));

        try self.alsaPcmHwParams(sample_rate, channel_count, &buffer_size, &period_size);

        self.period_size_frames = @intCast(period_size);
        const total_samples = self.period_size_frames * @as(usize, @intCast(channel_count));
        self.buf32 = self.allocator.alloc(f32, total_samples) catch {
            return error.BufferAllocationFailed;
        };
    }

    fn openDevice(self: *Context) !void {
        var errors = std.ArrayList(OpenError).empty;
        defer {
            for (errors.items) |entry| {
                self.allocator.free(entry.name);
            }
            errors.deinit(self.allocator);
        }

        const defaults = [_][]const u8{ "default", "plug:default" };
        for (defaults) |candidate| {
            if (try self.tryOpenCandidate(candidate, &errors)) {
                return;
            }
        }

        if (try self.openFromHints(&errors)) {
            return;
        }

        self.logOpenErrors(errors.items);
        return error.DeviceOpenFailed;
    }

    fn openFromHints(self: *Context, errors: *std.ArrayList(OpenError)) !bool {
        const allocator = self.allocator;
        const pcm_iface = try allocCString(allocator, "pcm");
        defer allocator.free(pcm_iface);

        var hints: ?[*c]?*anyopaque = null;
        const hint_result = c.snd_device_name_hint(-1, cPtr(pcm_iface), @ptrCast(&hints));
        if (hint_result != 0 or hints == null) {
            return false;
        }
        defer {
            _ = c.snd_device_name_free_hint(@ptrCast(hints.?));
        }

        const io_id = try allocCString(allocator, "IOID");
        defer allocator.free(io_id);
        const name_id = try allocCString(allocator, "NAME");
        defer allocator.free(name_id);

        const hint_list = hints.?;
        var index: usize = 0;
        while (true) : (index += 1) {
            const hint_ptr = hint_list[index];
            if (hint_ptr == null) break;

            const io_ptr = c.snd_device_name_get_hint(hint_ptr.?, cPtr(io_id));
            if (io_ptr != null) {
                defer c.free(io_ptr);
                const io_slice = std.mem.sliceTo(io_ptr.?, 0);
                if (std.mem.eql(u8, io_slice, "Input")) {
                    continue;
                }
            }

            const name_ptr = c.snd_device_name_get_hint(hint_ptr.?, cPtr(name_id));
            if (name_ptr == null) {
                continue;
            }
            defer c.free(name_ptr);

            const name_slice = std.mem.sliceTo(name_ptr.?, 0);
            if (std.mem.eql(u8, name_slice, "null") or std.mem.eql(u8, name_slice, "default")) {
                continue;
            }

            const name = try allocator.dupe(u8, name_slice);
            defer allocator.free(name);

            if (try self.tryOpenCandidate(name, errors)) {
                return true;
            }
        }

        return false;
    }

    fn tryOpenCandidate(self: *Context, candidate: []const u8, errors: *std.ArrayList(OpenError)) !bool {
        const allocator = self.allocator;
        const c_name = try allocCString(allocator, candidate);
        defer allocator.free(c_name);

        var handle: ?*c.snd_pcm_t = null;
        const open_err = c.snd_pcm_open(&handle, cPtr(c_name), c.SND_PCM_STREAM_PLAYBACK, 0);
        if (open_err < 0) {
            const copy = try allocator.dupe(u8, candidate);
            errdefer allocator.free(copy);
            try errors.append(self.allocator, .{ .name = copy, .code = open_err });
            return false;
        }

        self.handle = handle.?;
        return true;
    }

    fn alsaPcmHwParams(self: *Context, sample_rate: u32, channel_count: u32, buffer_size: *c.snd_pcm_uframes_t, period_size: *c.snd_pcm_uframes_t) DriverError!void {
        var params: ?*c.snd_pcm_hw_params_t = null;
        const rc_malloc = c.snd_pcm_hw_params_malloc(&params);
        if (rc_malloc < 0) {
            return alsaError("snd_pcm_hw_params_malloc", rc_malloc);
        }
        if (params == null) {
            return alsaError("snd_pcm_hw_params_malloc", -1);
        }
        defer c.snd_pcm_hw_params_free(params.?);

        const handle = self.handle.?;

        var rc = c.snd_pcm_hw_params_any(handle, params.?);
        if (rc < 0) return alsaError("snd_pcm_hw_params_any", rc);

        rc = c.snd_pcm_hw_params_set_access(handle, params.?, c.SND_PCM_ACCESS_RW_INTERLEAVED);
        if (rc < 0) return alsaError("snd_pcm_hw_params_set_access", rc);

        rc = c.snd_pcm_hw_params_set_format(handle, params.?, c.SND_PCM_FORMAT_FLOAT_LE);
        if (rc < 0) return alsaError("snd_pcm_hw_params_set_format", rc);

        rc = c.snd_pcm_hw_params_set_channels(handle, params.?, @as(c.uint, @intCast(channel_count)));
        if (rc < 0) return alsaError("snd_pcm_hw_params_set_channels", rc);

        rc = c.snd_pcm_hw_params_set_rate_resample(handle, params.?, 1);
        if (rc < 0) return alsaError("snd_pcm_hw_params_set_rate_resample", rc);

        var sr = @as(c.uint, @intCast(sample_rate));
        rc = c.snd_pcm_hw_params_set_rate_near(handle, params.?, &sr, null);
        if (rc < 0) return alsaError("snd_pcm_hw_params_set_rate_near", rc);

        rc = c.snd_pcm_hw_params_set_buffer_size_near(handle, params.?, buffer_size);
        if (rc < 0) return alsaError("snd_pcm_hw_params_set_buffer_size_near", rc);

        rc = c.snd_pcm_hw_params_set_period_size_near(handle, params.?, period_size, null);
        if (rc < 0) return alsaError("snd_pcm_hw_params_set_period_size_near", rc);

        rc = c.snd_pcm_hw_params(handle, params.?);
        if (rc < 0) return alsaError("snd_pcm_hw_params", rc);
    }

    fn loop(self: *Context) void {
        const buf = self.buf32 orelse return;
        while (self.readAndWrite(buf)) {}
    }

    fn readAndWrite(self: *Context, buf32: []f32) bool {
        self.mutex.lock();

        while ((self.suspended or self.shutdown) and self.err == null) {
            if (self.shutdown) {
                self.mutex.unlock();
                return false;
            }
            self.condition.wait(&self.mutex);
        }

        if (self.shutdown) {
            self.mutex.unlock();
            return false;
        }

        if (self.err != null) {
            self.mutex.unlock();
            return false;
        }

        self.mux.readFloat32s(buf32) catch |mux_err| {
            if (self.err == null) {
                self.err = mux_err;
                self.condition.signal();
            }
            self.mutex.unlock();
            return false;
        };

        const handle = self.handle orelse {
            if (self.err == null) {
                self.err = error.DeviceOpenFailed;
            }
            self.mutex.unlock();
            return false;
        };

        const channel_count_usize = @as(usize, @intCast(self.channel_count));
        var remaining = buf32;

        while (remaining.len > 0) {
            const frames = remaining.len / channel_count_usize;
            if (frames == 0) break;

            var written = c.snd_pcm_writei(handle, @ptrCast(remaining.ptr), @as(c.snd_pcm_uframes_t, @intCast(frames)));
            if (written < 0) {
                written = c.snd_pcm_recover(handle, @as(c_int, @intCast(written)), 1);
            }
            if (written < 0) {
                if (self.err == null) {
                    self.err = alsaError("snd_pcm_writei or snd_pcm_recover", @as(c_int, @intCast(written)));
                    self.condition.signal();
                }
                self.mutex.unlock();
                return false;
            }

            const frames_written = @as(usize, @intCast(written));
            const advance = frames_written * channel_count_usize;
            if (advance >= remaining.len) {
                remaining = remaining[remaining.len..];
            } else {
                remaining = remaining[advance..];
            }
        }

        const continue_running = !self.shutdown and self.err == null;
        self.mutex.unlock();
        return continue_running;
    }

    fn setError(self: *Context, err: anyerror) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        if (self.err == null) {
            self.err = err;
            self.condition.signal();
        }
    }

    fn logOpenErrors(self: *Context, errors: []OpenError) void {
        if (errors.len == 0) {
            std.log.err("ALSA: no playback devices available", .{});
            return;
        }

        var buf = std.ArrayList(u8).empty;
        defer buf.deinit(self.allocator);

        var writer = buf.writer(self.allocator);
        for (errors, 0..) |entry, idx| {
            if (idx != 0) {
                writer.writeAll(", ") catch break;
            }

            const msg_ptr = c.snd_strerror(entry.code);
            const msg_slice = if (msg_ptr != null)
                std.mem.sliceTo(msg_ptr, 0)
            else
                "unknown";

            writer.print("\"{s}\": {s}", .{ entry.name, msg_slice }) catch break;
        }

        if (buf.items.len == 0) {
            std.log.err("Failed to open any ALSA playback devices", .{});
            return;
        }

        const text = buf.toOwnedSlice(self.allocator) catch {
            std.log.err("Failed to open ALSA playback devices", .{});
            return;
        };
        defer self.allocator.free(text);

        std.log.err("Failed to open ALSA playback devices: {s}", .{text});
    }
};

fn audioContextWorker(ctx: *Context, sample_rate: u32, channel_count: u32, buffer_size_in_bytes: u32) void {
    ctx.setup(sample_rate, channel_count, buffer_size_in_bytes) catch |err| {
        ctx.setError(err);
        ctx.mutex.lock();
        ctx.ready = true;
        ctx.condition.signal();
        ctx.mutex.unlock();
        return;
    };

    ctx.mutex.lock();
    ctx.ready = true;
    ctx.condition.signal();
    ctx.mutex.unlock();

    ctx.loop();
}

fn allocCString(allocator: std.mem.Allocator, value: []const u8) ![]u8 {
    const buf = try allocator.alloc(u8, value.len + 1);
    @memcpy(buf[0..value.len], value);
    buf[value.len] = 0;
    return buf;
}

fn cPtr(buf: []u8) [*:0]const u8 {
    return @ptrCast(buf.ptr);
}

fn alsaError(name: []const u8, code: c_int) DriverError {
    const msg_ptr = c.snd_strerror(code);
    const msg = if (msg_ptr != null)
        std.mem.sliceTo(msg_ptr, 0)
    else
        "unknown";
    std.log.err("ALSA error at {s}: {s}", .{ name, msg });
    return error.AlsaFailure;
}

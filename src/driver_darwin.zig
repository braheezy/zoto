const objc = @import("objc");
const std = @import("std");
const mux = @import("mux.zig");
const Mux = mux.Mux;
const Format = mux.Format;
const Player = mux.Player;

const av_audio_session_error_code_cannot_start_playing = 0x21706c61; // '!pla'
const av_audio_session_error_code_cannot_interrupt_others = 0x21696e74; // '!int'
const av_audio_session_error_code_siri_is_recording = 0x73697269; // 'siri'
const audio_format_linear_pcm = 0x6C70636D; // 'lpcm'
const audio_format_flag_is_float = 1 << 0; // 0x1
const float32_size_in_bytes = 4;
const buffer_count = 4;
const no_err = 0;

pub const AudioStreamBasicDescription = extern struct {
    sample_rate: f64,
    format_id: u32,
    format_flags: u32,
    bytes_per_packet: u32,
    frames_per_packet: u32,
    bytes_per_frame: u32,
    channels_per_frame: u32,
    bits_per_channel: u32,
    reserved: u32 = 0,
};

pub const AudioQueueRef = usize;
pub const AudioQueueBufferRef = *AudioQueueBuffer;
pub const AudioTimeStamp = usize;

pub const AudioStreamPacketDescription = extern struct {
    start_offset: i64,
    variable_frames_in_packet: u32,
    data_byte_size: u32,
};

pub const AudioQueueBuffer = extern struct {
    audio_data_bytes_capacity: u32,
    audio_data: usize, // void*
    audio_data_byte_size: u32,
    user_data: usize, // void*
    packet_description_capacity: u32,
    packet_descriptions: ?*AudioStreamPacketDescription,
    packet_description_count: u32,
};

// Callback type for AudioQueue
pub const AudioQueueOutputCallback = fn (
    user_data: ?*anyopaque,
    aq: AudioQueueRef,
    buffer: AudioQueueBufferRef,
) callconv(.c) void;

// Function signatures for AudioQueue APIs (to be called via objc or Zig FFI)
// These are C functions, but you can call them via Zig's extern or via objc if needed
extern "c" fn AudioQueueNewOutput(
    format: *const AudioStreamBasicDescription,
    callback_proc: ?*const anyopaque, // Actually a function pointer
    user_data: ?*anyopaque,
    callback_run_loop: usize,
    callback_run_loop_mode: usize,
    flags: u32,
    aq: *AudioQueueRef,
) i32;

extern "c" fn AudioQueueAllocateBuffer(
    aq: AudioQueueRef,
    buffer_byte_size: u32,
    buffer: *AudioQueueBufferRef,
) i32;

extern "c" fn AudioQueueEnqueueBuffer(
    aq: AudioQueueRef,
    buffer: AudioQueueBufferRef,
    num_packet_descs: u32,
    packet_descs: ?*AudioStreamPacketDescription,
) i32;

extern "c" fn AudioQueueStart(
    aq: AudioQueueRef,
    start_time: ?*AudioTimeStamp,
) i32;

extern "c" fn AudioQueuePause(
    aq: AudioQueueRef,
) i32;

extern "c" fn AudioQueueStop(
    aq: AudioQueueRef,
    immediate: bool,
) i32;

extern "c" fn AudioQueueDispose(
    aq: AudioQueueRef,
    immediate: bool,
) i32;

fn disposeAudioQueue(queue: AudioQueueRef) void {
    const status = AudioQueueDispose(queue, true);
    if (status != no_err) {
        std.debug.panic("AudioQueueDispose failed: {d}", .{status});
    }
}

fn newAudioQueue(ctx: *Context, allocator: std.mem.Allocator, sample_rate: u32, channel_count: u32, one_buffer_size_in_bytes: u32) !struct { AudioQueueRef, []AudioQueueBufferRef } {
    const description = AudioStreamBasicDescription{
        .sample_rate = @floatFromInt(sample_rate),
        .format_id = audio_format_linear_pcm,
        .format_flags = audio_format_flag_is_float,
        .bytes_per_packet = channel_count * float32_size_in_bytes,
        .frames_per_packet = 1,
        .bytes_per_frame = channel_count * float32_size_in_bytes,
        .channels_per_frame = channel_count,
        .bits_per_channel = 8 * float32_size_in_bytes,
    };

    var audio_queue: AudioQueueRef = undefined;
    const err = AudioQueueNewOutput(
        &description,
        render,
        ctx,
        0,
        0,
        0,
        &audio_queue,
    );
    if (err != no_err) {
        std.log.err("AudioQueueNewOutput failed with error: {}\n", .{err});
        return error.AudioQueueNewOutputFailed;
    }
    errdefer disposeAudioQueue(audio_queue);

    const bufs = try allocator.alloc(AudioQueueBufferRef, buffer_count);
    errdefer allocator.free(bufs);
    var i: usize = 0;
    while (i < buffer_count) : (i += 1) {
        var buf: AudioQueueBufferRef = undefined;
        const osstatus = AudioQueueAllocateBuffer(audio_queue, one_buffer_size_in_bytes, &buf);
        if (osstatus != no_err) {
            std.log.err("AudioQueueAllocateBuffer failed for buffer {} with error: {}\n", .{ i, osstatus });
            return error.AudioQueueAllocateBufferFailed;
        }
        buf.audio_data_byte_size = @intCast(one_buffer_size_in_bytes);
        bufs[i] = buf;
    }

    return .{ audio_queue, bufs };
}

pub const Context = struct {
    audio_queue: ?AudioQueueRef = null,
    unqueued_buffers: std.array_list.Managed(AudioQueueBufferRef),
    allocated_buffers: ?[]AudioQueueBufferRef = null,
    buf32: ?[]f32 = null,
    one_buffer_size_in_bytes: u32,
    mutex: std.Io.Mutex,
    condition: std.Io.Condition,
    to_pause: bool,
    to_resume: bool,
    mux: *Mux,
    // initialization has finished, including failure handling
    ready: bool,
    allocator: std.mem.Allocator,
    err: ?anyerror = null,
    worker: ?std.Thread = null,
    stopping: std.atomic.Value(bool) = .init(false),

    pub fn init(allocator: std.mem.Allocator, sample_rate: u32, channel_count: u32, format: Format, buffer_size_in_bytes: u32) !*Context {
        // defaultOneBufferSizeInBytes is the default buffer size in bytes.
        //
        // 12288 seems necessary at least on iPod touch (7th) and MacBook Pro 2020.
        // With 48000[Hz] stereo, the maximum delay is (12288*4[buffers] / 4 / 2)[samples] / 48000 [Hz] = 100[ms].
        // '4' is float32 size in bytes. '2' is a number of channels for stereo
        const default_one_buffer_size_in_bytes = 12288;

        var one_buffer_size_in_bytes: u32 = 0;
        if (buffer_size_in_bytes != 0) {
            one_buffer_size_in_bytes = buffer_size_in_bytes / buffer_count;
        } else {
            one_buffer_size_in_bytes = default_one_buffer_size_in_bytes;
        }
        const bytes_per_sample = channel_count * float32_size_in_bytes;
        one_buffer_size_in_bytes = one_buffer_size_in_bytes / bytes_per_sample * bytes_per_sample;

        const c = try allocator.create(Context);
        errdefer allocator.destroy(c);
        c.* = Context{
            .audio_queue = null,
            .unqueued_buffers = std.array_list.Managed(AudioQueueBufferRef).init(allocator),
            .mutex = .init,
            .condition = .init,
            .to_pause = false,
            .to_resume = false,
            .one_buffer_size_in_bytes = one_buffer_size_in_bytes,
            .mux = try Mux.init(
                allocator,
                sample_rate,
                @intCast(channel_count),
                format,
            ),
            .ready = false,
            .allocator = allocator,
        };
        errdefer c.mux.deinit();
        errdefer c.unqueued_buffers.deinit();

        // Spawn the audio worker thread
        c.worker = try std.Thread.spawn(
            .{},
            audioContextWorker,
            .{ c, sample_rate, channel_count },
        );

        return c;
    }

    pub fn deinit(self: *Context) void {
        self.stopping.store(true, .release);

        self.mutex.lockUncancelable(std.Options.debug_io);
        self.condition.broadcast(std.Options.debug_io);
        self.mutex.unlock(std.Options.debug_io);

        if (self.worker) |worker| {
            worker.join();
            self.worker = null;
        }

        // Stop the audio queue immediately to prevent new callbacks from being queued
        if (self.audio_queue) |queue| {
            disposeAudioQueue(queue);
            self.audio_queue = null;
        }

        // Now it's safe to clean up other resources
        self.mux.deinit();
        self.unqueued_buffers.deinit();
        if (self.allocated_buffers) |buffers| {
            self.allocator.free(buffers);
        }
        if (self.buf32) |buf| {
            self.allocator.free(buf);
        }
        self.allocator.destroy(self);
    }

    pub fn waitForReady(self: *Context) void {
        self.mutex.lockUncancelable(std.Options.debug_io);
        defer self.mutex.unlock(std.Options.debug_io);

        while (!self.ready) {
            self.condition.waitUncancelable(std.Options.debug_io, &self.mutex);
        }
    }

    pub fn pause(self: *Context) !void {
        self.mutex.lockUncancelable(std.Options.debug_io);
        defer self.mutex.unlock(std.Options.debug_io);

        if (self.err) |err| return err;

        self.to_pause = true;
        self.to_resume = false;
        self.condition.signal(std.Options.debug_io);
    }

    pub fn play(self: *Context) !void {
        self.mutex.lockUncancelable(std.Options.debug_io);
        defer self.mutex.unlock(std.Options.debug_io);

        if (self.err) |err| return err;

        self.to_pause = false;
        self.to_resume = true;
        self.condition.signal(std.Options.debug_io);
    }

    pub fn getErr(self: *Context) ?anyerror {
        self.mutex.lockUncancelable(std.Options.debug_io);
        defer self.mutex.unlock(std.Options.debug_io);
        return self.err;
    }

    pub fn newPlayer(self: *Context, reader: *std.Io.Reader) !*Player {
        return try self.mux.newPlayer(reader);
    }

    fn wait(self: *Context) bool {
        self.mutex.lockUncancelable(std.Options.debug_io);
        defer self.mutex.unlock(std.Options.debug_io);

        while (!self.stopping.load(.acquire) and
            self.unqueued_buffers.items.len == 0 and
            self.err == null and
            !self.to_pause and
            !self.to_resume)
        {
            self.condition.waitUncancelable(std.Options.debug_io, &self.mutex);
        }
        return !self.stopping.load(.acquire) and self.err == null;
    }

    fn loop(self: *Context) void {
        const buf32 = self.buf32.?;

        while (true) {
            if (!self.wait()) {
                return;
            }
            self.appendBuffer(buf32);
        }
    }

    fn appendBuffer(self: *Context, buf32: []f32) void {
        self.mutex.lockUncancelable(std.Options.debug_io);
        defer self.mutex.unlock(std.Options.debug_io);

        if (self.stopping.load(.acquire)) return;

        if (self.err != null) {
            return;
        }

        if (self.to_pause) {
            self.pauseImpl() catch |pause_err| {
                if (self.err == null) self.err = pause_err;
            };
            self.to_pause = false;
            return;
        }

        if (self.to_resume) {
            self.resumeImpl() catch |resume_err| {
                if (self.err == null) self.err = resume_err;
            };
            self.to_resume = false;
            return;
        }

        if (self.unqueued_buffers.items.len == 0) {
            return;
        }

        const buf = self.unqueued_buffers.items[0];
        // Remove first element by copying the rest forward
        std.mem.copyForwards(AudioQueueBufferRef, self.unqueued_buffers.items[0 .. self.unqueued_buffers.items.len - 1], self.unqueued_buffers.items[1..]);
        _ = self.unqueued_buffers.pop();

        // Read audio data from mux
        self.mux.readFloat32s(buf32) catch |read_err| {
            if (self.err == null) self.err = read_err;
            return;
        };

        const audio_data_ptr: [*]f32 = @ptrFromInt(buf.audio_data);
        const audio_data_slice = audio_data_ptr[0 .. buf.audio_data_byte_size / float32_size_in_bytes];

        @memcpy(audio_data_slice, buf32[0..@min(buf32.len, audio_data_slice.len)]);

        const osstatus = AudioQueueEnqueueBuffer(self.audio_queue.?, buf, 0, null);
        if (osstatus != no_err) {
            if (self.err == null) self.err = error.AudioQueueEnqueueBufferFailed;
        }
    }

    fn pauseImpl(self: *Context) !void {
        const osstatus = AudioQueuePause(self.audio_queue.?);
        if (osstatus != no_err) {
            return error.AudioQueuePauseFailed;
        }
    }

    fn resumeImpl(self: *Context) !void {
        var retry_count: i32 = 0;
        while (true) {
            if (self.stopping.load(.acquire)) return;

            const osstatus = AudioQueueStart(self.audio_queue.?, null);
            if (osstatus == no_err) {
                break;
            }

            if ((osstatus == av_audio_session_error_code_cannot_start_playing or
                osstatus == av_audio_session_error_code_cannot_interrupt_others) and
                retry_count < 30)
            {
                // Use exponential backoff for temporary errors
                std.Io.sleep(std.Options.debug_io, .fromNanoseconds(sleepTime(retry_count)), .awake) catch {};
                retry_count += 1;
                continue;
            }

            if (osstatus == av_audio_session_error_code_siri_is_recording) {
                // Siri recording error should be temporary
                std.Io.sleep(std.Options.debug_io, .fromNanoseconds(10 * std.time.ns_per_ms), .awake) catch {};
                continue;
            }

            return error.AudioQueueStartFailed;
        }
    }

    fn suspendPlay(self: *Context) void {
        self.mutex.lockUncancelable(std.Options.debug_io);
        defer self.mutex.unlock(std.Options.debug_io);

        self.to_pause = true;
        self.to_resume = false;
        self.condition.signal(std.Options.debug_io);
    }

    fn resumePlay(self: *Context) void {
        self.mutex.lockUncancelable(std.Options.debug_io);
        defer self.mutex.unlock(std.Options.debug_io);

        self.to_pause = false;
        self.to_resume = true;
        self.condition.signal(std.Options.debug_io);
    }
};

fn audioContextWorker(ctx: *Context, sample_rate: u32, channel_count: u32) void {
    var ready_closed = false;
    var initialization_error: anyerror = error.ContextDestroyed;
    defer {
        ctx.mutex.lockUncancelable(std.Options.debug_io);
        defer ctx.mutex.unlock(std.Options.debug_io);

        if (!ready_closed) {
            if (ctx.err == null) ctx.err = initialization_error;
            ctx.ready = true;
            ctx.condition.broadcast(std.Options.debug_io);
        }
    }

    const q, const bs = newAudioQueue(
        ctx,
        ctx.allocator,
        sample_rate,
        channel_count,
        ctx.one_buffer_size_in_bytes,
    ) catch |err| {
        initialization_error = err;
        // Store error in context
        std.log.err("newAudioQueue failed: {any}", .{err});
        return;
    };

    ctx.audio_queue = q;
    ctx.allocated_buffers = bs;
    ctx.unqueued_buffers.clearAndFree();
    ctx.unqueued_buffers.appendSlice(bs) catch |err| {
        initialization_error = err;
        std.log.err("Failed to append buffers in audioContextWorker: {any}", .{err});
        return;
    };

    // Allocate the buffer once and store it in the context
    ctx.buf32 = ctx.allocator.alloc(
        f32,
        ctx.one_buffer_size_in_bytes / float32_size_in_bytes,
    ) catch |err| {
        initialization_error = err;
        return;
    };

    var retry_count: i32 = 0;
    while (true) {
        if (ctx.stopping.load(.acquire)) return;

        const osstatus = AudioQueueStart(q, null);
        if (osstatus == no_err) {
            break;
        }

        if (osstatus == av_audio_session_error_code_cannot_start_playing and retry_count < 100) {
            std.Io.sleep(std.Options.debug_io, .fromNanoseconds(10 * std.time.ns_per_ms), .awake) catch {};
            retry_count += 1;
            continue;
        }

        initialization_error = error.AudioQueueStartFailed;
        std.log.err("AudioQueueStart failed at newContext: {d}", .{osstatus});
        return;
    }

    ctx.mutex.lockUncancelable(std.Options.debug_io);
    ctx.ready = true;
    ctx.condition.broadcast(std.Options.debug_io);
    ctx.mutex.unlock(std.Options.debug_io);
    ready_closed = true;

    // Start the main audio processing loop
    ctx.loop();
}

fn render(user_data: ?*anyopaque, aq: AudioQueueRef, buffer: AudioQueueBufferRef) callconv(.c) void {
    const ctx: *Context = @ptrCast(@alignCast(user_data orelse return));
    _ = aq;

    ctx.mutex.lockUncancelable(std.Options.debug_io);
    defer ctx.mutex.unlock(std.Options.debug_io);

    // Add the finished buffer back to the pool of available buffers
    ctx.unqueued_buffers.append(buffer) catch |err| {
        std.log.err("Failed to append buffer in render callback: {}", .{err});
        return;
    };

    // Signal that a buffer is available
    ctx.condition.signal(std.Options.debug_io);
}

fn sleepTime(count: i32) u64 {
    return switch (count) {
        0 => 10 * std.time.ns_per_ms,
        1 => 20 * std.time.ns_per_ms,
        2 => 50 * std.time.ns_per_ms,
        else => 100 * std.time.ns_per_ms,
    };
}

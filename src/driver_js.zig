// WASM/JavaScript driver for Web Audio API
//
// This driver requires a JavaScript interop layer that provides the following functions:
// - js_global_get: Get a global JavaScript object
// - js_value_new: Create a new JavaScript object
// - js_value_get/set: Get/set properties on JavaScript objects
// - js_value_call/call_func: Call methods/functions on JavaScript objects
// - js_value_truthy/is_null: Check JavaScript value properties
// - js_value_release: Release a JavaScript value reference
// - js_string_new: Create a JavaScript string
// - js_float32_array_new: Create a Float32Array from Zig memory
// - js_blob_new: Create a Blob
// - js_url_create_object_url: Create an object URL
// - js_add_event_listener/remove_event_listener: Manage event listeners
// - js_promise_then: Handle promise callbacks
// - js_create_callback: Create a JavaScript function that calls back into WASM
//
// The exported functions (zoto_*) are called from JavaScript and handle audio processing.

const std = @import("std");
const builtin = @import("builtin");
const mux = @import("mux.zig");
const Mux = mux.Mux;
const Player = mux.Player;
const Format = mux.Format;

// JavaScript interop types
// In WASM, JS objects are represented as opaque handles
const JSValue = u32;

// JavaScript function imports
extern "env" fn js_global_get(prop: [*:0]const u8) JSValue;
extern "env" fn js_value_new(class: JSValue, args: [*]const JSValue, args_len: usize) JSValue;
extern "env" fn js_value_get(obj: JSValue, prop: [*:0]const u8) JSValue;
extern "env" fn js_value_set(obj: JSValue, prop: [*:0]const u8, value: JSValue) void;
extern "env" fn js_value_call(obj: JSValue, method: [*:0]const u8, args: [*]const JSValue, args_len: usize) JSValue;
extern "env" fn js_value_truthy(obj: JSValue) bool;
extern "env" fn js_value_is_null(obj: JSValue) bool;
extern "env" fn js_value_release(obj: JSValue) void;
extern "env" fn js_string_new(str: [*:0]const u8) JSValue;
extern "env" fn js_number_new(value: f64) JSValue;
extern "env" fn js_float32_array_new(buffer: [*]const f32, len: usize) JSValue;
extern "env" fn js_blob_new(parts: [*]const JSValue, parts_len: usize, mime_type: [*:0]const u8) JSValue;
extern "env" fn js_url_create_object_url(blob: JSValue) JSValue;
extern "env" fn js_add_event_listener(target: JSValue, event: [*:0]const u8, callback: JSValue) void;
extern "env" fn js_remove_event_listener(target: JSValue, event: [*:0]const u8, callback: JSValue) void;
extern "env" fn js_promise_then(promise: JSValue, on_fulfilled: JSValue) void;
extern "env" fn js_create_callback(ctx_ptr: usize, func_name: [*:0]const u8) JSValue;
extern "env" fn js_create_script_processor_callback(ctx_ptr: usize, func_name: [*:0]const u8) JSValue;

// Export callback functions that can be called from JavaScript
// These will be wrapped by JavaScript code
// Called when the worklet processor requests more data (sends null message)
export fn zoto_worklet_onmessage(ctx_ptr: usize) void {
    const ctx: *Context = @ptrFromInt(ctx_ptr);
    if (!builtin.single_threaded) {
        ctx.mutex.lock();
        defer ctx.mutex.unlock();
    }

    if (ctx.buf32) |buf| {
        ctx.mux.readFloat32s(buf) catch |err| {
            if (ctx.err == null) {
                ctx.err = err;
            }
            return;
        };

        if (ctx.worklet_port != 0) {
            const typed_array = js_float32_array_new(buf.ptr, buf.len);
            const buffer_value = js_value_get(typed_array, "buffer");
            const transfer_list = jsNewArray();
            const push_result = js_value_call(transfer_list, "push", &[_]JSValue{buffer_value}, 1);
            if (push_result != 0) {
                js_value_release(push_result);
            }
            const args = [_]JSValue{ typed_array, transfer_list };
            const post_result = js_value_call(ctx.worklet_port, "postMessage", &args, args.len);
            if (post_result != 0) {
                js_value_release(post_result);
            }
            js_value_release(transfer_list);
            js_value_release(buffer_value);
            js_value_release(typed_array);
        }
    }
}

export fn zoto_script_processor_callback(ctx_ptr: usize, output_buffer: JSValue) void {
    const ctx: *Context = @ptrFromInt(ctx_ptr);
    if (!builtin.single_threaded) {
        ctx.mutex.lock();
        defer ctx.mutex.unlock();
    }

    if (ctx.buf32) |buf| {
        ctx.mux.readFloat32s(buf) catch |err| {
            if (ctx.err == null) {
                ctx.err = err;
            }
            return;
        };

        const channel_count = ctx.channel_count;
        const samples_per_channel = buf.len / channel_count;

        // Deinterleave channels
        if (ctx.ch_buf32) |ch_buf| {
            for (0..channel_count) |ch| {
                for (0..samples_per_channel) |i| {
                    ch_buf[ch][i] = buf[i * channel_count + ch];
                }
            }

            // Copy to output buffer
            for (0..channel_count) |ch| {
                const ch_num = jsNumberFromU32(@intCast(ch));
                const channel_data = js_value_call(output_buffer, "getChannelData", &[_]JSValue{ch_num}, 1);
                js_value_release(ch_num);
                const typed_array = js_float32_array_new(ch_buf[ch].ptr, ch_buf[ch].len);

                const copy_to_channel = js_value_get(output_buffer, "copyToChannel");
                const has_copy_to_channel = js_value_truthy(copy_to_channel);
                js_value_release(copy_to_channel);

                if (has_copy_to_channel) {
                    const ch_idx = jsNumberFromU32(@intCast(ch));
                    const offset = jsNumberFromU32(0);
                    const res = js_value_call(output_buffer, "copyToChannel", &[_]JSValue{ typed_array, ch_idx, offset }, 3);
                    if (res != 0) {
                        js_value_release(res);
                    }
                    js_value_release(ch_idx);
                    js_value_release(offset);
                } else {
                    const res = js_value_call(channel_data, "set", &[_]JSValue{typed_array}, 1);
                    if (res != 0) {
                        js_value_release(res);
                    }
                }

                js_value_release(channel_data);
                js_value_release(typed_array);
            }
        }
    }
}

export fn zoto_setup_after_user_interaction(ctx_ptr: usize) void {
    const ctx: *Context = @ptrFromInt(ctx_ptr);
    if (!builtin.single_threaded) {
        ctx.mutex.lock();
        defer ctx.mutex.unlock();
    }

    // Get AudioContext class (first JavaScript call, but after user interaction)
    if (ctx.audio_context_class == 0) {
        const audio_context_class = js_global_get("AudioContext");
        if (js_value_is_null(audio_context_class) or !js_value_truthy(audio_context_class)) {
            const webkit_audio_context_class = js_global_get("webkitAudioContext");
            if (js_value_is_null(webkit_audio_context_class) or !js_value_truthy(webkit_audio_context_class)) {
                js_value_release(audio_context_class);
                ctx.err = error.AudioContextNotFound;
                return;
            }
            js_value_release(audio_context_class);
            ctx.audio_context_class = webkit_audio_context_class;
        } else {
            ctx.audio_context_class = audio_context_class;
        }
    }

    // Set up event handlers now (after user interaction, won't block)
    if (ctx.on_event_fired == 0) {
        const document = js_global_get("document");
        const events = [_][*:0]const u8{ "touchend", "keyup", "mouseup" };

        ctx.on_event_fired = js_create_callback(@intFromPtr(ctx), "zoto_on_event_fired");
        ctx.on_resume_success = js_create_callback(@intFromPtr(ctx), "zoto_on_resume_success");

        for (events) |event| {
            js_add_event_listener(document, event, ctx.on_event_fired);
        }

        js_value_release(document);
    }

    // Create AudioContext now (after user interaction) if not already created
    if (ctx.audio_context == 0 and ctx.audio_context_class != 0) {
        const options = jsNewObject();
        const sample_rate_value = jsNumberFromU32(ctx.deferred_sample_rate);
        js_value_set(options, "sampleRate", sample_rate_value);
        js_value_release(sample_rate_value);
        ctx.audio_context = js_value_new(ctx.audio_context_class, &[_]JSValue{options}, 1);
        js_value_release(options);
    }

    if (!ctx.ready and ctx.audio_context != 0) {
        const resume_promise = js_value_call(ctx.audio_context, "resume", &[_]JSValue{}, 0);
        _ = js_promise_then(resume_promise, ctx.on_resume_success);
        js_value_release(resume_promise);
    }
}

export fn zoto_on_event_fired(ctx_ptr: usize) void {
    const ctx: *Context = @ptrFromInt(ctx_ptr);
    if (!builtin.single_threaded) {
        ctx.mutex.lock();
        defer ctx.mutex.unlock();
    }

    if (!ctx.ready and ctx.audio_context != 0) {
        const resume_promise = js_value_call(ctx.audio_context, "resume", &[_]JSValue{}, 0);
        _ = js_promise_then(resume_promise, ctx.on_resume_success);
        js_value_release(resume_promise);
    }
}

export fn zoto_on_resume_success(ctx_ptr: usize) void {
    const ctx: *Context = @ptrFromInt(ctx_ptr);
    if (!builtin.single_threaded) {
        ctx.mutex.lock();
        defer ctx.mutex.unlock();
    }

    // Set up audio worklet/script processor now (after user interaction, won't block)
    if (!ctx.audio_setup_done and ctx.deferred_buffer_size > 0) {
        const audio_worklet = js_value_get(ctx.audio_context, "audioWorklet");
        if (js_value_truthy(audio_worklet)) {
            ctx.setupAudioWorklet(ctx.allocator, ctx.deferred_buffer_size, ctx.deferred_channel_count) catch |err| {
                ctx.err = err;
            };
        } else {
            ctx.setupScriptProcessorNode(ctx.allocator, ctx.deferred_buffer_size, ctx.deferred_channel_count) catch |err| {
                ctx.err = err;
            };
        }
        js_value_release(audio_worklet);
        ctx.audio_setup_done = true;
    }

    ctx.ready = true;
    if (!builtin.single_threaded) {
        ctx.condition.signal();
    }

    // Remove event listeners
    const document = js_global_get("document");
    const events = [_][*:0]const u8{ "touchend", "keyup", "mouseup" };
    for (events) |event| {
        js_remove_event_listener(document, event, ctx.on_event_fired);
    }
    js_value_release(document);

    if (ctx.on_event_fired != 0) {
        js_value_release(ctx.on_event_fired);
        ctx.on_event_fired = 0;
    }
    if (ctx.on_resume_success != 0) {
        js_value_release(ctx.on_resume_success);
        ctx.on_resume_success = 0;
    }
}

const float32_size_in_bytes: usize = @sizeOf(f32);

pub const Context = struct {
    allocator: std.mem.Allocator,
    channel_count: u32,
    mux: *Mux,
    audio_context: JSValue = 0,
    audio_context_class: JSValue = 0, // Store class reference for deferred creation
    script_processor: JSValue = 0,
    worklet_node: JSValue = 0,
    worklet_port: JSValue = 0,
    on_event_fired: JSValue = 0,
    on_resume_success: JSValue = 0,
    mutex: if (builtin.single_threaded) struct {} else std.Thread.Mutex = if (builtin.single_threaded) .{} else .{},
    condition: if (builtin.single_threaded) struct {} else std.Thread.Condition = if (builtin.single_threaded) .{} else .{},
    ready: bool = false,
    err: ?anyerror = null,
    buf32: ?[]f32 = null,
    ch_buf32: ?[]([]f32) = null, // Per-channel buffers for ScriptProcessorNode
    deferred_buffer_size: u32 = 0,
    deferred_channel_count: u32 = 0,
    deferred_sample_rate: u32 = 0,
    audio_setup_done: bool = false,

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
        };

        // Calculate buffer size
        var actual_buffer_size: u32 = buffer_size_in_bytes;
        if (actual_buffer_size == 0) {
            // 4096 was not great at least on Safari 15.
            actual_buffer_size = 8192 * channel_count;
        }

        const buf32_size = actual_buffer_size / 4;
        ctx.buf32 = try allocator.alloc(f32, buf32_size);
        errdefer allocator.free(ctx.buf32.?);

        // Initialize per-channel buffers for ScriptProcessorNode
        ctx.ch_buf32 = try allocator.alloc([]f32, channel_count);
        errdefer allocator.free(ctx.ch_buf32.?);
        for (0..channel_count) |i| {
            ctx.ch_buf32.?[i] = try allocator.alloc(f32, buf32_size / channel_count);
        }
        errdefer {
            if (ctx.ch_buf32) |ch_buf| {
                for (ch_buf) |buf| {
                    allocator.free(buf);
                }
            }
        }

        // NO JAVASCRIPT CALLS HERE - defer everything to avoid blocking
        // Store setup parameters for deferred setup
        ctx.deferred_buffer_size = actual_buffer_size;
        ctx.deferred_channel_count = channel_count;
        ctx.deferred_sample_rate = sample_rate;

        return ctx;
    }

    fn setupAudioWorklet(self: *Context, allocator: std.mem.Allocator, buffer_size_in_bytes: u32, channel_count: u32) !void {
        const buffer_size = buffer_size_in_bytes / 4 / channel_count;

        // Create worklet processor script
        const script_content = try std.fmt.allocPrint(
            allocator,
            \\class OtoWorkletProcessor extends AudioWorkletProcessor {{
            \\    constructor() {{
            \\        super();
            \\        this.bufferSize_ = {d};
            \\        this.channelCount_ = {d};
            \\        this.buf_ = new Float32Array();
            \\        this.waitRecv_ = false;
            \\
            \\        this.port.onmessage = (event) => {{
            \\            const buf = event.data;
            \\            const newBuf = new Float32Array(this.buf_.length + buf.length);
            \\            newBuf.set(this.buf_);
            \\            newBuf.set(buf, this.buf_.length);
            \\            this.buf_ = newBuf;
            \\            this.waitRecv_ = false;
            \\        }}
            \\    }}
            \\
            \\    process(inputs, outputs, parameters) {{
            \\        const output = outputs[0];
            \\
            \\        if (this.buf_.length < output[0].length*this.channelCount_) {{
            \\            if (!this.waitRecv_) {{
            \\                this.waitRecv_ = true;
            \\                this.port.postMessage(null);
            \\            }}
            \\            for (let i = 0; i < output.length; i++) {{
            \\                output[i].fill(0);
            \\            }}
            \\            return true;
            \\        }}
            \\
            \\        if (this.buf_.length < this.bufferSize_*this.channelCount_ / 2 && !this.waitRecv_) {{
            \\            this.waitRecv_ = true;
            \\            this.port.postMessage(null);
            \\        }}
            \\
            \\        for (let i = 0; i < this.channelCount_; i++) {{
            \\            for (let j = 0; j < output[i].length; j++) {{
            \\                output[i][j] = this.buf_[j*this.channelCount_+i];
            \\            }}
            \\        }}
            \\        this.buf_ = this.buf_.slice(output[0].length*this.channelCount_);
            \\        return true;
            \\    }}
            \\}}
            \\registerProcessor('oto-worklet-processor', OtoWorkletProcessor);
        ,
            .{ buffer_size, channel_count },
        );
        defer allocator.free(script_content);

        // Add null terminator for C string compatibility
        const script_with_null = try allocator.alloc(u8, script_content.len + 1);
        @memcpy(script_with_null[0..script_content.len], script_content);
        script_with_null[script_content.len] = 0;
        defer allocator.free(script_with_null);

        // Use script_with_null which is already null-terminated
        const script_url = newScriptURL(script_with_null);
        const audio_worklet = js_value_get(self.audio_context, "audioWorklet");
        const add_module_promise = js_value_call(audio_worklet, "addModule", &[_]JSValue{script_url}, 1);
        js_value_release(script_url);
        js_value_release(audio_worklet);

        // Create callback for promise resolution
        // JavaScript will create a wrapper function that calls zoto_on_worklet_module_loaded
        const on_fulfilled = js_create_callback(@intFromPtr(self), "zoto_on_worklet_module_loaded");
        _ = js_promise_then(add_module_promise, on_fulfilled);
        js_value_release(add_module_promise);
        js_value_release(on_fulfilled);
    }

    export fn zoto_on_worklet_module_loaded(ctx_ptr: usize) void {
        const ctx: *Context = @ptrFromInt(ctx_ptr);
        if (!builtin.single_threaded) {
            ctx.mutex.lock();
            defer ctx.mutex.unlock();
        }

        const audio_worklet_node_class = js_global_get("AudioWorkletNode");
        defer js_value_release(audio_worklet_node_class);

        const processor_name = "oto-worklet-processor";
        const processor_name_null: [*:0]const u8 = processor_name;
        const processor_name_value = js_string_new(processor_name_null);
        defer js_value_release(processor_name_value);

        const node_options = jsNewObject();
        const output_channels = jsNewArray();
        const ch_value = jsNumberFromU32(ctx.channel_count);
        const push_res = js_value_call(output_channels, "push", &[_]JSValue{ch_value}, 1);
        if (push_res != 0) {
            js_value_release(push_res);
        }
        js_value_release(ch_value);
        js_value_set(node_options, "outputChannelCount", output_channels);
        js_value_release(output_channels);

        const node = js_value_new(audio_worklet_node_class, &[_]JSValue{
            ctx.audio_context,
            processor_name_value,
            node_options,
        }, 3);
        js_value_release(node_options);

        ctx.worklet_node = node;
        ctx.worklet_port = js_value_get(node, "port");

        // Set up message handler
        const on_message = js_create_callback(@intFromPtr(ctx), "zoto_worklet_onmessage");
        js_value_set(ctx.worklet_port, "onmessage", on_message);
        js_value_release(on_message);

        const destination = js_value_get(ctx.audio_context, "destination");
        const connect_res = js_value_call(node, "connect", &[_]JSValue{destination}, 1);
        if (connect_res != 0) {
            js_value_release(connect_res);
        }
        js_value_release(destination);
    }

    fn setupScriptProcessorNode(self: *Context, allocator: std.mem.Allocator, buffer_size_in_bytes: u32, channel_count: u32) !void {
        _ = allocator;
        const buffer_size = buffer_size_in_bytes / 4 / channel_count;

        const buffer_size_value = jsNumberFromU32(buffer_size);
        const zero_value = jsNumberFromU32(0);
        const channel_count_value = jsNumberFromU32(channel_count);
        const script_processor = js_value_call(
            self.audio_context,
            "createScriptProcessor",
            &[_]JSValue{ buffer_size_value, zero_value, channel_count_value },
            3,
        );
        js_value_release(buffer_size_value);
        js_value_release(zero_value);
        js_value_release(channel_count_value);

        self.script_processor = script_processor;

        // Create callback
        const callback = js_create_script_processor_callback(@intFromPtr(self), "zoto_script_processor_callback");
        const event_name = js_string_new("audioprocess");
        const add_listener_res = js_value_call(script_processor, "addEventListener", &[_]JSValue{
            event_name,
            callback,
        }, 2);
        if (add_listener_res != 0) {
            js_value_release(add_listener_res);
        }
        js_value_release(event_name);
        js_value_release(callback);

        const destination = js_value_get(self.audio_context, "destination");
        const connect_res = js_value_call(script_processor, "connect", &[_]JSValue{destination}, 1);
        if (connect_res != 0) {
            js_value_release(connect_res);
        }
        js_value_release(destination);
    }

    fn setupUserInteractionHandlers(self: *Context, allocator: std.mem.Allocator) !void {
        _ = allocator;
        const document = js_global_get("document");
        const events = [_][*:0]const u8{ "touchend", "keyup", "mouseup" };

        self.on_event_fired = js_create_callback(@intFromPtr(self), "zoto_on_event_fired");
        self.on_resume_success = js_create_callback(@intFromPtr(self), "zoto_on_resume_success");

        for (events) |event| {
            js_add_event_listener(document, event, self.on_event_fired);
        }

        js_value_release(document);
    }

    pub fn deinit(self: *Context) void {
        // Release JavaScript objects
        if (self.audio_context != 0) {
            js_value_release(self.audio_context);
        }
        if (self.script_processor != 0) {
            js_value_release(self.script_processor);
        }
        if (self.worklet_node != 0) {
            js_value_release(self.worklet_node);
        }
        if (self.worklet_port != 0) {
            js_value_release(self.worklet_port);
        }
        if (self.on_event_fired != 0) {
            js_value_release(self.on_event_fired);
        }
        if (self.on_resume_success != 0) {
            js_value_release(self.on_resume_success);
        }

        // Free buffers
        if (self.buf32) |buf| {
            self.allocator.free(buf);
        }
        if (self.ch_buf32) |ch_buf| {
            for (ch_buf) |buf| {
                self.allocator.free(buf);
            }
            self.allocator.free(ch_buf);
        }

        self.mux.deinit();
        self.allocator.destroy(self);
    }

    pub fn waitForReady(self: *Context) void {
        // In single-threaded WASM/browser mode, don't block
        // The audio context will be ready after user interaction (handled by JS callbacks)
        // Blocking here would freeze the browser tab
        if (builtin.single_threaded) {
            return; // Don't wait - ready will be set asynchronously via JS callbacks
        }

        self.mutex.lock();
        defer self.mutex.unlock();

        while (!self.ready) {
            self.condition.wait(&self.mutex);
        }
    }

    pub fn pause(self: *Context) !void {
        if (!builtin.single_threaded) {
            self.mutex.lock();
            defer self.mutex.unlock();
        }

        if (self.err) |stored_err| return stored_err;

        if (self.audio_context != 0) {
            const res = js_value_call(self.audio_context, "suspend", &[_]JSValue{}, 0);
            if (res != 0) {
                js_value_release(res);
            }
        }
    }

    pub fn play(self: *Context) !void {
        if (!builtin.single_threaded) {
            self.mutex.lock();
            defer self.mutex.unlock();
        }

        if (self.err) |stored_err| return stored_err;

        if (self.audio_context != 0) {
            const res = js_value_call(self.audio_context, "resume", &[_]JSValue{}, 0);
            if (res != 0) {
                js_value_release(res);
            }
        }
    }

    pub fn getErr(self: *Context) ?anyerror {
        if (!builtin.single_threaded) {
            self.mutex.lock();
            defer self.mutex.unlock();
        }
        return self.err;
    }

    pub fn newPlayer(self: *Context, reader: *std.Io.Reader) !*Player {
        return try self.mux.newPlayer(reader);
    }
};

fn newScriptURL(script: []const u8) JSValue {
    // Ensure script is null-terminated for js_string_new
    // script is already null-terminated from setupAudioWorklet, so we can cast
    const script_null: [*:0]const u8 = @ptrCast(script.ptr);
    var blob_parts = [_]JSValue{js_string_new(script_null)};
    const blob = js_blob_new(&blob_parts, 1, "text/javascript");
    js_value_release(blob_parts[0]);
    const url = js_url_create_object_url(blob);
    js_value_release(blob);
    return url;
}

fn jsNumberFromU32(value: u32) JSValue {
    return js_number_new(@floatFromInt(value));
}

fn jsNewObject() JSValue {
    const object_class = js_global_get("Object");
    defer js_value_release(object_class);
    return js_value_new(object_class, &[_]JSValue{}, 0);
}

fn jsNewArray() JSValue {
    const array_class = js_global_get("Array");
    defer js_value_release(array_class);
    return js_value_new(array_class, &[_]JSValue{}, 0);
}

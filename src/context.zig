const std = @import("std");
const builtin = @import("builtin");
const Format = @import("mux.zig").Format;

var context_created: bool = false;
var context_creation_mutex: std.Thread.Mutex = .{};

pub const Options = struct {
    sample_rate: u32,
    channel_count: u8,
    format: Format,
    buffer_size: i64 = 0,
};

pub const Context = switch (builtin.os.tag) {
    .macos => @import("driver_darwin.zig").Context,
    .linux => @import("driver_unix.zig").Context,
    .windows => @import("driver_windows.zig").Context,
    else => @compileError("Unsupported platform"),
};

pub fn newContext(allocator: std.mem.Allocator, options: Options) !*Context {
    context_creation_mutex.lock();
    defer context_creation_mutex.unlock();

    if (context_created) {
        return error.ContextAlreadyCreated;
    }
    context_created = true;

    var buffer_size_in_bytes: u32 = 0;
    if (options.buffer_size != 0) {
        // the underlying driver always uses 32bit floats
        const bytes_per_sample = options.channel_count * 4;
        const bytes_per_second = options.sample_rate * bytes_per_sample;
        buffer_size_in_bytes = @intCast(@divFloor(options.buffer_size * bytes_per_second, std.time.ns_per_s));
        buffer_size_in_bytes = buffer_size_in_bytes / bytes_per_sample * bytes_per_sample;
    }

    const ctx = try Context.init(
        allocator,
        options.sample_rate,
        options.channel_count,
        options.format,
        buffer_size_in_bytes,
    );

    return ctx;
}

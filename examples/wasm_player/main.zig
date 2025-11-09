// WASM Audio Player Example
// This player receives audio data from JavaScript and plays it using zoto

const std = @import("std");
const zoto = @import("zoto");

// Global allocator for WASM - single-threaded (like oto)
const allocator = std.heap.page_allocator;

// Global context and player
var ctx: ?*zoto.Context = null;
var current_player: ?*zoto.Player = null;
var audio_data: ?[]u8 = null;
var fixed_reader: ?std.Io.Reader = null;

// Deferred initialization parameters
var deferred_sample_rate: u32 = 0;
var deferred_channel_count: u8 = 0;
var deferred_format: zoto.Format = .float32_le;
var context_created: bool = false;

// Note: We'll use std.Io.Reader.fixed() to create a Reader from the audio data buffer

// Export: Initialize the audio context (ABSOLUTELY MINIMAL - just store params)
export fn zoto_init(sample_rate: u32, channel_count: u8, format: u32) i32 {
    if (context_created) {
        return 1; // Already initialized
    }

    const fmt = switch (format) {
        0 => zoto.Format.float32_le,
        1 => zoto.Format.uint8,
        2 => zoto.Format.int16_le,
        else => return -1, // Invalid format
    };

    // JUST STORE PARAMETERS - don't create anything yet
    deferred_sample_rate = sample_rate;
    deferred_channel_count = channel_count;
    deferred_format = fmt;

    return 0;
}

// Export: Create context and get pointer (called after user interaction)
export fn zoto_create_context() usize {
    if (ctx != null) {
        return @intFromPtr(ctx.?);
    }

    if (deferred_sample_rate == 0) {
        return 0; // Not initialized
    }

    // NOW create the context (after user interaction, won't block page load)
    ctx = zoto.newContext(allocator, .{
        .sample_rate = deferred_sample_rate,
        .channel_count = deferred_channel_count,
        .format = deferred_format,
        .buffer_size = 0,
    }) catch return 0;

    context_created = true;
    return @intFromPtr(ctx.?);
}

// Export: Load audio data from JavaScript
// data_ptr: pointer to audio data in WASM memory
// data_len: length of audio data in bytes
export fn zoto_load_audio(data_ptr: [*]u8, data_len: usize) i32 {
    if (ctx == null) {
        return -1; // Context not initialized
    }

    // Free previous audio data if any
    if (audio_data) |old_data| {
        allocator.free(old_data);
    }

    // Copy audio data into WASM memory
    audio_data = allocator.alloc(u8, data_len) catch return -2;
    @memcpy(audio_data.?, data_ptr[0..data_len]);

    return 0;
}

// Export: Play the loaded audio
export fn zoto_play() i32 {
    if (ctx == null) {
        return -1; // Context not initialized
    }

    if (audio_data == null) {
        return -2; // No audio data loaded
    }

    // Close previous player if any
    if (current_player) |player| {
        player.close() catch {};
        player.deinit();
    }

    // Create reader from audio data using std.Io.Reader.fixed
    fixed_reader = std.Io.Reader.fixed(audio_data.?);
    const reader = &fixed_reader.?;

    // Create and start player
    current_player = ctx.?.newPlayer(reader) catch return -3;
    current_player.?.play() catch return -4;

    return 0;
}

// Export: Pause playback
export fn zoto_pause() i32 {
    if (current_player) |player| {
        player.pause();
        return 0;
    }
    return -1; // No player active
}

// Export: Resume playback
export fn zoto_resume() i32 {
    if (current_player) |player| {
        player.play() catch return -1;
        return 0;
    }
    return -1; // No player active
}

// Export: Check if playing
export fn zoto_is_playing() i32 {
    if (current_player) |player| {
        return if (player.isPlaying()) 1 else 0;
    }
    return 0;
}

// Export: Set volume (0.0 to 1.0)
export fn zoto_set_volume(volume: f64) i32 {
    if (current_player) |player| {
        player.setVolume(volume);
        return 0;
    }
    return -1; // No player active
}

// Export: Get volume
export fn zoto_get_volume() f64 {
    if (current_player) |player| {
        return player.getVolume();
    }
    return 0.0;
}

// Export: Stop and cleanup
export fn zoto_stop() i32 {
    if (current_player) |player| {
        player.close() catch {};
        player.deinit();
        current_player = null;
    }
    fixed_reader = null;
    return 0;
}

// Export: Cleanup everything
export fn zoto_cleanup() void {
    if (current_player) |player| {
        player.close() catch {};
        player.deinit();
        current_player = null;
    }

    if (audio_data) |data| {
        allocator.free(data);
        audio_data = null;
    }

    if (ctx) |context| {
        context.deinit();
        ctx = null;
    }
}

// Export: Simple malloc for WASM (required by HTML)
export fn malloc(size: usize) ?[*]u8 {
    const mem = allocator.alloc(u8, size) catch return null;
    return mem.ptr;
}

// Export: Simple free for WASM (required by HTML)
export fn free(ptr: [*]u8) void {
    // Note: This is a simplified free - in a real implementation,
    // you'd need to track allocations properly
    _ = ptr;
    // For now, we rely on the allocator to handle cleanup
}

// Note: _start is provided by the standard library for WASM

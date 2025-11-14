# `zoto`

Low-level Zig library to play audio on different platforms. Inspired by [`oto`](https://github.com/ebitengine/oto).

Supports these platforms:

- macOS
- Linux/BSDs
- Windows
- WASM

## Prerequisites

These are usually available by default.

### Linux/BSDs

ALSA is used.

```bash
# Debian-based
apt install libasound2-dev
# Fedora-based
dnf install alsa-lib-dev
```

## Usage

Zoto provides audio playback through two main components: `Context` and `Player`. There can be only **one** context per application, which manages interactions with the OS audio drivers.

### Context and Player

A `Context` is initialized once with audio configuration (sample rate, channel count, format). From the context, you can create multiple `Player` instances, each reading audio data from a `std.Io.Reader`.

### Basic Setup

```zig
const std = @import("std");
const zoto = @import("zoto");

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    // Create a context (only one per application)
    const ctx = try zoto.newContext(allocator, .{
        .sample_rate = 44100,
        .channel_count = 2,  // Stereo
        .format = .int16_le,
        .buffer_size = 0,  // Auto
    });
    defer ctx.deinit();

    // Wait for audio device to be ready
    ctx.waitForReady();

    // Your player code here
}
```

### Playing from Memory

To play audio data already loaded in memory:

```zig
// Load audio data into memory
const audio_data = try std.fs.cwd().readFileAlloc(allocator, "sound.wav", std.math.maxInt(usize));
defer allocator.free(audio_data);

// Create a reader from the memory buffer
var buffer: [1024]u8 = undefined;
var stream = std.io.fixedBufferStream(audio_data);
const reader = stream.reader(&buffer);

// Create and play
const player = try ctx.newPlayer(&reader);
defer player.deinit();

try player.play();
```

### Streaming from a File

For large files or to minimize memory usage, stream directly from a file:

```zig
// Open file (keep it open while playing)
const file = try std.fs.cwd().openFile("song.mp3", .{});
defer file.close();

// Create a reader from the file
var buffer: [1024]u8 = undefined;
const reader = file.reader(&buffer);

// Create and play
const player = try ctx.newPlayer(&reader);
defer player.deinit();

try player.play();

// Wait for playback to finish
while (player.isPlaying()) {
    std.Thread.sleep(std.time.ns_per_ms);
}
```

Note: The file must remain open during playback. Closing it before playback completes will cause errors.

### Supported Formats

- `.float32_le` - 32-bit floating point, little-endian
- `.uint8` - Unsigned 8-bit integer
- `.int16_le` - 16-bit signed integer, little-endian

### Player Controls

```zig
// Check if playing
const is_playing = player.isPlaying();

// Pause playback
player.pause();

// Resume playback (after pause)
try player.play();

// Set volume (0.0 to 1.0)
player.setVolume(0.5);

// Get volume
const volume = player.getVolume();

// Close player when done
try player.close();
```

### Detailed Example

See the [player example in `zigaudio`](https://github.com/braheezy/zigaudio/blob/f86cb9b9cf0cc938965334a2eb73585e3064e96d/examples/player/main.zig).

See the local [`wasm_player`](./examples/wasm_player/).

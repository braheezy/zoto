const std = @import("std");
const builtin = @import("builtin");
const mux = @import("mux.zig");
const Mux = mux.Mux;
const Format = mux.Format;
const Player = mux.Player;

// Windows API constants and types
// WINAPI is stdcall calling convention on Windows
const HRESULT = std.os.windows.HRESULT;
const BOOL = std.os.windows.BOOL;
const DWORD = std.os.windows.DWORD;
const UINT = std.os.windows.UINT;
const HANDLE = std.os.windows.HANDLE;
const LPCWSTR = std.os.windows.LPCWSTR;
const LPWSTR = std.os.windows.LPWSTR;
const LPVOID = std.os.windows.LPVOID;
// In Zig, these are just aliases for *const GUID
const REFIID = *const std.os.windows.GUID;
const REFCLSID = *const std.os.windows.GUID;
const REFGUID = *const std.os.windows.GUID;

// WASAPI constants
const AUDCLNT_SHAREMODE_SHARED = 0;
const AUDCLNT_STREAMFLAGS_EVENTCALLBACK = 0x00040000;
const AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM = 0x80000000;
const AUDCLNT_STREAMFLAGS_NOPERSIST = 0x00080000;

const WAVE_FORMAT_IEEE_FLOAT = 3;
const WAVE_FORMAT_PCM = 1;

// COM interface GUIDs
const IID_IAudioClient = std.os.windows.GUID{
    .Data1 = 0x1cb9ad4c,
    .Data2 = 0xdbfa,
    .Data3 = 0x4c32,
    .Data4 = .{ 0xb1, 0x78, 0xc2, 0xf5, 0x68, 0xa7, 0x03, 0xb2 },
};

const IID_IAudioRenderClient = std.os.windows.GUID{
    .Data1 = 0xf294acfc,
    .Data2 = 0x3146,
    .Data3 = 0x4483,
    .Data4 = .{ 0xa7, 0xbf, 0xad, 0xdc, 0xa7, 0xc2, 0x60, 0xe2 },
};

const IID_ISimpleAudioVolume = std.os.windows.GUID{
    .Data1 = 0x87ce5498,
    .Data2 = 0x68d6,
    .Data3 = 0x44e5,
    .Data4 = .{ 0x92, 0x15, 0x6d, 0xa4, 0x7f, 0x92, 0x4a, 0x8f },
};

const CLSID_MMDeviceEnumerator = std.os.windows.GUID{
    .Data1 = 0xbcde0395,
    .Data2 = 0xe52f,
    .Data3 = 0x467c,
    .Data4 = .{ 0x8e, 0x3d, 0xc4, 0x57, 0x92, 0x91, 0x69, 0x2e },
};

const IID_IMMDeviceEnumerator = std.os.windows.GUID{
    .Data1 = 0xa95664d2,
    .Data2 = 0x9614,
    .Data3 = 0x4f35,
    .Data4 = .{ 0xa7, 0x46, 0xde, 0x8d, 0xb6, 0x36, 0x17, 0xe6 },
};

const IID_IMMDevice = std.os.windows.GUID{
    .Data1 = 0xd666063f,
    .Data2 = 0x1587,
    .Data3 = 0x4e43,
    .Data4 = .{ 0x91, 0xf1, 0xbc, 0x82, 0x74, 0x24, 0x27, 0x84 },
};

const eRender = 0;
const eConsole = 0;

// COM interface structures
const IUnknown = extern struct {
    vtable: *const VTable,

    pub const VTable = extern struct {
        QueryInterface: *const fn (*IUnknown, REFIID, ?*LPVOID) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        AddRef: *const fn (*IUnknown) callconv(std.builtin.CallingConvention.winapi) ULONG,
        Release: *const fn (*IUnknown) callconv(std.builtin.CallingConvention.winapi) ULONG,
    };

    pub fn QueryInterface(self: *IUnknown, riid: REFIID, ppvObject: ?*LPVOID) HRESULT {
        return self.vtable.QueryInterface(self, riid, ppvObject);
    }

    pub fn AddRef(self: *IUnknown) ULONG {
        return self.vtable.AddRef(self);
    }

    pub fn Release(self: *IUnknown) ULONG {
        return self.vtable.Release(self);
    }
};

const IAudioClient = extern struct {
    vtable: *const VTable,

    pub const VTable = extern struct {
        // IUnknown methods
        QueryInterface: *const fn (*IAudioClient, REFIID, ?*LPVOID) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        AddRef: *const fn (*IAudioClient) callconv(std.builtin.CallingConvention.winapi) ULONG,
        Release: *const fn (*IAudioClient) callconv(std.builtin.CallingConvention.winapi) ULONG,
        // IAudioClient methods
        Initialize: *const fn (*IAudioClient, DWORD, DWORD, i64, i64, ?*const WAVEFORMATEX, ?REFGUID) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        GetBufferSize: *const fn (*IAudioClient, *UINT) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        GetStreamLatency: *const fn (*IAudioClient, *i64) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        GetCurrentPadding: *const fn (*IAudioClient, *UINT) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        IsFormatSupported: *const fn (*IAudioClient, DWORD, ?*const WAVEFORMATEX, ?*?*WAVEFORMATEX) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        GetMixFormat: *const fn (*IAudioClient, ?*?*WAVEFORMATEX) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        GetDevicePeriod: *const fn (*IAudioClient, ?*i64, ?*i64) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        Start: *const fn (*IAudioClient) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        Stop: *const fn (*IAudioClient) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        Reset: *const fn (*IAudioClient) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        SetEventHandle: *const fn (*IAudioClient, ?HANDLE) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        GetService: *const fn (*IAudioClient, REFIID, ?*LPVOID) callconv(std.builtin.CallingConvention.winapi) HRESULT,
    };

    pub fn Initialize(self: *IAudioClient, ShareMode: DWORD, StreamFlags: DWORD, hnsBufferDuration: i64, hnsPeriodicity: i64, pFormat: ?*const WAVEFORMATEX, AudioSessionGuid: ?REFGUID) HRESULT {
        return self.vtable.Initialize(self, ShareMode, StreamFlags, hnsBufferDuration, hnsPeriodicity, pFormat, AudioSessionGuid);
    }

    pub fn GetBufferSize(self: *IAudioClient, pNumBufferFrames: *UINT) HRESULT {
        return self.vtable.GetBufferSize(self, pNumBufferFrames);
    }

    pub fn GetCurrentPadding(self: *IAudioClient, pNumPaddingFrames: *UINT) HRESULT {
        return self.vtable.GetCurrentPadding(self, pNumPaddingFrames);
    }

    pub fn Start(self: *IAudioClient) HRESULT {
        return self.vtable.Start(self);
    }

    pub fn Stop(self: *IAudioClient) HRESULT {
        return self.vtable.Stop(self);
    }

    pub fn GetService(self: *IAudioClient, riid: REFIID, ppv: ?*LPVOID) HRESULT {
        return self.vtable.GetService(self, riid, ppv);
    }

    pub fn SetEventHandle(self: *IAudioClient, eventHandle: ?HANDLE) HRESULT {
        return self.vtable.SetEventHandle(self, eventHandle);
    }

    pub fn Release(self: *IAudioClient) ULONG {
        return self.vtable.Release(self);
    }
};

const IAudioRenderClient = extern struct {
    vtable: *const VTable,

    pub const VTable = extern struct {
        // IUnknown methods
        QueryInterface: *const fn (*IAudioRenderClient, REFIID, ?*LPVOID) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        AddRef: *const fn (*IAudioRenderClient) callconv(std.builtin.CallingConvention.winapi) ULONG,
        Release: *const fn (*IAudioRenderClient) callconv(std.builtin.CallingConvention.winapi) ULONG,
        // IAudioRenderClient methods
        GetBuffer: *const fn (*IAudioRenderClient, UINT, ?*?*BYTE) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        ReleaseBuffer: *const fn (*IAudioRenderClient, UINT, DWORD) callconv(std.builtin.CallingConvention.winapi) HRESULT,
    };

    pub fn GetBuffer(self: *IAudioRenderClient, NumFramesRequested: UINT, ppData: ?*?*BYTE) HRESULT {
        return self.vtable.GetBuffer(self, NumFramesRequested, ppData);
    }

    pub fn ReleaseBuffer(self: *IAudioRenderClient, NumFramesWritten: UINT, dwFlags: DWORD) HRESULT {
        return self.vtable.ReleaseBuffer(self, NumFramesWritten, dwFlags);
    }

    pub fn Release(self: *IAudioRenderClient) ULONG {
        return self.vtable.Release(self);
    }
};

const IMMDeviceEnumerator = extern struct {
    vtable: *const VTable,

    pub const VTable = extern struct {
        // IUnknown methods
        QueryInterface: *const fn (*IMMDeviceEnumerator, REFIID, ?*LPVOID) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        AddRef: *const fn (*IMMDeviceEnumerator) callconv(std.builtin.CallingConvention.winapi) ULONG,
        Release: *const fn (*IMMDeviceEnumerator) callconv(std.builtin.CallingConvention.winapi) ULONG,
        // IMMDeviceEnumerator methods
        EnumAudioEndpoints: *const fn (*IMMDeviceEnumerator, DWORD, DWORD, ?*?*IMMDevice) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        GetDefaultAudioEndpoint: *const fn (*IMMDeviceEnumerator, DWORD, DWORD, ?*?*IMMDevice) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        GetDevice: *const fn (*IMMDeviceEnumerator, LPCWSTR, ?*?*IMMDevice) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        RegisterEndpointNotificationCallback: *const fn (*IMMDeviceEnumerator, ?*IMMNotificationClient) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        UnregisterEndpointNotificationCallback: *const fn (*IMMDeviceEnumerator, ?*IMMNotificationClient) callconv(std.builtin.CallingConvention.winapi) HRESULT,
    };

    pub fn GetDefaultAudioEndpoint(self: *IMMDeviceEnumerator, dataFlow: DWORD, role: DWORD, ppDevice: ?*?*IMMDevice) HRESULT {
        return self.vtable.GetDefaultAudioEndpoint(self, dataFlow, role, ppDevice);
    }

    pub fn Release(self: *IMMDeviceEnumerator) ULONG {
        return self.vtable.Release(self);
    }
};

const IMMDevice = extern struct {
    vtable: *const VTable,

    pub const VTable = extern struct {
        // IUnknown methods
        QueryInterface: *const fn (*IMMDevice, REFIID, ?*LPVOID) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        AddRef: *const fn (*IMMDevice) callconv(std.builtin.CallingConvention.winapi) ULONG,
        Release: *const fn (*IMMDevice) callconv(std.builtin.CallingConvention.winapi) ULONG,
        // IMMDevice methods
        Activate: *const fn (*IMMDevice, REFIID, DWORD, ?*PROPVARIANT, ?*LPVOID) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        OpenPropertyStore: *const fn (*IMMDevice, DWORD, ?*?*IPropertyStore) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        GetId: *const fn (*IMMDevice, ?*LPWSTR) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        GetState: *const fn (*IMMDevice, ?*DWORD) callconv(std.builtin.CallingConvention.winapi) HRESULT,
    };

    pub fn Activate(self: *IMMDevice, iid: REFIID, dwClsCtx: DWORD, pActivationParams: ?*PROPVARIANT, ppInterface: ?*LPVOID) HRESULT {
        return self.vtable.Activate(self, iid, dwClsCtx, pActivationParams, ppInterface);
    }

    pub fn Release(self: *IMMDevice) ULONG {
        return self.vtable.Release(self);
    }
};

// Additional types
const BYTE = u8;
const ULONG = u32;
const REFERENCE_TIME = i64;
const PROPVARIANT = extern struct {
    vt: u16,
    wReserved1: u16,
    wReserved2: u16,
    wReserved3: u16,
    data: [16]u8,
};

const IPropertyStore = extern struct {
    vtable: *const VTable,

    pub const VTable = extern struct {
        QueryInterface: *const fn (*IPropertyStore, REFIID, ?*LPVOID) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        AddRef: *const fn (*IPropertyStore) callconv(std.builtin.CallingConvention.winapi) ULONG,
        Release: *const fn (*IPropertyStore) callconv(std.builtin.CallingConvention.winapi) ULONG,
        GetCount: *const fn (*IPropertyStore, ?*DWORD) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        GetAt: *const fn (*IPropertyStore, DWORD, ?*PROPERTYKEY) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        GetValue: *const fn (*IPropertyStore, ?*const PROPERTYKEY, ?*PROPVARIANT) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        SetValue: *const fn (*IPropertyStore, ?*const PROPERTYKEY, ?*const PROPVARIANT) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        Commit: *const fn (*IPropertyStore) callconv(std.builtin.CallingConvention.winapi) HRESULT,
    };
};

const PROPERTYKEY = extern struct {
    fmtid: std.os.windows.GUID,
    pid: DWORD,
};

const IMMNotificationClient = extern struct {
    vtable: *const VTable,

    pub const VTable = extern struct {
        QueryInterface: *const fn (*IMMNotificationClient, REFIID, ?*LPVOID) callconv(std.builtin.CallingConvention.winapi) HRESULT,
        AddRef: *const fn (*IMMNotificationClient) callconv(std.builtin.CallingConvention.winapi) ULONG,
        Release: *const fn (*IMMNotificationClient) callconv(std.builtin.CallingConvention.winapi) ULONG,
        OnDeviceStateChanged: *const fn (*IMMNotificationClient, LPCWSTR, DWORD) callconv(std.builtin.CallingConvention.winapi) void,
        OnDeviceAdded: *const fn (*IMMNotificationClient, LPCWSTR) callconv(std.builtin.CallingConvention.winapi) void,
        OnDeviceRemoved: *const fn (*IMMNotificationClient, LPCWSTR) callconv(std.builtin.CallingConvention.winapi) void,
        OnDefaultDeviceChanged: *const fn (*IMMNotificationClient, DWORD, DWORD, LPCWSTR) callconv(std.builtin.CallingConvention.winapi) void,
        OnPropertyValueChanged: *const fn (*IMMNotificationClient, LPCWSTR, PROPERTYKEY) callconv(std.builtin.CallingConvention.winapi) void,
    };
};

const WAVEFORMATEX = extern struct {
    wFormatTag: u16,
    nChannels: u16,
    nSamplesPerSec: u32,
    nAvgBytesPerSec: u32,
    nBlockAlign: u16,
    wBitsPerSample: u16,
    cbSize: u16,
};

// Windows API functions
extern "ole32" fn CoInitializeEx(pvReserved: ?LPVOID, dwCoInit: DWORD) HRESULT;
extern "ole32" fn CoUninitialize() void;
extern "ole32" fn CoCreateInstance(rclsid: REFCLSID, pUnkOuter: ?*IUnknown, dwClsContext: DWORD, riid: REFIID, ppv: ?*LPVOID) HRESULT;

extern "kernel32" fn CreateEventW(lpEventAttributes: ?*anyopaque, bManualReset: BOOL, bInitialState: BOOL, lpName: ?LPCWSTR) ?HANDLE;
extern "kernel32" fn CloseHandle(hObject: HANDLE) BOOL;
extern "kernel32" fn WaitForSingleObject(hHandle: HANDLE, dwMilliseconds: DWORD) DWORD;

const COINIT_MULTITHREADED = 0x0;
const CLSCTX_ALL = 0x17;

// Windows constants
const WAIT_OBJECT_0 = 0x00000000;

// Context structure
pub const Context = struct {
    audio_client: ?*IAudioClient = null,
    render_client: ?*IAudioRenderClient = null,
    buffer_size_in_frames: UINT = 0,
    event_handle: ?HANDLE = null,
    mux: *Mux,
    mutex: std.Thread.Mutex,
    condition: std.Thread.Condition,
    suspended: bool = false,
    ready: bool = false,
    err: ?anyerror = null,
    allocator: std.mem.Allocator,
    thread_handle: ?std.Thread = null,
    channel_count: u32,

    pub fn init(allocator: std.mem.Allocator, sample_rate: u32, channel_count: u32, format: Format, _: u32) !*Context {
        const c = try allocator.create(Context);
        c.* = Context{
            .mux = try Mux.init(allocator, sample_rate, @intCast(channel_count), format),
            .mutex = .{},
            .condition = .{},
            .allocator = allocator,
            .channel_count = channel_count,
        };

        // Initialize COM
        const hr = CoInitializeEx(null, COINIT_MULTITHREADED);
        if (hr < 0) {
            allocator.destroy(c);
            return error.COMInitializationFailed;
        }

        // Create audio client
        c.createAudioClient(sample_rate, channel_count) catch |err| {
            c.mux.deinit();
            allocator.destroy(c);
            CoUninitialize();
            return err;
        };

        // Start audio thread
        c.thread_handle = std.Thread.spawn(.{}, audioThread, .{c}) catch |err| {
            c.mux.deinit();
            allocator.destroy(c);
            CoUninitialize();
            return err;
        };

        return c;
    }

    fn createAudioClient(self: *Context, sample_rate: u32, channel_count: u32) !void {
        var device_enumerator: ?*IMMDeviceEnumerator = null;
        var device: ?*IMMDevice = null;
        var audio_client: ?*IAudioClient = null;

        // Create device enumerator
        const hr = CoCreateInstance(
            &CLSID_MMDeviceEnumerator,
            null,
            CLSCTX_ALL,
            &IID_IMMDeviceEnumerator,
            @ptrCast(&device_enumerator),
        );
        if (hr < 0) {
            return error.CreateDeviceEnumeratorFailed;
        }
        defer if (device_enumerator) |de| {
            _ = de.Release();
        };

        // Get default audio endpoint
        const hr2 = device_enumerator.?.GetDefaultAudioEndpoint(eRender, eConsole, &device);
        if (hr2 < 0) {
            std.debug.print("Failed to get default audio endpoint, HRESULT: 0x{X}\n", .{@as(u32, @bitCast(hr2))});
            return error.GetDefaultAudioEndpointFailed;
        }
        std.debug.print("Successfully got default audio endpoint\n", .{});
        defer if (device) |d| {
            _ = d.Release();
        };

        // Activate audio client
        const hr3 = device.?.Activate(&IID_IAudioClient, CLSCTX_ALL, null, @ptrCast(&audio_client));
        if (hr3 < 0) {
            return error.ActivateAudioClientFailed;
        }

        // Set up wave format
        const wave_format = WAVEFORMATEX{
            .wFormatTag = WAVE_FORMAT_IEEE_FLOAT,
            .nChannels = @intCast(channel_count),
            .nSamplesPerSec = sample_rate,
            .nAvgBytesPerSec = sample_rate * channel_count * 4, // 4 bytes per float
            .nBlockAlign = @intCast(channel_count * 4),
            .wBitsPerSample = 32,
            .cbSize = 0,
        };

        // Initialize audio client
        std.debug.print("Initializing audio client with format: {} Hz, {} channels\n", .{ sample_rate, channel_count });
        const hr4 = audio_client.?.Initialize(
            AUDCLNT_SHAREMODE_SHARED,
            AUDCLNT_STREAMFLAGS_EVENTCALLBACK | AUDCLNT_STREAMFLAGS_NOPERSIST | AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM,
            10000000, // 100ms buffer
            0,
            &wave_format,
            null,
        );
        if (hr4 < 0) {
            std.debug.print("Failed to initialize audio client, HRESULT: 0x{X}\n", .{@as(u32, @bitCast(hr4))});
            return error.InitializeAudioClientFailed;
        }
        std.debug.print("Successfully initialized audio client\n", .{});

        // Get buffer size
        var buffer_size: UINT = 0;
        const hr5 = audio_client.?.GetBufferSize(&buffer_size);
        if (hr5 < 0) {
            return error.GetBufferSizeFailed;
        }

        // Create event handle
        const event_handle = CreateEventW(null, 0, 0, null);
        if (event_handle == null) {
            return error.CreateEventFailed;
        }

        // Set event handle
        const hr6 = audio_client.?.SetEventHandle(event_handle);
        if (hr6 < 0) {
            _ = CloseHandle(event_handle.?);
            return error.SetEventHandleFailed;
        }

        // Get render client
        var render_client: ?*IAudioRenderClient = null;
        const hr7 = audio_client.?.GetService(&IID_IAudioRenderClient, @ptrCast(&render_client));
        if (hr7 < 0) {
            _ = CloseHandle(event_handle.?);
            return error.GetRenderClientFailed;
        }

        self.audio_client = audio_client;
        self.render_client = render_client;
        self.buffer_size_in_frames = buffer_size;
        self.event_handle = event_handle;
    }

    pub fn deinit(self: *Context) void {
        // Signal thread to stop
        self.mutex.lock();
        self.err = error.ContextDestroyed;
        self.condition.signal();
        self.mutex.unlock();

        // Wait for thread to finish
        if (self.thread_handle) |thread| {
            thread.join();
        }

        // Clean up resources
        if (self.event_handle) |handle| {
            _ = CloseHandle(handle);
        }
        if (self.render_client) |rc| _ = rc.Release();
        if (self.audio_client) |ac| _ = ac.Release();

        self.mux.deinit();
        self.allocator.destroy(self);

        // Uninitialize COM
        CoUninitialize();
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

        if (self.err) |err| return err;
        self.suspended = true;
        self.condition.signal();
    }

    pub fn play(self: *Context) !void {
        self.mutex.lock();
        defer self.mutex.unlock();

        if (self.err) |err| return err;
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

    fn audioThread(ctx: *Context) void {
        // Start audio client
        const hr = ctx.audio_client.?.Start();
        if (hr < 0) {
            ctx.mutex.lock();
            ctx.err = error.StartAudioClientFailed;
            ctx.condition.signal();
            ctx.mutex.unlock();
            return;
        }

        // Signal ready
        ctx.mutex.lock();
        ctx.ready = true;
        ctx.condition.signal();
        ctx.mutex.unlock();

        // Audio processing loop - allocate a buffer that can be resized as needed
        var buffer = std.ArrayList(f32).empty;
        defer buffer.deinit(ctx.allocator);

        while (true) {
            // Wait for event or suspension
            ctx.mutex.lock();
            while (ctx.suspended and ctx.err == null) {
                ctx.condition.wait(&ctx.mutex);
            }
            const should_exit = ctx.err != null;
            ctx.mutex.unlock();

            if (should_exit) break;

            // Wait for audio event
            const wait_result = WaitForSingleObject(ctx.event_handle.?, 1000);
            if (wait_result != WAIT_OBJECT_0) {
                continue;
            }

            // Get current padding
            var padding: UINT = 0;
            const hr2 = ctx.audio_client.?.GetCurrentPadding(&padding);
            if (hr2 < 0) continue;

            const frames_to_write = ctx.buffer_size_in_frames - padding;
            if (frames_to_write == 0) continue;

            // Get buffer
            var data: ?*BYTE = null;
            const hr3 = ctx.render_client.?.GetBuffer(frames_to_write, &data);
            if (hr3 < 0) continue;

            // Calculate buffer size in samples (frames * channels)
            const buffer_len = frames_to_write * ctx.channel_count;

            // Resize buffer if needed
            if (buffer.capacity < buffer_len) {
                buffer.ensureTotalCapacity(ctx.allocator, buffer_len) catch continue;
            }
            buffer.items.len = buffer_len;

            // Read audio data from mux
            ctx.mux.readFloat32s(buffer.items) catch continue;

            // Copy to WASAPI buffer
            const float_data: [*]f32 = @ptrCast(@alignCast(data));
            @memcpy(float_data[0..buffer_len], buffer.items);

            // Release buffer
            _ = ctx.render_client.?.ReleaseBuffer(frames_to_write, 0);
        }
    }
};

// Global context for callbacks
var global_context: ?*Context = null;

fn audioCallback(user_data: ?*anyopaque, aq: HANDLE, buffer: HANDLE) callconv(std.builtin.CallingConvention.winapi) void {
    _ = user_data;
    _ = aq;
    _ = buffer;
    // This is handled in the audio thread instead
}

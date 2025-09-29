const std = @import("std");

const Endian = std.builtin.Endian;
const Allocator = std.mem.Allocator;

pub const Pool = @import("pool.zig").Pool;

pub const Buffer = struct {
    // Two allocators! This is largely a feature meant to be used with the Pool.
    // Imagine you have a pool of 100 Buffers. Each one has a static buffer
    // of 2K, allocated with a general purpose allocator. We store that in _a.
    // Now you acquire one and start to write. You write more than 2K, so we
    // need to allocate `dynamic`. Yes, we could use our general purpose allocator
    // (aka _a), but what if the app would like to use a different allocator for
    // that, like an Arena?
    // Thus, `static` is always allocated with _a, and apps can opt to use a
    // different allocator, _da, to manage `dynamic`. `_da` is meant to be set
    // via pool.acquireWithAllocator since we expect _da to be transient.
    _a: Allocator,

    _da: ?Allocator,

    // where in buf we are
    pos: usize,

    // points to either static of dynamic.?
    buf: []u8,

    // fixed size, created on startup
    static: []u8,

    // created when we try to write more than static.len
    dynamic: ?[]u8,

    pub fn init(allocator: Allocator, size: usize) !Buffer {
        const static = try allocator.alloc(u8, size);
        return .{
            ._a = allocator,
            ._da = null,
            .pos = 0,
            .buf = static,
            .static = static,
            .dynamic = null,
        };
    }

    pub fn deinit(self: Buffer) void {
        const allocator = self._a;
        allocator.free(self.static);
        if (self.dynamic) |dyn| {
            (self._da orelse allocator).free(dyn);
        }
    }

    pub fn reset(self: *Buffer) void {
        self.pos = 0;
        if (self.dynamic) |dyn| {
            (self._da orelse self._a).free(dyn);
            self.dynamic = null;
            self.buf = self.static;
        }
        self._da = null;
    }

    pub fn resetRetainingCapacity(self: *Buffer) void {
        self.pos = 0;
    }

    pub fn len(self: Buffer) usize {
        return self.pos;
    }

    pub fn string(self: Buffer) []const u8 {
        return self.buf[0..self.pos];
    }

    pub fn cString(self: *Buffer) ![:0]const u8 {
        // Make sure there is a null byte at the end

        try self.ensureUnusedCapacity(1);
        self.buf[self.pos] = 0;

        return self.buf[0..self.pos :0];
    }

    pub fn truncate(self: *Buffer, n: usize) void {
        const pos = self.pos;
        if (n >= pos) {
            self.pos = 0;
            return;
        }
        self.pos = pos - n;
    }

    pub fn skip(self: *Buffer, n: usize) !View {
        try self.ensureUnusedCapacity(n);
        const pos = self.pos;
        self.pos = pos + n;
        return .{
            .pos = pos,
            .buf = self,
        };
    }

    pub fn writeByte(self: *Buffer, b: u8) !void {
        try self.ensureUnusedCapacity(1);
        self.writeByteAssumeCapacity(b);
    }

    pub fn writeByteAssumeCapacity(self: *Buffer, b: u8) void {
        const pos = self.pos;
        writeByteInto(self.buf, pos, b);
        self.pos = pos + 1;
    }

    pub fn writeByteNTimes(self: *Buffer, b: u8, n: usize) !void {
        try self.ensureUnusedCapacity(n);
        const pos = self.pos;
        writeByteNTimesInto(self.buf, pos, b, n);
        self.pos = pos + n;
    }

    pub fn write(self: *Buffer, data: []const u8) !void {
        try self.ensureUnusedCapacity(data.len);
        self.writeAssumeCapacity(data);
    }

    pub fn writeAssumeCapacity(self: *Buffer, data: []const u8) void {
        const pos = self.pos;
        writeInto(self.buf, pos, data);
        self.pos = pos + data.len;
    }

    // unsafe
    pub fn writeAt(self: *Buffer, data: []const u8, pos: usize) void {
        @memcpy(self.buf[pos .. pos + data.len], data);
    }

    pub fn writeU16Little(self: *Buffer, value: u16) !void {
        return self.writeIntT(u16, value, .little);
    }

    pub fn writeU32Little(self: *Buffer, value: u32) !void {
        return self.writeIntT(u32, value, .little);
    }

    pub fn writeU64Little(self: *Buffer, value: u64) !void {
        return self.writeIntT(u64, value, .little);
    }

    pub fn writeIntLittle(self: *Buffer, comptime T: type, value: T) !void {
        return self.writeIntT(T, value, .little);
    }

    pub fn writeU16Big(self: *Buffer, value: u16) !void {
        return self.writeIntT(u16, value, .big);
    }

    pub fn writeU32Big(self: *Buffer, value: u32) !void {
        return self.writeIntT(u32, value, .big);
    }

    pub fn writeU64Big(self: *Buffer, value: u64) !void {
        return self.writeIntT(u64, value, .big);
    }

    pub fn writeIntBig(self: *Buffer, comptime T: type, value: T) !void {
        return self.writeIntT(T, value, .big);
    }

    pub fn writeIntT(self: *Buffer, comptime T: type, value: T, endian: Endian) !void {
        const l = @divExact(@typeInfo(T).int.bits, 8);
        const pos = self.pos;
        try self.ensureUnusedCapacity(l);
        writeIntInto(T, self.buf, pos, value, l, endian);
        self.pos = pos + l;
    }

    pub fn ensureUnusedCapacity(self: *Buffer, n: usize) !void {
        return self.ensureTotalCapacity(self.pos + n);
    }

    pub fn ensureTotalCapacity(self: *Buffer, required_capacity: usize) !void {
        const buf = self.buf;
        if (required_capacity <= buf.len) {
            return;
        }

        // from std.ArrayList
        var new_capacity = buf.len;
        while (true) {
            new_capacity +|= new_capacity / 2 + 8;
            if (new_capacity >= required_capacity) break;
        }

        const allocator = self._da orelse self._a;
        if (buf.ptr == self.static.ptr or !allocator.resize(buf, new_capacity)) {
            const new_buffer = try allocator.alloc(u8, new_capacity);
            @memcpy(new_buffer[0..buf.len], buf);

            if (self.dynamic) |dyn| {
                allocator.free(dyn);
            }

            self.buf = new_buffer;
            self.dynamic = new_buffer;
        } else {
            const new_buffer = buf.ptr[0..new_capacity];
            self.buf = new_buffer;
            self.dynamic = new_buffer;
        }
    }

    pub fn copy(self: Buffer, allocator: Allocator) ![]const u8 {
        const pos = self.pos;
        const c = try allocator.alloc(u8, pos);
        @memcpy(c, self.buf[0..pos]);
        return c;
    }

    // Writer interface removed for Zig 0.15+. Implement a custom adapter if needed.
};

pub const View = struct {
    pos: usize,
    buf: *Buffer,

    pub fn writeByte(self: *View, b: u8) void {
        const pos = self.pos;
        writeByteInto(self.buf.buf, pos, b);
        self.pos = pos + 1;
    }

    pub fn writeByteNTimes(self: *View, b: u8, n: usize) void {
        const pos = self.pos;
        writeByteNTimesInto(self.buf.buf, pos, b, n);
        self.pos = pos + n;
    }

    pub fn write(self: *View, data: []const u8) void {
        const pos = self.pos;
        writeInto(self.buf.buf, pos, data);
        self.pos = pos + data.len;
    }

    pub fn writeU16(self: *View, value: u16) void {
        return self.writeIntT(u16, value, self.endian);
    }

    pub fn writeI16(self: *View, value: i16) void {
        return self.writeIntT(i16, value, self.endian);
    }

    pub fn writeU32(self: *View, value: u32) void {
        return self.writeIntT(u32, value, self.endian);
    }

    pub fn writeI32(self: *View, value: i32) void {
        return self.writeIntT(i32, value, self.endian);
    }

    pub fn writeU64(self: *View, value: u64) void {
        return self.writeIntT(u64, value, self.endian);
    }

    pub fn writeI64(self: *View, value: i64) void {
        return self.writeIntT(i64, value, self.endian);
    }

    pub fn writeU16Little(self: *View, value: u16) void {
        return self.writeIntT(u16, value, .little);
    }

    pub fn writeI16Little(self: *View, value: i16) void {
        return self.writeIntT(i16, value, .little);
    }

    pub fn writeU32Little(self: *View, value: u32) void {
        return self.writeIntT(u32, value, .little);
    }

    pub fn writeI32Little(self: *View, value: i32) void {
        return self.writeIntT(i32, value, .little);
    }

    pub fn writeU64Little(self: *View, value: u64) void {
        return self.writeIntT(u64, value, .little);
    }

    pub fn writeI64Little(self: *View, value: i64) void {
        return self.writeIntT(i64, value, .little);
    }

    pub fn writeIntLittle(self: *View, comptime T: type, value: T) void {
        self.writeIntT(T, value, .little);
    }

    pub fn writeU16Big(self: *View, value: u16) void {
        return self.writeIntT(u16, value, .big);
    }

    pub fn writeI16Big(self: *View, value: i16) void {
        return self.writeIntT(i16, value, .big);
    }

    pub fn writeU32Big(self: *View, value: u32) void {
        return self.writeIntT(u32, value, .big);
    }

    pub fn writeI32Big(self: *View, value: i32) void {
        return self.writeIntT(i32, value, .big);
    }

    pub fn writeU64Big(self: *View, value: u64) void {
        return self.writeIntT(u64, value, .big);
    }

    pub fn writeI64Big(self: *View, value: i64) void {
        return self.writeIntT(i64, value, .big);
    }

    pub fn writeIntBig(self: *View, comptime T: type, value: T) void {
        self.writeIntT(T, value, .big);
    }

    pub fn writeIntT(self: *View, comptime T: type, value: T, endian: Endian) void {
        const l = @divExact(@typeInfo(T).int.bits, 8);
        const pos = self.pos;
        writeIntInto(T, self.buf.buf, pos, value, l, endian);
        self.pos = pos + l;
    }
};

// Functions that write for either a *StringBuilder or a *View
inline fn writeInto(buf: []u8, pos: usize, data: []const u8) void {
    const end_pos = pos + data.len;
    @memcpy(buf[pos..end_pos], data);
}

inline fn writeByteInto(buf: []u8, pos: usize, b: u8) void {
    buf[pos] = b;
}

inline fn writeByteNTimesInto(buf: []u8, pos: usize, b: u8, n: usize) void {
    for (0..n) |offset| {
        buf[pos + offset] = b;
    }
}

inline fn writeIntInto(comptime T: type, buf: []u8, pos: usize, value: T, l: usize, endian: Endian) void {
    const end_pos = pos + l;
    std.mem.writeInt(T, buf[pos..end_pos][0..l], value, endian);
}

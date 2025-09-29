const std = @import("std");
const builtin = @import("builtin");

const Buffer = @import("buffer.zig").Buffer;

const Mutex = std.Thread.Mutex;
const Allocator = std.mem.Allocator;

pub const Pool = struct {
    mutex: Mutex,
    available: usize,
    allocator: Allocator,
    buffer_size: usize,
    buffers: []*Buffer,

    pub fn init(allocator: Allocator, pool_size: u16, buffer_size: usize) !Pool {
        const buffers = try allocator.alloc(*Buffer, pool_size);

        for (0..pool_size) |i| {
            const sb = try allocator.create(Buffer);
            sb.* = try Buffer.init(allocator, buffer_size);
            buffers[i] = sb;
        }

        return .{ .mutex = .{}, .buffers = buffers, .allocator = allocator, .available = pool_size, .buffer_size = buffer_size };
    }

    pub fn deinit(self: *Pool) void {
        const allocator = self.allocator;
        for (self.buffers) |sb| {
            sb.deinit();
            allocator.destroy(sb);
        }
        allocator.free(self.buffers);
    }

    pub fn acquire(self: *Pool) !*Buffer {
        return self.acquireWithAllocator(self.allocator);
    }

    pub fn acquireWithAllocator(self: *Pool, dyn_allocator: Allocator) !*Buffer {
        const buffers = self.buffers;

        self.mutex.lock();
        const available = self.available;
        if (available == 0) {
            // dont hold the lock over factory
            self.mutex.unlock();

            const allocator = self.allocator;
            const sb = try allocator.create(Buffer);
            sb.* = try Buffer.init(allocator, self.buffer_size);
            sb._da = dyn_allocator;
            return sb;
        }
        const index = available - 1;
        const sb = buffers[index];
        self.available = index;
        self.mutex.unlock();
        sb._da = dyn_allocator;
        return sb;
    }

    pub fn release(self: *Pool, sb: *Buffer) void {
        sb.reset();
        self.mutex.lock();

        var buffers = self.buffers;
        const available = self.available;
        if (available == buffers.len) {
            self.mutex.unlock();
            const allocator = self.allocator;
            sb.deinit();
            allocator.destroy(sb);
            return;
        }
        buffers[available] = sb;
        self.available = available + 1;
        self.mutex.unlock();
    }
};

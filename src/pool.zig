const std = @import("std");
const builtin = @import("builtin");

const Buffer = @import("buffer.zig").Buffer;

const Allocator = std.mem.Allocator;

const CompatMutex = if (builtin.single_threaded) struct {} else struct {
    inner: std.Io.Mutex = .init,

    fn lock(self: *@This()) void {
        self.inner.lockUncancelable(std.Options.debug_io);
    }

    fn unlock(self: *@This()) void {
        self.inner.unlock(std.Options.debug_io);
    }
};

pub const Pool = struct {
    mutex: CompatMutex = .{},
    available: usize,
    allocator: Allocator,
    buffer_size: usize,
    buffers: []*Buffer,

    pub fn init(allocator: Allocator, pool_size: u16, buffer_size: usize) !Pool {
        const buffers = try allocator.alloc(*Buffer, pool_size);
        errdefer allocator.free(buffers);

        var initialized: usize = 0;
        errdefer for (buffers[0..initialized]) |sb| {
            sb.deinit();
            allocator.destroy(sb);
        };

        for (0..pool_size) |i| {
            const sb = try allocator.create(Buffer);
            errdefer allocator.destroy(sb);
            sb.* = try Buffer.init(allocator, buffer_size);
            buffers[i] = sb;
            initialized += 1;
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

        if (!builtin.single_threaded) self.mutex.lock();
        const available = self.available;
        if (available == 0) {
            // dont hold the lock over factory
            if (!builtin.single_threaded) self.mutex.unlock();

            const allocator = self.allocator;
            const sb = try allocator.create(Buffer);
            errdefer allocator.destroy(sb);
            sb.* = try Buffer.init(allocator, self.buffer_size);
            sb._da = dyn_allocator;
            return sb;
        }
        const index = available - 1;
        const sb = buffers[index];
        self.available = index;
        if (!builtin.single_threaded) self.mutex.unlock();
        // Existing dynamic storage must be freed by its original allocator.
        const previous_allocator = sb._da orelse sb._a;
        if (previous_allocator.ptr != dyn_allocator.ptr or
            previous_allocator.vtable != dyn_allocator.vtable)
        {
            sb.reset();
        }
        sb._da = dyn_allocator;
        return sb;
    }

    pub fn release(self: *Pool, sb: *Buffer) void {
        // Retain capacity owned by the pool allocator. A borrower's allocator
        // may expire after release, so free its dynamic storage now.
        const dynamic_allocator = sb._da orelse sb._a;
        if (dynamic_allocator.ptr == self.allocator.ptr and
            dynamic_allocator.vtable == self.allocator.vtable)
        {
            sb.resetRetainingCapacity();
        } else {
            sb.reset();
        }

        if (!builtin.single_threaded) self.mutex.lock();

        var buffers = self.buffers;
        const available = self.available;
        if (available == buffers.len) {
            if (!builtin.single_threaded) self.mutex.unlock();
            const allocator = self.allocator;
            sb.deinit();
            allocator.destroy(sb);
            return;
        }
        buffers[available] = sb;
        self.available = available + 1;
        if (!builtin.single_threaded) self.mutex.unlock();
    }
};

fn poolAllocationScenario(allocator: std.mem.Allocator) !void {
    var pool = try Pool.init(allocator, 3, 8);
    defer pool.deinit();
    const first = try pool.acquire();
    defer pool.release(first);
    const second = try pool.acquire();
    defer pool.release(second);
    const third = try pool.acquire();
    defer pool.release(third);
    const overflow = try pool.acquire();
    defer pool.release(overflow);
}

test "pool construction and overflow allocation clean up every failure" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, poolAllocationScenario, .{});
}

test "pool keeps its capacity but releases custom allocator storage" {
    var pool = try Pool.init(std.testing.allocator, 1, 4);
    defer pool.deinit();
    const first = try pool.acquire();
    try first.ensureTotalCapacity(64);
    const retained = first.buf.ptr;
    pool.release(first);
    const second = try pool.acquire();
    try std.testing.expectEqual(retained, second.buf.ptr);
    pool.release(second);
    {
        var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
        defer arena.deinit();
        const custom = try pool.acquireWithAllocator(arena.allocator());
        try std.testing.expect(custom.dynamic == null);
        try custom.ensureTotalCapacity(128);
        pool.release(custom);
        try std.testing.expect(custom.dynamic == null);
    }
    const last = try pool.acquire();
    defer pool.release(last);
    try last.ensureTotalCapacity(256);
}

const std = @import("std");
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const root_mod = b.addModule("zoto", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });

    const target_os = target.result.os.tag;

    if (target_os == .macos) {
        root_mod.addImport("objc", b.dependency("zig_objc", .{
            .target = target,
            .optimize = optimize,
        }).module("objc"));
        root_mod.linkFramework("AudioToolbox", .{});
    } else if (target_os == .linux) {
        root_mod.linkSystemLibrary("asound", .{});
    }
}

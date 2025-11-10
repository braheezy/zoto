const std = @import("std");
pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});

    const wasm_target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .wasi,
    });

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("main.zig"),
        .target = wasm_target,
        .optimize = optimize,
        .link_libc = false,
    });

    const zoto_dep = b.dependency("zoto", .{
        .target = wasm_target,
        .optimize = optimize,
        // .single_threaded = true,
    });
    const zoto_mod = zoto_dep.module("zoto");
    zoto_mod.link_libc = false;
    exe_mod.addImport("zoto", zoto_mod);

    const wasm_exe = b.addExecutable(.{
        .name = "player",
        .root_module = exe_mod,
    });
    wasm_exe.root_module.single_threaded = true;
    wasm_exe.rdynamic = true;
    wasm_exe.entry = .disabled; // WASM doesn't need _start entry point
    wasm_exe.export_memory = true; // Export memory for JavaScript access

    const wasm_install = b.addInstallArtifact(wasm_exe, .{});

    const wasm_step = b.step("player", "Build WASM player example");
    wasm_step.dependOn(&wasm_install.step);

    b.installArtifact(wasm_exe);
}

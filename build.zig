const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Compile library
    const lib = b.addStaticLibrary(.{
        .name = "pretty",
        .root_source_file = .{ .path = "src/pretty.zig" },
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(lib);

    // Run tests
    const lib_tests = b.addTest(.{
        .name = "tests",
        .root_source_file = .{ .path = "src/pretty.zig" },
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(lib_tests);

    const step_tests = b.step("test", "Run pretty tests");
    const run_lib_tests = b.addRunArtifact(lib_tests);
    step_tests.dependOn(&run_lib_tests.step);

    // Run example
    const example = b.addExecutable(.{
        .name = "pretty_example",
        .root_source_file = .{ .path = "src/example.zig" },
        .target = target,
        .optimize = optimize,
    });

    const step_run = b.step("run", "Run a pretty usage example");
    step_run.dependOn(&example.step);
}

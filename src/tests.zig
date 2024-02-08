const std = @import("std");
const pretty = @import("pretty.zig");


test {
    // if (true) return error.SkipZigTest;
    const run = struct {
        pub fn case(input: anytype, expected: []const u8, comptime opt: pretty.Options) !void {
            const actual = try pretty.dump(std.testing.allocator, input, opt);
            defer actual.deinit();
            try std.testing.expectEqualStrings(expected, actual.items);
        }
    };

    // ------------------------
    // Test primitives
    // ------------------------
    try run.case(42,
        \\comptime_int
        \\  42
        \\
    , .{});

    try run.case(@as(u8, 42),
        \\u8
        \\  42
        \\
    , .{});

    // ------------------------
    // Test arrays
    // ------------------------
    const arr_1: [3]u8 = .{ 1, 2, 3 };

    try run.case(arr_1,
        \\[3]u8
        \\  1
        \\  2
        \\  3
        \\
    , .{ .arr_show_item_idx = false });

    try run.case(arr_1,
        \\[3]u8
        \\  0: 1
        \\  1: 2
        \\  2: 3
        \\
    , .{ .arr_show_item_idx = true });

    // ------------------------
    // Test structs
    // ------------------------
    const struct_1: struct {
        field1: bool = true,
        field2: u8 = 42,
        field3: f32 = 1.1,
    } = .{};

    try run.case(struct_1,
        \\.field1: bool
        \\  true
        \\.field2: u8
        \\  42
        \\.field3: f32
        \\  1.10000002e+00
        \\
    , .{
        .filter_depths = .{ .exclude = &.{0} },
    });

    try run.case(struct_1,
        \\.field1:
        \\  true
        \\.field2:
        \\  42
        \\.field3:
        \\  1.10000002e+00
        \\
    , .{
        .filter_depths = .{ .exclude = &.{0} },
        .show_type_names = false,
    });

    try run.case(struct_1,
        \\.field1:
        \\.field2:
        \\.field3:
        \\
    , .{
        .filter_depths = .{ .exclude = &.{0} },
        .show_type_names = false,
        .show_vals = false,
    });

    try run.case(struct_1,
        \\.field1: [Bool]
        \\.field2: [Int]
        \\.field3: [Float]
        \\
    , .{
        .filter_depths = .{ .exclude = &.{0} },
        .show_type_tags = true,
        .show_type_names = false,
        .show_vals = false,
    });

    try run.case(struct_1,
        \\.field1: bool
        \\.field2: u8
        \\.field3: f32
        \\
    , .{
        .filter_depths = .{ .include = &.{1} },
    });

    try run.case(struct_1,
        \\true
        \\42
        \\1.10000002e+00
        \\
    , .{
        .filter_depths = .{ .include = &.{2} },
    });

    try run.case(struct_1,
        \\.field2: u8
        \\  42
        \\
    , .{
        .filter_depths = .{ .exclude = &.{0} },
        .filter_field_types = .{ .exclude = &.{ .Bool, .Float } },
    });

    try run.case(struct_1,
        \\.field2: u8
        \\  42
        \\
    , .{
        .filter_depths = .{ .exclude = &.{0} },
        .filter_field_types = .{ .exclude = &.{.Bool} },
        .filter_fields = .{ .exclude = &.{"field3"} },
    });

    try run.case(struct_1,
        \\
    , .{
        .filter_depths = .{ .include = &.{100} },
    });

    // ------------------------
    // Test optionals
    // ------------------------
    const opt_1: ???u8 = 42;

    try run.case(opt_1,
        \\???u8
        \\  ??u8
        \\    ?u8
        \\      u8
        \\        42
        \\
    , .{ .optional_skip_dup_unfold = false });

    const opt_2: ???u8 = null;
    try run.case(opt_2,
        \\???u8
        \\  null
        \\
    , .{ .optional_skip_dup_unfold = false });

    // ------------------------
    // Test pointers
    // ------------------------
    const ptr_1: *const *const u8 = &&42;

    try run.case(ptr_1,
        \\*const *const u8
        \\  *const u8
        \\    u8
        \\      42
        \\
    , .{ .ptr_skip_dup_unfold = false });

    try run.case(ptr_1,
        \\*const *const u8
        \\  42
        \\
    , .{ .ptr_skip_dup_unfold = true });

    const ptr_2: *const u8 = &42;

    try run.case(ptr_2,
        \\*const u8
        \\  42
        \\
    , .{ .ptr_skip_dup_unfold = true });

    // ------------------------
    // Test strings
    // ------------------------

    // ------------------------
    // Test union and enums
    // ------------------------
    // const case = union(enum) { a, b, c };
    // const case = @typeName(@TypeOf(Union.a));
    // const case: struct {
    //     f1: u32 = 42,
    //     f2: u32 = 42,
    // } = .{};
    // const case: []?*const u8 = null;

    // ------------------------
    // Test mixed
    // ------------------------

    try run.case(std.testing.allocator,
        \\mem.Allocator
        \\  .ptr: *anyopaque
        \\  .vtable: *const mem.Allocator.VTable
        \\    .alloc: *const fn (*anyopaque, usize, u8, usize) ?[*]u8
        \\    .resize: *const fn (*anyopaque, []u8, u8, usize, usize) bool
        \\    .free: *const fn (*anyopaque, []u8, u8, usize) void
        \\
    , .{});

    try run.case(std.testing.allocator,
        \\mem.Allocator
        \\  .ptr: *anyopaque
        \\  .vtable: *const mem.Allocator.VTable
        \\    mem.Allocator.VTable
        \\      .alloc: *const fn (*anyopaque, usize, u8, usize) ?[*]u8
        \\      .resize: *const fn (*anyopaque, []u8, u8, usize, usize) bool
        \\      .free: *const fn (*anyopaque, []u8, u8, usize) void
        \\
    , .{ .ptr_skip_dup_unfold = false });
}

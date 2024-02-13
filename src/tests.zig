const std = @import("std");
const pretty = @import("pretty.zig");

pub fn main() !void {
    const val = struct {
        a1: struct {
            a2: struct {} = .{},
            b2: struct {
                b3: struct {} = .{},
            } = .{},
            c2: struct {} = .{},
        } = .{},
        b1: struct {} = .{},
        c1: struct {
            a2: struct {} = .{},
        } = .{},
        d4: struct {} = .{},
    }{};

    const options: pretty.Options = comptime .{
        .depth_max = 4,
        .type_name_max_len = 20, // .first, .last, .smart
        .filter_depths = .{ .exclude = &.{ 1, 3 } },
        .filter_field_names = .{ .exclude = &.{"b1"} },

        .optional_skip_dup_unfold = true,
        // .ptr_skip_dup_unfold = false,
        // .val_on_same_line = true,
        .arr_show_item_idx = false,
        // .arr_show_item_types = false,
        // .show_types = false,
        .struct_show_empty = false,
        .struct_max_len = 4,
    };

    try pretty.print(std.heap.page_allocator, val, options);
}

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
    // Test arrays and slices
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

    try run.case(arr_1,
        \\[3]u8
        \\  0: 1
        \\  1: 2
        \\
    , .{
        .arr_show_item_idx = true,
        .arr_max_len = 2,
    });

    const slice_1: []const [2]u8 = &.{
        [_]u8{ 1, 2 },
        [_]u8{ 3, 4 },
    };

    try run.case(slice_1,
        \\[]const [2]u8
        \\  0: [2]u8
        \\    0: 1
        \\    1: 2
        \\  1: [2]u8
        \\    0: 3
        \\    1: 4
        \\
    , .{
        .arr_show_item_idx = true,
    });

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
        .filter_field_names = .{ .exclude = &.{"field3"} },
    });

    try run.case(struct_1,
        \\
    , .{
        .filter_depths = .{ .include = &.{100} },
    });

    const Struct_1 = struct {};

    try run.case(Struct_1{},
        \\tests.test_0.Struct_1
        \\
    , .{ .struct_show_empty = false });

    const Struct_2 = struct {
        field1: Struct_1 = .{},
        field2: Struct_1 = .{},
        field3: Struct_1 = .{},
    };

    try run.case(Struct_2{},
        \\tests.test_0.Struct_2
        \\  .field1: tests.test_0.Struct_1
        \\    null
        \\  .field2: tests.test_0.Struct_1
        \\    null
        \\  .field3: tests.test_0.Struct_1
        \\    null
        \\
    , .{ .struct_show_empty = true });

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

    try run.case(opt_1,
        \\???u8
        \\  42
        \\
    , .{ .optional_skip_dup_unfold = true });

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
    , .{
        .ptr_skip_dup_unfold = false,
        .ptr_deref = true,
    });

    try run.case(ptr_1,
        \\*const *const u8
        \\  42
        \\
    , .{
        .ptr_skip_dup_unfold = true,
        .ptr_deref = true,
    });

    const ptr_2: *const u8 = &42;

    try run.case(ptr_2,
        \\*const u8
        \\  42
        \\
    , .{
        .ptr_skip_dup_unfold = true,
        .ptr_deref = true,
    });

    // ------------------------
    // Test strings
    // ------------------------
    const str_1: []const u8 = "Hello pretty!";
    try run.case(str_1,
        \\[]const u8
        \\  "Hello pretty!"
        \\
    , .{ .ptr_skip_dup_unfold = true });

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
        \\    mem.Allocator.VTable
        \\      .alloc: *const fn (*anyopaque, usize, u8, usize) ?[*]u8
        \\      .resize: *const fn (*anyopaque, []u8, u8, usize, usize) bool
        \\      .free: *const fn (*anyopaque, []u8, u8, usize) void
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

const std = @import("std");
const pretty = @import("pretty.zig");

test {
    // if (true) return error.SkipZigTest;
    const case = struct {
        pub fn run(input: anytype, comptime expected: []const u8, comptime opt: pretty.Options) !void {
            const expect = expected ++ "\n"; // additional newline for opt.empty_line_at_end
            const actual = try pretty.dump(std.testing.allocator, input, opt);
            defer std.testing.allocator.free(actual);
            try std.testing.expectEqualStrings(expect, actual);
        }
        pub fn skip(input: anytype, comptime expected: []const u8, comptime opt: pretty.Options) !void {
            _ = opt;
            _ = expected;
            _ = input;
        }
    };

    // ------------------------
    // Primitives
    // ------------------------

    try case.run(42,
        \\(no output)
        \\
    , .{
        .filter_depths = .{ .include = &.{100} },
    });

    try case.run(42,
        \\(no output)
    , .{
        .empty_line_at_end = false,
        .filter_depths = .{ .include = &.{100} },
    });

    try case.run(@as(u8, 42),
        \\u8
        \\  42
        \\
    , .{});

    try case.run(@as(u8, 42),
        \\u8 = 42
        \\
    , .{
        .inline_mode = true,
    });

    // ------------------------
    // Optionals
    // ------------------------
    const opt_1: ???u8 = 42;

    try case.run(opt_1,
        \\???u8
        \\  ??u8
        \\    ?u8
        \\      u8
        \\        42
        \\
    , .{
        .optional_skip_dup_unfold = false,
    });

    try case.run(opt_1,
        \\???u8
        \\  42
        \\
    , .{
        .optional_skip_dup_unfold = true,
    });

    const opt_2: ???u8 = null;
    try case.run(opt_2,
        \\???u8
        \\  null
        \\
    , .{
        .optional_skip_dup_unfold = false,
    });

    // ------------------------
    // Arrays and slices
    // ------------------------
    const arr_1: [3]u8 = .{ 1, 2, 3 };

    try case.run(arr_1,
        \\[3]u8
        \\  1
        \\  2
        \\  3
        \\
    , .{
        .array_show_item_idx = false,
    });

    try case.run(arr_1,
        \\[3]u8
        \\  [0]: u8 => 1
        \\  [1]: u8 => 2
        \\  [2]: u8 => 3
        \\
    , .{
        .array_show_item_idx = true,
        .array_hide_prim_types = false,
    });

    try case.run(arr_1,
        \\[3]u8
        \\  [0]: 1
        \\  [1]: 2
        \\
    , .{
        .array_show_item_idx = true,
        .array_max_len = 2,
        .array_hide_prim_types = true,
    });

    const slice_1: []const [2]u8 = &.{
        [_]u8{ 1, 2 },
        [_]u8{ 3, 4 },
    };

    try case.run(slice_1,
        \\[]const [2]u8
        \\  [0]: [2]u8
        \\    [0]: 1
        \\    [1]: 2
        \\  [1]: [2]u8
        \\    [0]: 3
        \\    [1]: 4
        \\
    , .{
        .array_show_item_idx = true,
        .array_hide_prim_types = true,
    });

    // // ------------------------
    // // Structs
    // // ------------------------
    const Struct = struct {
        field1: bool = true,
        field2: u8 = 42,
        field3: f32 = 1.1,
    };
    const struct_1: Struct = .{};

    try case.run(struct_1,
        \\tests.test_0.Struct
        \\  .field1: bool => true
        \\  .field2: u8 => 42
        \\  .field3: f32 => 1.10000002e+00
        \\
    , .{});

    try case.run(struct_1,
        \\.field1: bool => true
        \\.field2: u8 => 42
        \\.field3: f32 => 1.10000002e+00
        \\
    , .{
        .filter_depths = .{ .exclude = &.{0} },
    });

    try case.run(struct_1,
        \\.field1: true
        \\.field2: 42
        \\.field3: 1.10000002e+00
        \\
    , .{
        .filter_depths = .{ .exclude = &.{0} },
        .show_type_names = false,
    });

    try case.run(struct_1,
        \\.field1:
        \\.field2:
        \\.field3:
        \\
    , .{
        .filter_depths = .{ .exclude = &.{0} },
        .show_type_names = false,
        .show_vals = false,
    });

    try case.run(struct_1,
        \\.field1: bool
        \\.field2: u8
        \\.field3: f32
        \\
    , .{
        .filter_depths = .{ .exclude = &.{0} },
        .show_vals = false,
    });

    try case.run(struct_1,
        \\.field1: bool
        \\.field2: u8
        \\.field3: f32
        \\
    , .{
        .filter_depths = .{ .exclude = &.{ 0, 2 } },
    });
    try case.run(struct_1,
        \\true
        \\42
        \\1.10000002e+00
        \\
    , .{
        .filter_depths = .{ .include = &.{2} },
    });

    try case.run(struct_1,
        \\tests.test_0.Struct
        \\  .field1: bool
        \\    true
        \\  .field2: u8
        \\    42
        \\  .field3: f32
        \\    1.10000002e+00
        \\
    , .{
        .struct_inline_prim_types = false,
    });

    try case.run(struct_1,
        \\tests.test_0.Struct{ .field1: bool = true, .field2: u8 = 42, .field3: f32 = 1.10000002e+00 }
        \\
    , .{
        .inline_mode = true,
    });

    try case.run(struct_1,
        \\.{ .field1:, .field2:, .field3: }
        \\
    , .{
        .inline_mode = true,
        .show_type_names = false,
        .show_vals = false,
    });

    try case.run(struct_1,
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

    try case.run(struct_1,
        \\.field2: u8 => 42
        \\
    , .{
        .filter_depths = .{ .exclude = &.{0} },
        .filter_field_type_tags = .{ .exclude = &.{ .Bool, .Float } },
    });

    try case.run(struct_1,
        \\.field2: u8 => 42
        \\
    , .{
        .filter_depths = .{ .exclude = &.{0} },
        .filter_field_type_tags = .{ .exclude = &.{.Bool} },
        .filter_field_names = .{ .exclude = &.{"field3"} },
    });

    const Struct_1 = struct {};
    const Struct_2 = struct {
        field1: Struct_1 = .{},
        field2: Struct_1 = .{},
        field3: Struct_1 = .{},
    };

    // try case.run(Struct_1{},
    //     \\tests.test_0.Struct_1
    //     \\
    // , .{ .struct_show_empty = false });

    try case.run(Struct_2{},
        \\tests.test_0.Struct_2
        \\  .field1: tests.test_0.Struct_1
        \\    (empty)
        \\  .field2: tests.test_0.Struct_1
        \\    (empty)
        \\  .field3: tests.test_0.Struct_1
        \\    (empty)
        \\
    , .{
        .struct_show_empty = true,
    });

    // ------------------------
    // Pointers
    // ------------------------
    const ptr_1: *const *const u8 = &&42;

    try case.run(ptr_1,
        \\*const *const u8
        \\  *const u8
        \\    u8
        \\      42
        \\
    , .{
        .ptr_skip_dup_unfold = false,
        .ptr_deref = true,
    });

    try case.run(ptr_1,
        \\*const *const u8
        \\  42
        \\
    , .{
        .ptr_skip_dup_unfold = true,
        .ptr_deref = true,
    });

    try case.run(ptr_1,
        \\*const *const u8 = 42
        \\
    , .{
        .inline_mode = true,
        .ptr_skip_dup_unfold = true,
        .ptr_deref = true,
    });

    // ------------------------
    // Strings
    // ------------------------
    const str_1: []const u8 = "pretty!";
    try case.run(str_1,
        \\[]const u8
        \\  "pretty!"
        \\
    , .{ .ptr_skip_dup_unfold = true });

    try case.run(str_1,
        \\[]const u8
        \\  [0]: 112
        \\  [1]: 114
        \\  [2]: 101
        \\  [3]: 116
        \\  [4]: 116
        \\  [5]: 121
        \\  [6]: 33
        \\
    , .{
        .ptr_skip_dup_unfold = true,
        .str_is_u8 = false,
    });

    // ------------------------
    // Union and enums
    // ------------------------
    // const case = union(enum) { a, b, c };
    // const case = @typeName(@TypeOf(Union.a));
    // const case: struct {
    //     f1: u32 = 42,
    //     f2: u32 = 42,
    // } = .{};
    // const case: []?*const u8 = null;

    // ------------------------
    // Mixed
    // ------------------------

    try case.run(@typeInfo(struct { f1: bool, f2: u8 }),
        \\builtin.Type
        \\  .Struct: builtin.Type.Struct
        \\    .layout: builtin.Type.ContainerLayout
        \\      .Auto
        \\    .backing_integer: ?type
        \\      null
        \\    .fields: []const builtin.Type.StructField
        \\      [0]: builtin.Type.StructField
        \\        .name: [:0]const u8
        \\          "f1"
        \\        .type: type
        \\          bool
        \\        .default_value: ?*const anyopaque
        \\          null
        \\        .is_comptime: bool => false
        \\        .alignment: comptime_int => 1
        \\      [1]: builtin.Type.StructField
        \\        .name: [:0]const u8
        \\          "f2"
        \\        .type: type
        \\          u8
        \\        .default_value: ?*const anyopaque
        \\          null
        \\        .is_comptime: bool => false
        \\        .alignment: comptime_int => 1
        \\    .decls: []const builtin.Type.Declaration
        \\      (empty)
        \\    .is_tuple: bool => false
        \\
    , .{
        .ptr_show_addr = false,
    });

    try case.run(std.testing.allocator,
        \\mem.Allocator
        \\  .ptr: *anyopaque
        \\  .vtable: *const mem.Allocator.VTable
        \\    .alloc: *const fn (*anyopaque, usize, u8, usize) ?[*]u8
        \\    .resize: *const fn (*anyopaque, []u8, u8, usize, usize) bool
        \\    .free: *const fn (*anyopaque, []u8, u8, usize) void
        \\
    , .{});

    try case.run(std.testing.allocator,
        \\mem.Allocator{ .ptr: *anyopaque, .vtable: *const mem.Allocator.VTable{ .alloc: *const fn (*anyopaque, usize, u8, usize) ?[*]u8, .resize: *const fn (*anyopaque, []u8, u8, usize, usize) bool, .free: *const fn (*anyopaque, []u8, u8, usize) void } }
        \\
    , .{
        .inline_mode = true,
    });
}

const std = @import("std");
const pretty = @import("pretty.zig");

test {
    const case = struct {
        pub fn run(input: anytype, comptime expected: []const u8, comptime opt: pretty.Options) !void {
            const actual = try pretty.dump(std.testing.allocator, input, opt);
            defer std.testing.allocator.free(actual);
            try std.testing.expectEqualStrings(expected, actual);
        }
        pub fn skip(input: anytype, comptime expected: []const u8, comptime opt: pretty.Options) !void {
            _ = opt;
            _ = expected;
            _ = input;
        }
    };

    // Tests are organized by type tags; within each tag, print options are
    // tested in applicable places. Inline mode is scattered throughout.
    // Search by .option_name to see how an option changes the resulting output.

    // ------------------------
    // Primitives
    // ------------------------

    try case.run(@as(u8, 42),
        \\u8
        \\  42
    , .{});

    // [[ .max_depth ]]
    try case.run(@as(u8, 42),
        \\u8
    , .{
        .max_depth = 1,
    });

    // [[ .tab_size ]]
    try case.run(@as(u8, 42),
        \\u8
        \\    42
    , .{
        .tab_size = 4,
    });

    try case.run(@as(u8, 42),
        \\u8
        \\ 42
    , .{
        .tab_size = 1,
    });

    // [[ .inline_mode ]]
    try case.run(@as(u8, 42),
        \\u8 = 42
    , .{
        .inline_mode = true,
    });

    // [[ .indicate_empty_output ]]
    try case.run(42,
        \\(empty output)
    , .{
        .indicate_empty_output = true,
        .filter_depths = .{ .include = &.{100} }, // simulate empty output
    });

    try case.run(42,
        \\
    , .{
        .indicate_empty_output = false,
        .filter_depths = .{ .include = &.{100} }, // simulate empty output
    });

    // [[ .fmt ]]
    try case.run(42,
        \\prepost
    , .{
        .fmt = "pre{s}post",
        .indicate_empty_output = false,
        .filter_depths = .{ .include = &.{100} }, // simulate empty output
    });

    // [[ .fmt ]]
    try case.run(42,
        \\pre(empty output)post
    , .{
        .fmt = "pre{s}post",
        .filter_depths = .{ .include = &.{100} }, // simulate empty output
    });

    // ------------------------
    // Optionals
    // ------------------------

    // [[ .optional_skip_dup_unfold ]]
    try case.run(@as(???u8, 42),
        \\???u8
        \\  ??u8
        \\    ?u8
        \\      u8
        \\        42
    , .{
        .optional_skip_dup_unfold = false,
    });

    try case.run(@as(???u8, 42),
        \\???u8
        \\  42
    , .{
        .optional_skip_dup_unfold = true,
    });

    try case.run(@as(???u8, null),
        \\???u8
        \\  null
    , .{
        .optional_skip_dup_unfold = false,
    });

    // [[ .max_depth ]]
    try case.run(@as(???u8, 42),
        \\???u8
        \\  ??u8
        \\    ?u8
        \\      u8
    , .{
        .max_depth = 4,
        .optional_skip_dup_unfold = false,
    });

    // [[ .inline_mode ]]
    try case.run(@as(???u8, null),
        \\???u8 = null
    , .{
        .inline_mode = true,
    });

    // ------------------------
    // Arrays
    // ------------------------

    // [[ .array_show_item_idx ]]
    try case.run([3]u8{ 1, 2, 3 },
        \\[3]u8
        \\  [0]: 1
        \\  [1]: 2
        \\  [2]: 3
    , .{
        .array_show_item_idx = true,
    });

    try case.run([3]u8{ 1, 2, 3 },
        \\[3]u8
        \\  1
        \\  2
        \\  3
    , .{
        .array_show_item_idx = false,
    });

    // [[ .array_show_prim_type_info ]]
    try case.run([3]u8{ 1, 2, 3 },
        \\[3]u8
        \\  [0]: u8 = 1
        \\  [1]: u8 = 2
        \\  [2]: u8 = 3
    , .{
        .array_show_item_idx = true,
        .array_show_prim_type_info = true,
    });

    try case.run([3]u8{ 1, 2, 3 },
        \\[3]u8
        \\  [0]: 1
        \\  [1]: 2
        \\  [2]: 3
    , .{
        .array_show_item_idx = true,
        .array_show_prim_type_info = false,
    });

    // [[ .show_type_names ]]
    try case.run([3]u8{ 1, 2, 3 },
        \\1
        \\2
        \\3
    , .{
        .show_type_names = false,
        .array_show_item_idx = false,
    });

    // ------------------------
    // Slices
    // ------------------------
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
    , .{
        .array_show_item_idx = true,
        .array_show_prim_type_info = false,
    });

    try case.run(slice_1,
        \\[]const [2]u8{ [0]: [2]u8{ [0]: 1, [1]: 2 }, [1]: [2]u8{ [0]: 3, [1]: 4 } }
    , .{
        .array_show_item_idx = true,
        .array_show_prim_type_info = false,
        .inline_mode = true,
    });

    // ------------------------
    // Structs
    // ------------------------
    const Struct = struct {
        field1: bool = true,
        field2: u8 = 42,
        field3: f32 = 1.1,
    };
    const struct_1: Struct = .{};

    try case.run(struct_1,
        \\tests.test_0.Struct
        \\  .field1: bool = true
        \\  .field2: u8 = 42
        \\  .field3: f32 = 1.1e0
    , .{});

    try case.run(struct_1,
        \\.field1: bool = true
        \\.field2: u8 = 42
        \\.field3: f32 = 1.1e0
    , .{
        .filter_depths = .{ .exclude = &.{0} },
    });

    try case.run(struct_1,
        \\.field1: true
        \\.field2: 42
        \\.field3: 1.1e0
    , .{
        .filter_depths = .{ .exclude = &.{0} },
        .show_type_names = false,
    });

    try case.run(struct_1,
        \\.field1:
        \\.field2:
        \\.field3:
    , .{
        .filter_depths = .{ .exclude = &.{0} },
        .show_type_names = false,
        .show_vals = false,
    });

    try case.run(struct_1,
        \\.field1: bool
        \\.field2: u8
        \\.field3: f32
    , .{
        .filter_depths = .{ .exclude = &.{0} },
        .show_vals = false,
    });

    try case.run(struct_1,
        \\.field1: bool
        \\.field2: u8
        \\.field3: f32
    , .{
        .filter_depths = .{ .exclude = &.{ 0, 2 } },
    });

    try case.run(struct_1,
        \\true
        \\42
        \\1.1e0
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
        \\    1.1e0
    , .{
        .struct_inline_prim_types = false,
    });

    try case.run(struct_1,
        \\tests.test_0.Struct{ .field1: bool = true, .field2: u8 = 42, .field3: f32 = 1.1e0 }
    , .{
        .inline_mode = true,
    });

    try case.run(struct_1,
        \\.{ .field1: true, .field2: 42, .field3: 1.1e0 }
    , .{
        .inline_mode = true,
        .show_type_names = false,
    });

    try case.run(struct_1,
        \\.{ .field1:, .field2:, .field3: }
    , .{
        .inline_mode = true,
        .show_type_names = false,
        .show_vals = false,
    });

    try case.run(struct_1,
        \\[Struct] .{ .field1: [Bool], .field2: [Int], .field3: [Float] }
    , .{
        .inline_mode = true,
        .show_type_names = false,
        .show_type_tags = true,
        .show_vals = false,
    });

    try case.run(struct_1,
        \\.field1: [Bool]
        \\.field2: [Int]
        \\.field3: [Float]
    , .{
        .filter_depths = .{ .exclude = &.{0} },
        .show_type_tags = true,
        .show_type_names = false,
        .show_vals = false,
    });

    try case.run(struct_1,
        \\.field2: u8 = 42
    , .{
        .filter_depths = .{ .exclude = &.{0} },
        .filter_field_type_tags = .{ .exclude = &.{ .Bool, .Float } },
    });

    try case.run(struct_1,
        \\.field2: u8 = 42
    , .{
        .filter_depths = .{ .exclude = &.{0} },
        .filter_field_type_tags = .{ .exclude = &.{.Bool} },
        .filter_field_names = .{ .exclude = &.{"field3"} },
    });

    const Struct_1 = struct {};

    try case.run(Struct_1{},
        \\tests.test_0.Struct_1
    , .{
        .struct_show_empty = false,
    });

    const Struct_2 = struct {
        field1: Struct_1 = .{},
        field2: Struct_1 = .{},
    };

    try case.run(Struct_2{},
        \\tests.test_0.Struct_2
        \\  .field1: tests.test_0.Struct_1
        \\    (empty)
        \\  .field2: tests.test_0.Struct_1
        \\    (empty)
    , .{
        .struct_show_empty = true,
    });

    try case.run(Struct_2{},
        \\tests.test_0.Struct_2
        \\  .field1: tests.test_0.Struct_1
        \\  .field2: tests.test_0.Struct_1
    , .{
        .struct_show_empty = false,
    });

    try case.run(Struct_2{},
        \\tests.test_0.Struct_2{ .field1: tests.test_0.Struct_1, .field2: tests.test_0.Struct_1 }
    , .{
        .struct_show_empty = false,
        .inline_mode = true,
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
    , .{
        .ptr_skip_dup_unfold = false,
        .ptr_deref = true,
    });

    try case.run(ptr_1,
        \\*const *const u8
        \\  42
    , .{
        .ptr_skip_dup_unfold = true,
        .ptr_deref = true,
    });

    try case.run(ptr_1,
        \\*const *const u8 = 42
    , .{
        .inline_mode = true,
        .ptr_skip_dup_unfold = true,
        .ptr_deref = true,
    });

    // ------------------------
    // Primitives
    // ------------------------

    try case.run(@as(u21, 'λ'),
        \\u21
        \\  'λ'
    , .{
        .u21_is_codepoint = true,
    });

    try case.run(@as(u21, 'λ'),
        \\u21
        \\  955
    , .{
        .u21_is_codepoint = false,
    });

    // Test that codepoints over maximum are handled
    // without a panic
    try case.run(@as(u21, 0x110000),
        \\u21
        \\  '�'
    , .{
        .u21_is_codepoint = true,
    });

    // ------------------------
    // Strings
    // ------------------------

    // [[ .array_u8z_is_str ]]
    try case.run("pretty",
        \\*const [6:0]u8
        \\  "pretty"
    , .{
        .array_u8z_is_str = true,
    });

    try case.run("pretty",
        \\*const [6:0]u8
        \\  [0]: 112
        \\  [1]: 114
        \\  [2]: 101
        \\  [3]: 116
        \\  [4]: 116
        \\  [5]: 121
    , .{
        .array_u8z_is_str = false,
    });

    // [[ .slice_u8z_is_str ]]
    try case.run(@as([:0]const u8, "pretty"),
        \\[:0]const u8
        \\  "pretty"
    , .{
        .slice_u8z_is_str = true,
    });

    try case.run(@as([:0]const u8, "pretty"),
        \\[:0]const u8
        \\  [0]: 112
        \\  [1]: 114
        \\  [2]: 101
        \\  [3]: 116
        \\  [4]: 116
        \\  [5]: 121
    , .{
        .slice_u8z_is_str = false,
    });

    // [[ .array_u8_is_str ]]
    try case.run(@as([6]u8, [_]u8{ 'p', 'r', 'e', 't', 't', 'y' }),
        \\[6]u8
        \\  "pretty"
    , .{
        .array_u8_is_str = true,
    });

    try case.run(@as([6]u8, [_]u8{ 'p', 'r', 'e', 't', 't', 'y' }),
        \\[6]u8
        \\  [0]: 112
        \\  [1]: 114
        \\  [2]: 101
        \\  [3]: 116
        \\  [4]: 116
        \\  [5]: 121
    , .{
        .array_u8_is_str = false,
    });

    // [[ .ptr_many_u8z_is_str ]]
    try case.run(@as([*:0]const u8, "pretty"),
        \\[*:0]const u8
        \\  "pretty"
    , .{
        .ptr_many_u8z_is_str = true,
    });

    try case.run(@as([*:0]const u8, "pretty"),
        \\[*:0]const u8
        \\  [0]: 112
        \\  [1]: 114
        \\  [2]: 101
        \\  [3]: 116
        \\  [4]: 116
        \\  [5]: 121
    , .{
        .ptr_many_u8z_is_str = false,
    });

    // [[ .ptr_many_with_sentinel_is_array ]]
    try case.run(@as([*:0.125]const f32, &[_:0.125]f32{ 1.1, 1.2, 1.3 }),
        \\[*:0.125]const f32
        \\  [0]: 1.1e0
        \\  [1]: 1.2e0
        \\  [2]: 1.3e0
    , .{
        .ptr_many_with_sentinel_is_array = true,
    });

    try case.run(@as([*:0.125]const f32, &[_:0.125]f32{ 1.1, 1.2, 1.3 }),
        \\[*:0.125]const f32
        \\  ?
    , .{
        .ptr_many_with_sentinel_is_array = false,
    });

    // [[ .slice_u8_is_str ]]
    try case.run(@as([]const u8, "pretty"),
        \\[]const u8
        \\  "pretty"
    , .{});

    try case.run(@as([]const u8, "pretty"),
        \\[]const u8
        \\  [0]: 112
        \\  [1]: 114
        \\  [2]: 101
        \\  [3]: 116
        \\  [4]: 116
        \\  [5]: 121
    , .{
        .slice_u8_is_str = false,
    });

    // [[ .str_max_len ]]
    try case.run("pretty",
        \\*const [6:0]u8
        \\  "pretty"
    , .{ .str_max_len = 0 });

    try case.run("pretty",
        \\*const [6:0]u8
        \\  "p.."
    , .{ .str_max_len = 1 });

    // [[ .inline_mode ]]
    try case.run("pretty",
        \\*const [6:0]u8 = "pretty"
    , .{
        .inline_mode = true,
    });

    // ------------------------
    // Union and enums
    // ------------------------
    const Enum = enum { a, b, c };
    try case.run(@as(Enum, .a),
        \\tests.test_0.Enum
        \\  .a
    , .{});

    // untagged union
    const Union_1 = union { a: u8, b: u8, c: u8 };
    try case.run(@as(Union_1, .{ .a = 42 }),
        \\tests.test_0.Union_1
        \\  ?
    , .{});

    try case.run(@as(Union_1, .{ .a = 42 }),
        \\tests.test_0.Union_1 = ?
    , .{
        .inline_mode = true,
    });

    // tagged union
    const Union_2 = union(enum) { a: u8, b: u8, c: u8 };
    try case.run(@as(Union_2, .{ .a = 42 }),
        \\tests.test_0.Union_2
        \\  .a: u8 = 42
    , .{});

    try case.run(@as(Union_2, .{ .a = 42 }),
        \\tests.test_0.Union_2{ .a: u8 = 42 }
    , .{
        .inline_mode = true,
    });

    // recursive tagged union
    const Union_4 = union(enum) { a: u8, b: *const @This() };
    try case.run(@as(Union_4, .{ .a = 42 }),
        \\tests.test_0.Union_4
        \\  .a: u8 = 42
    , .{});

    try case.run(@as(Union_4, .{ .a = 42 }),
        \\tests.test_0.Union_4{ .a: u8 = 42 }
    , .{
        .inline_mode = true,
    });

    // pure tagged union
    const Union_3 = union(enum) { a, b, c };
    try case.run(@as(Union_3, .a),
        \\tests.test_0.Union_3
        \\  .a: void = void
    , .{});

    try case.run(@as(Union_3, .a),
        \\tests.test_0.Union_3{ .a: void = void }
    , .{
        .inline_mode = true,
    });

    // ------------------------
    // Types
    // ------------------------
    const Type_1 = union(enum) { a, b, c };
    // const case = @typeName(@TypeOf(Union.a));

    try case.run(Type_1.a,
        \\@typeInfo(tests.test_0.Type_1).Union.tag_type.?
        \\  .a
    , .{});

    // ------------------------
    // Mixed
    // ------------------------

    try case.run(@typeInfo(struct { f1: bool, f2: u8 }),
        \\builtin.Type
        \\  .Struct: builtin.Type.Struct
        \\    .layout: builtin.Type.ContainerLayout
        \\      .auto
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
        \\        .is_comptime: bool = false
        \\        .alignment: comptime_int = 1
        \\      [1]: builtin.Type.StructField
        \\        .name: [:0]const u8
        \\          "f2"
        \\        .type: type
        \\          u8
        \\        .default_value: ?*const anyopaque
        \\          null
        \\        .is_comptime: bool = false
        \\        .alignment: comptime_int = 1
        \\    .decls: []const builtin.Type.Declaration
        \\      (empty)
        \\    .is_tuple: bool = false
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
    , .{});

    try case.run(std.testing.allocator,
        \\mem.Allocator{ .ptr: *anyopaque, .vtable: *const mem.Allocator.VTable{ .alloc: *const fn (*anyopaque, usize, u8, usize) ?[*]u8, .resize: *const fn (*anyopaque, []u8, u8, usize, usize) bool, .free: *const fn (*anyopaque, []u8, u8, usize) void } }
    , .{
        .inline_mode = true,
    });
}

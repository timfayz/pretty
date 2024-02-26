// MIT License (c) Timur Fayzrakhmanov.
// tim.fayzrakhmanov@gmail.com (github.com/timfayz)

const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const meta = std.meta;

/// pretty's formatting options.
pub const Options = struct {

    // [Generic printing options]

    /// Limit the depth.
    max_depth: u8 = 10,
    /// Specify depths to include or exclude from the output.
    filter_depths: Filter(usize) = .{ .exclude = &.{} },
    /// Indentation size for depth levels.
    tab_size: u8 = 2,
    /// Add an empty line at the end of the output (to separate several prints).
    empty_line_at_end: bool = true,
    /// Single line printing mode.
    inline_mode: bool = false,
    /// Sign used to indicate skipping.
    skip_sign: []const u8 = "..",

    // [Generic type printing options]

    /// Display type fields (structs and unions only).
    show_field_names: bool = true,
    /// Display type tags.
    show_type_tags: bool = false,
    /// Display type names.
    show_type_names: bool = true,
    /// Limit the length of type names.
    type_name_max_len: usize = 60,
    /// Fold brackets in type names (with '..').
    type_name_fold_brackets: bool = true,
    /// Do not fold brackets for function signatures.
    type_name_fold_except_fn: bool = true,

    // [Generic value printing options]

    /// Display values.
    show_vals: bool = true,
    /// Display empty values.
    show_empty_vals: bool = true,

    // [Pointer printing options]

    /// Follow pointers instead of printing their address.
    ptr_deref: bool = true,
    /// Reduce duplicating depths when dereferencing pointers.
    ptr_skip_dup_unfold: bool = true,

    // [Optional printing options]

    /// Reduce duplicating depths when unfolding optional types.
    optional_skip_dup_unfold: bool = true,

    // [Struct printing options]

    /// Treat empty structs as having a 'null' value.
    struct_show_empty: bool = true,
    /// Limit the number of fields in the output.
    struct_max_len: usize = 10,
    /// Specify field names to include or exclude from the output.
    filter_field_names: Filter([]const u8) = .{ .exclude = &.{} },
    /// Specify field type tags to include or exclude from the output.
    filter_field_type_tags: Filter(std.builtin.TypeId) = .{ .exclude = &.{} },
    /// Specify field types to include or exclude from the output.
    filter_field_types: Filter(type) = .{ .exclude = &.{} },

    // [Array and slice printing options]

    /// Limit the number of items in the output.
    arr_max_len: usize = 20,
    /// Display item indices.
    arr_show_item_idx: bool = true,
    /// Display values of primitive types on the same line as the index.
    arr_inline_prim_types: bool = true,
    /// Specify types to treat as primitives.
    arr_prim_types: Filter(std.builtin.TypeId) = .{ .include = &.{
        .Int,
        .ComptimeInt,
        .Float,
        .ComptimeFloat,
        .Void,
        .Bool,
    } },

    // [String printing options]

    /// Treat `[]u8` as "string".
    str_is_u8: bool = true,
    /// Limit the length of strings.
    str_max_len: usize = 80,

    // TODO
    // solo mode for a certain field (recursive)
    // inline if struct length is <= 2
    // inline_unions = true
    // inline_small_struct = true
    // small_struct_size = 3
    show_tree_lines: bool = false, // '├' // '─' // '│' // '└'
    // show_colors
};

/// Generates a pretty formatted string and returns it as std.ArrayList interface.
pub fn dumpAsList(allocator: Allocator, value: anytype, comptime options: Options) !ArrayList(u8) {
    var printer = Pretty(options).init(allocator);
    try printer.traverse(value, .{});

    // [Option] Append an empty line at the end
    if (options.empty_line_at_end)
        try printer.out.append('\n');

    return printer.out;
}

/// Generates a pretty formatted string and returns it as a []u8 slice.
pub fn dump(allocator: Allocator, value: anytype, comptime options: Options) ![]u8 {
    var list = try dumpAsList(allocator, value, options);
    return list.toOwnedSlice();
}

/// Prints pretty formatted string for an arbitrary input value.
pub fn print(alloc: Allocator, value: anytype, comptime options: Options) !void {
    const out = try dump(alloc, value, options);
    defer alloc.free(out);
    std.debug.print("{s}", .{out});
}

/// pretty implementation structure.
fn Pretty(options: Options) type {
    comptime {
        if (options.tab_size < 1) @compileError(".tab_size cannot be less than 1.");
    }
    return struct {
        alloc: Allocator,
        out: ArrayList(u8),

        /// Runtime information between recursive calls.
        idx: usize = 0,
        last_op: enum { Indent, Text, Newline } = .Newline,

        /// Comptime options.
        const opt: Options = options;

        /// Comptime information between recursive calls.
        const Ctx = struct {
            depth: usize = 0,
            depth_skip: usize = 0,
            field: []const u8 = "",
            prev: ?type = null,
            last: bool = false,

            fn incDepth(c: Ctx) Ctx {
                var upd = c;
                upd.depth = c.depth + 1;
                return upd;
            }

            fn decDepth(c: Ctx) Ctx {
                var upd = c;
                upd.depth = c.depth -| 1;
                return upd;
            }

            fn incSkipDepth(c: Ctx) Ctx {
                var upd = c;
                upd.depth_skip = c.depth_skip + 1;
                return upd;
            }

            fn getDepth(c: Ctx) usize {
                return c.depth -| c.depth_skip;
            }

            fn setField(c: Ctx, comptime field: []const u8) Ctx {
                var upd = c;
                upd.field = field;
                return upd;
            }

            fn hasField(c: Ctx) bool {
                return c.field.len != 0;
            }

            fn setLast(c: Ctx, comptime last: bool) Ctx {
                var upd = c;
                upd.last = last;
                return upd;
            }

            fn setPrev(c: Ctx, comptime prev: ?type) Ctx {
                var upd = c;
                upd.prev = prev;
                return upd;
            }

            fn prevIs(c: Ctx, comptime tag: std.builtin.TypeId) bool {
                if (c.prev == null) return false;
                return typeTagIs(c.prev.?, tag);
            }

            fn prevPtrIsSlice(c: Ctx) bool {
                if (c.prev == null) return false;
                return typeIsSlicePtr(c.prev.?);
            }
        };

        const Self = @This();

        pub fn init(alloc: Allocator) Self {
            return Self{
                .alloc = alloc,
                .out = ArrayList(u8).init(alloc),
            };
        }

        pub fn deinit(self: *Self) void {
            self.out.deinit();
        }

        fn fmtTypeName(comptime T: type) []const u8 {
            var name: []const u8 = @typeName(T);

            // [Option] Shorten type name by folding brackets in it
            if (opt.type_name_fold_brackets) {
                var level = 1;

                // [Option] Except function signatures
                if (opt.type_name_fold_except_fn and typeIsFnPtr(T)) {
                    level = 2;
                }

                // [Option]-less If type name starts with '@TypeOf(..)'
                // (a rare case, usually primitives)
                else if (name.len != 0 and name[0] == '@') {
                    level = 2;
                }

                name = strFoldBracketsCt(name, .{
                    .fold_depth = level,
                    .bracket = .Round,
                });
            }

            // [Option] Cut the type name if exceeds the length
            if (opt.type_name_max_len != 0 and name.len > opt.type_name_max_len) {
                name = name[0..opt.type_name_max_len] ++ "..";
            }

            return name;
        }

        fn fmtEnumValue(alloc: Allocator, val: anytype) ![]const u8 {
            const T = @TypeOf(val);
            // `.Name` for exhaustive and named enums
            if (std.enums.tagName(T, val)) |tag_name| {
                return std.fmt.allocPrint(alloc, ".{s}", .{tag_name});
            }
            // `enum_type_name(integer)` for non-exhaustive and unnamed enums
            else {
                const tag_type = @typeInfo(T).Enum.tag_type;
                return std.fmt.allocPrint(alloc, "{s}({d})", .{ @typeName(tag_type), @intFromEnum(val) });
            }
        }

        // Append primitives

        fn appendIndent(self: *Self, comptime ctx: Ctx) !void {
            // [Option] Inline mode needs no indentations
            if (opt.inline_mode)
                return;

            //  Indent only if not already indented or after a newline
            if (self.last_op == .Indent or self.last_op != .Newline)
                return;

            // [Option] Show tree lines
            if (opt.show_tree_lines) {
                if (ctx.getDepth() > 0) {
                    try self.out.appendSlice(if (ctx.last) "└" else "├");
                    for (0..(ctx.getDepth() * opt.tab_size) - 1) |_| {
                        try self.out.appendSlice("─");
                    }
                }
            } else {
                try self.out.appendNTimes(' ', ctx.getDepth() * opt.tab_size);
            }

            self.last_op = .Indent;
        }

        fn appendText(self: *Self, text: []const u8, comptime ctx: Ctx) !void {
            // If text block is first after newline
            if (self.last_op == .Newline) {
                try self.appendIndent(ctx);
            }
            // If text block is after another
            else if (self.last_op == .Text) {
                try self.out.appendSlice(" ");
            }

            try self.out.appendSlice(text);
            self.last_op = .Text;
        }

        fn appendNewline(self: *Self) !void {
            if (self.last_op == .Indent or self.last_op == .Newline) return;
            try self.out.append('\n');
            self.last_op = .Newline;
        }

        // Append derivatives

        fn appendIndex(self: *Self, comptime ctx: Ctx) !void {
            const index = try std.fmt.allocPrint(self.alloc, "{d}:", .{self.idx});
            defer self.alloc.free(index);
            try self.appendText(index, ctx);
        }

        fn appendType(self: *Self, val: anytype, comptime ctx: Ctx) !void {
            // [Option] Show field name (if available)
            if (opt.show_field_names and ctx.hasField()) {
                const field = try std.fmt.allocPrint(self.alloc, ".{s}:", .{ctx.field});
                defer self.alloc.free(field);
                try self.appendText(field, ctx);
            }

            // [Option] Show type tag
            if (opt.show_type_tags) {
                const tag_name = @tagName(@typeInfo(@TypeOf(val)));
                const tag = try strEmbraceWith(self.alloc, tag_name, "[", "]");
                defer tag.deinit();
                try self.appendText(tag.items, ctx);
            }

            // [Option] Show type name
            if (opt.show_type_names) {
                const type_name = comptime fmtTypeName(@TypeOf(val));
                try self.appendText(type_name, ctx);
            }
        }

        fn appendValue(self: *Self, str_val: []const u8, comptime ctx: Ctx) !void {
            // [Option] Show value
            if (!opt.show_vals)
                return;

            // [Option] Stop if depth exceeds
            if (opt.max_depth > 0 and ctx.depth > opt.max_depth)
                return;

            // [Option] Skip if depth is not included
            if (!opt.filter_depths.includes(ctx.depth))
                return;

            try self.appendText(str_val, ctx);

            // [Option] Add a proper separator for single line mode
            if (opt.inline_mode) { // TODO if (ctx.inline_mode) instead
                try self.out.appendSlice(if (ctx.last) " " else ", ");
            } else {
                try self.appendNewline();
            }
        }

        fn appendValuePredefined(self: *Self, comptime tag: enum { Skip, Empty, Null, Unknown }, comptime ctx: Ctx) !void {
            const c = ctx.setLast(true);
            switch (tag) {
                .Skip => {
                    // [Option]-less Show skip sign
                    try self.appendValue(opt.skip_sign, c);
                },
                .Null => {
                    // [Option]-less
                    try self.appendValue("null", c);
                },
                .Empty => {
                    // [Option] Show empty values
                    if (opt.show_empty_vals)
                        try self.appendValue("(empty)", c);
                },
                .Unknown => {
                    // [Option]-less
                    try self.appendValue("?", c);
                },
            }
        }

        fn appendValueString(self: *Self, str: []const u8, comptime ctx: Ctx) !void {
            // [Option] Trim string if exceeds
            const trimmed = try strTrim(self.alloc, str, opt.str_max_len, "..", .Auto);
            defer trimmed.deinit();

            // [Option]-less Embrace with double quotes
            const quoted = try strEmbraceWith(self.alloc, trimmed.items, "\"", "\"");
            defer quoted.deinit();

            try self.appendValue(quoted.items, ctx);
        }

        fn appendValueFmt(self: *Self, comptime fmt: []const u8, val: anytype, comptime ctx: Ctx) !void {
            const any = try std.fmt.allocPrint(self.alloc, fmt, .{val});
            defer self.alloc.free(any);
            try self.appendValue(any, ctx);
        }

        fn appendValueType(self: *Self, comptime T: type, comptime ctx: Ctx) !void {
            // [Option]-less Fold brackets in type names at the second level.
            const type_name = comptime strFoldBracketsCt(@typeName(T), .{
                .fold_depth = 1,
                .bracket = .Any,
            });
            try self.appendValue(type_name, ctx);
        }

        // Traverse primitives

        fn traverse(self: *Self, val: anytype, comptime ctx: Ctx) !void {
            const T = @TypeOf(val);
            comptime var c = ctx;

            // [Option] Stop if depth exceeds
            if (opt.max_depth > 0 and c.depth > opt.max_depth)
                return;

            // [Option] If depth is filtered
            if (comptime !opt.filter_depths.includes(c.depth)) {
                c = c.incSkipDepth(); // adjust indentation
            } else {
                // Within struct or union
                if (comptime c.prevIs(.Struct) or c.prevIs(.Union)) {
                    try self.appendType(val, c);
                    try self.appendNewline();
                }
                // Within array or slice
                else if (comptime c.prevIs(.Array) or c.prevPtrIsSlice()) {

                    // [Option] Show item indices
                    if (opt.arr_show_item_idx) {
                        try self.appendIndex(c);
                    }

                    // [Option] Show primitive types on the same line as index
                    if (comptime opt.arr_prim_types.includes(typeTag(T))) {
                        c = c.decDepth();
                        if (!opt.arr_inline_prim_types)
                            // c = c.setInlineMode();
                            try self.appendType(val, c);
                    } else {
                        try self.appendType(val, c);
                        try self.appendNewline();
                    }
                }
                // Within pointer
                else if (comptime c.prevIs(.Pointer)) {
                    // [Option] Reduce dereferencing (unless struct or array?)
                    if (opt.ptr_skip_dup_unfold) { // and comptime !(typeTagIs(T, .Struct) or typeTagIs(T, .Array))
                        c = c.decDepth();
                    } else {
                        try self.appendType(val, c);
                        try self.appendNewline();
                    }
                }
                // Within optional
                else if (comptime c.prevIs(.Optional)) {
                    // [Option] Skip duplicate unfolding
                    if (opt.optional_skip_dup_unfold) {
                        c = c.decDepth();
                    } else {
                        try self.appendType(val, c);
                        try self.appendNewline();
                    }
                }
                // Any other
                else {
                    try self.appendType(val, c);
                    try self.appendNewline();
                }
            }

            c = c.setPrev(T).incDepth().setField(""); // update context for next recursion

            switch (@typeInfo(T)) {
                // Recursive
                .Pointer => {
                    switch (@typeInfo(T).Pointer.size) {
                        .One => {
                            // [Option]-less Do not show opaque or function pointers
                            if (meta.Child(T) == anyopaque or
                                @typeInfo(meta.Child(T)) == .Fn)
                                return;

                            // [Option] Follow the pointer
                            if (opt.ptr_deref) {
                                try self.traverse(val.*, c);
                            } else {
                                try self.appendValueFmt("{*}", val, c);
                            }
                        },
                        .Many, .C => {
                            // TODO support [*:0]u8 as string
                            try self.appendValuePredefined(.Unknown, c);
                        },
                        .Slice => {
                            // Slice is empty
                            if (val.len == 0) {
                                try self.appendValuePredefined(.Empty, c.setLast(true));
                                return;
                            }

                            // Slice has a single element
                            if (val.len == 1) {
                                // Re-interpret it as a single-item pointer
                                try self.traverse(val[0], c.setPrev(@TypeOf(&val[0])));
                                return;
                            }

                            // [Option] Interpret slice []u8 as string
                            if (opt.str_is_u8 and meta.Child(T) == u8) {
                                try self.appendValueString(val, c.setLast(true));
                                return;
                            }

                            // Slice is comptime-known
                            if (isComptime(val)) {
                                inline for (val, 1..) |item, len| {
                                    // [Option] Stop if the length of a slice exceeds
                                    if (opt.arr_max_len != 0 and len > opt.arr_max_len)
                                        break;

                                    self.idx = len - 1;
                                    try self.traverse(item, c);
                                }
                            }
                            // Slice is runtime
                            else {
                                for (val, 1..) |item, len| {
                                    // [Option] Stop if the length of a slice exceeds
                                    if (opt.arr_max_len != 0 and len > opt.arr_max_len)
                                        break;

                                    self.idx = len - 1;
                                    try self.traverse(item, c);
                                }
                            }
                        },
                    }
                },
                .Struct => {
                    // [Option] Show empty struct as empty value
                    if (meta.fields(T).len == 0 and opt.struct_show_empty) {
                        try self.appendValuePredefined(.Empty, c.setLast(true));
                        return;
                    }

                    // Struct has fields
                    inline for (meta.fields(T), 1..) |field, len| {
                        // [Option] If field type tag should be ignored
                        if (comptime !opt.filter_field_type_tags.includes(typeTag(field.type)))
                            continue;

                        // [Option] If field type should be ignored
                        if (comptime !opt.filter_field_types.includes(field.type))
                            continue;

                        // [Option] If field name should be ignored
                        if (comptime !opt.filter_field_names.includes(field.name))
                            continue;

                        // [Option] If the number of struct fields exceeds
                        if (opt.struct_max_len != 0 and len > opt.struct_max_len) {
                            try self.appendValuePredefined(.Skip, c.setLast());
                            break;
                        }

                        // Pass field value
                        const field_value = @field(val, field.name);
                        c = c.setField(field.name);

                        // Past last status
                        if (len == meta.fields(T).len)
                            c = c.setLast(true);

                        try self.traverse(field_value, c);
                    }
                },
                .Array => {
                    // String
                    if (opt.str_is_u8 and
                        meta.Child(T) == u8 and meta.sentinel(T) != null and meta.sentinel(T) == 0)
                    {
                        try self.appendValueString(val, c.setLast(true));
                        return;
                    }

                    // Other
                    inline for (val, 1..) |item, len| {
                        // [Option] Stop if the length of an array exceeds
                        if (opt.arr_max_len != 0 and len > opt.arr_max_len)
                            break;

                        self.idx = len - 1;
                        try self.traverse(item, c);
                    }
                },
                .Optional => {
                    // Optional has payload
                    if (val) |unwrapped| {
                        try self.traverse(unwrapped, c.setLast(true));
                        return;
                    }

                    // Optional is null
                    try self.appendValuePredefined(.Null, c.setLast(true));
                },
                // // Non-recursive
                // .Type => try self.appendValueType(val, c),
                .Enum => try self.traverseEnum(val, c),
                .Union => try self.traverseUnion(val, c),
                else => {
                    // Fall back to standard "{any}" formatter if it's a
                    // primitive or unsupported value
                    try self.appendValueFmt("{any}", val, c);
                },
            }
        }

        fn traverseUnion(self: *Self, val: anytype, comptime ctx: Ctx) !void {
            const T = @TypeOf(val);

            // Tagged union
            if (@typeInfo(T).Union.tag_type) |tag_type| {
                switch (@as(tag_type, val)) {
                    inline else => |tag| {
                        try self.traverse(@field(val, @tagName(tag)), ctx.setField(@tagName(tag)));
                    },
                }
                return;
            }

            // Normal one
            try self.appendValuePredefined(.Unknown, ctx);
        }

        fn traverseEnum(self: *Self, val: anytype, comptime ctx: Ctx) !void {
            const enum_name = try fmtEnumValue(self.alloc, val);
            defer self.alloc.free(enum_name);
            try self.appendValue(enum_name, ctx);
        }
    };
}

/// An interface that allows checking the inclusion or exclusion of items of
/// a specified type.
fn Filter(T: type) type {
    return union(enum) {
        include: []const T,
        exclude: []const T,
        fn includes(self: @This(), item: T) bool {
            switch (self) {
                .include => {
                    for (self.include) |elm| {
                        if (T == []const u8) {
                            if (std.mem.eql(u8, elm, item)) return true;
                        } else {
                            if (elm == item) return true;
                        }
                    }
                    return false;
                },
                .exclude => {
                    for (self.exclude) |elm| {
                        if (T == []const u8) {
                            if (std.mem.eql(u8, elm, item)) return false;
                        } else {
                            if (elm == item) return false;
                        }
                    }
                    return true;
                },
            }
        }
    };
}

test Filter {
    const expect = std.testing.expect;
    const opt1 = Options{
        .filter_depths = .{ .include = &.{ 1, 2, 3 } },
    };
    try expect(!opt1.filter_depths.includes(4));
    try expect(opt1.filter_depths.includes(2));

    const opt2 = Options{
        .filter_depths = .{ .exclude = &.{ 1, 2, 3 } },
    };
    try expect(opt2.filter_depths.includes(4));
    try expect(!opt2.filter_depths.includes(2));

    const opt3 = Options{
        .filter_depths = .{ .include = &.{} },
    };
    try expect(!opt3.filter_depths.includes(4));
    try expect(!opt3.filter_depths.includes(2));

    const opt4 = Options{
        .filter_depths = .{ .exclude = &.{} },
    };
    try expect(opt4.filter_depths.includes(4));
    try expect(opt4.filter_depths.includes(2));
}

/// Checks if the value is comptime-known
inline fn isComptime(val: anytype) bool {
    return @typeInfo(@TypeOf(.{val})).Struct.fields[0].is_comptime;
}

test isComptime {
    try std.testing.expect(isComptime(std.builtin.Type.StructField));
    try std.testing.expect(isComptime(@typeInfo(struct { f: u8 }).Struct.fields));
    var rt_slice: []const u8 = &[_]u8{1};
    _ = &rt_slice;
    try std.testing.expect(!isComptime(rt_slice));
}

/// Retrieves the value type tag.
fn typeTag(comptime T: type) std.builtin.TypeId {
    return std.meta.activeTag(@typeInfo(T));
}

test typeTag {
    try std.testing.expect(typeTag(@TypeOf(struct {}{})) == .Struct);
    try std.testing.expect(typeTag(@TypeOf(42)) == .ComptimeInt);
    try std.testing.expect(typeTag(@TypeOf(null)) == .Null);
}

/// Checks whether the value type belongs to the type tag.
fn typeTagIs(comptime T: type, comptime tag: std.builtin.TypeId) bool {
    return typeTag(T) == tag;
}

/// Checks whether the value type belongs to the provided list of tags.
fn typeTagIn(comptime T: type, comptime tags: []const std.builtin.TypeId) bool {
    inline for (tags) |tag| {
        if (typeTag(T) == tag) return true;
    }
    return false;
}

test typeTagIn {
    const equal = std.testing.expect;
    try equal(typeTagIn(@TypeOf(true), &.{ .Null, .Int, .Bool }));
}

/// Retrieves the default value of a struct field.
fn typeFieldDefValue(comptime T: type, comptime field: @TypeOf(.enum_literal)) meta.fieldInfo(T, field).type {
    const f = meta.fieldInfo(T, field);
    if (f.default_value == null) @compileError("Field doesn't have a default value");
    const dval_ptr = @as(*align(f.alignment) const anyopaque, @alignCast(f.default_value.?));
    const val = @as(*const f.type, @ptrCast(dval_ptr)).*;
    return val;
}

test typeFieldDefValue {
    const Data = struct {
        val1: []const u8 = "test",
        val2: u8 = 42,
        val3: ?u8 = null,
        val4: type = ?u8,
        val5: struct {} = .{},
    };

    const equal = std.testing.expectEqual;
    try equal("test", typeFieldDefValue(Data, .val1));
    try equal(42, typeFieldDefValue(Data, .val2));
    try equal(null, typeFieldDefValue(Data, .val3));
    try equal(?u8, typeFieldDefValue(Data, .val4));
    try equal(std.meta.fieldInfo(Data, .val5).type{}, typeFieldDefValue(Data, .val5));
}

/// Checks whether a type is a function pointer.
fn typeIsFnPtr(comptime T: type) bool {
    return @typeInfo(T) == .Pointer and @typeInfo(meta.Child(T)) == .Fn;
}

/// Checks whether a type is a slice.
fn typeIsSlicePtr(comptime T: type) bool {
    return @typeInfo(T) == .Pointer and @typeInfo(T).Pointer.size == .Slice;
}

/// Concatenates arg strings with the separator in between (comptime only).
fn strAddSepCt(sep: []const u8, args: anytype) []const u8 {
    if (!@inComptime()) @compileError("Must be called at comptime.");
    const args_len = meta.fields(@TypeOf(args)).len;
    const items: [args_len][]const u8 = args;

    var out: []const u8 = "";
    for (items) |field| {
        if (field.len == 0) continue;
        out = out ++ field ++ sep;
    }

    return out[0..out.len -| sep.len];
}

/// Concatenates arg strings with the separator in between (runtime only).
fn strAddSep(alloc: Allocator, sep: []const u8, args: anytype) !ArrayList(u8) {
    const args_len = meta.fields(@TypeOf(args)).len;
    const items: [args_len][]const u8 = args;

    var out = ArrayList(u8).init(alloc);
    for (items) |field| {
        if (field.len == 0) continue;
        try out.appendSlice(field);
        try out.appendSlice(sep);
    }

    out.shrinkRetainingCapacity(out.items.len -| sep.len);
    return out;
}

test strAddSep {
    const equal = std.testing.expectEqualStrings;
    const run = struct {
        pub fn case(comptime expect: []const u8, comptime sep: []const u8, args: anytype) !void {
            try equal(expect, comptime strAddSepCt(sep, args));

            const out = try strAddSep(std.testing.allocator, sep, args);
            defer out.deinit();
            try equal(expect, out.items);
        }
    };

    try run.case("a=b=c", "=", .{ "a", "b", "c" });
    try run.case("a", "=", .{ "", "a" });
    try run.case("a", "=", .{ "a", "" });
    try run.case("a", "=", .{"a"});
    try run.case("", "=", .{ "", "" });
    try run.case("", "=", .{""});
}

/// Configuration structure for strFoldBracketsCt.
const FoldBracketsConf = struct {
    fold_depth: u8 = 1, // 0 is do not fold
    bracket: enum { Round, Square, Curly, Angle, Any } = .Any,
    max_cap: usize = 32,
};

/// Folds content inside brackets with ".." based on the specified
/// configuration. Defaults are: nesting level = 1 (0 means no folding),
/// bracket type = any, and max capacity = 32. Returns unchanged input if
/// brackets are unbalanced or their nesting level, as well as the number of
/// pairs, exceeds the max capacity. (Function is comptime only)
fn strFoldBracketsCt(stream: []const u8, conf: FoldBracketsConf) []const u8 {
    if (!@inComptime()) @compileError("Must be called at comptime.");
    if (conf.fold_depth == 0) return stream;

    const Bracket = struct {
        idx: usize,
        type: u8,
        pub fn isPairTo(self: *const @This(), closing: u8) bool {
            const opposite = switch (self.type) {
                '(' => ')',
                '[' => ']',
                '{' => '}',
                '<' => '>',
                else => unreachable,
            };
            return closing == opposite;
        }
    };
    const IndexPair = struct { start: usize, end: usize };

    var brackets = Stack(Bracket, conf.max_cap){};
    var fold_stack = Stack(IndexPair, conf.max_cap){};

    const closing = switch (conf.bracket) {
        .Round => ')', // 40 41
        .Square => ']', // 91 93
        .Curly => '}', // 123 125
        .Angle => '>', // 60 62
        else => 0,
    };

    // Collect indices for trimming
    for (stream, 0..) |c, i| {
        if (c == '(' or c == '[' or c == '{' or c == '<') {
            brackets.push(.{ .idx = i, .type = c }) catch return stream;
        } else if (c == ')' or c == ']' or c == '}' or c == '>') {
            if (brackets.pop()) |start_bracket| {
                // Check if closing bracket matches the opening one
                if (!start_bracket.isPairTo(c)) return stream;
                // Save trimming indices only for the necessary bracket levels and types
                const paren_depth = brackets.len();
                if (paren_depth == (conf.fold_depth - 1) and (c == closing or conf.bracket == .Any)) {
                    // Save open and closed indices
                    fold_stack.push(.{ .start = start_bracket.idx, .end = i }) catch return stream;
                }
            } else {
                // If closing bracket is found but there wasn't an opening one
                return stream;
            }
        }
    }

    // If no brackets were found
    if (brackets.empty()) return stream;

    // Trim according to the collected data
    var out: []const u8 = "";
    var i: usize = 0;
    var next: usize = 0;

    // Per each trim index pair
    while (i < fold_stack.len()) : (i += 1) {
        const start = fold_stack.stack[i].start;
        const end = fold_stack.stack[i].end;
        out = out ++ stream[next .. start + 1] ++ if ((end - start) > 1) ".." else stream[start + 1 .. end];
        next = end;
    }

    // Append leftovers
    out = out ++ stream[next..];

    return out;
}

test strFoldBracketsCt {
    const equal = std.testing.expectEqualStrings;
    try equal("", comptime strFoldBracketsCt("", .{}));
    try equal("()", comptime strFoldBracketsCt("()", .{}));
    try equal("f", comptime strFoldBracketsCt("f", .{}));
    try equal("foo", comptime strFoldBracketsCt("foo", .{}));
    try equal("foo()", comptime strFoldBracketsCt("foo()", .{}));
    try equal("foo(..)", comptime strFoldBracketsCt("foo(bar)", .{}));
    try equal("(..)foo(..)", comptime strFoldBracketsCt("(bar)foo(bar)", .{}));
    try equal("(1(..)1(..)1)", comptime strFoldBracketsCt("(1(2)1(2)1)", .{ .fold_depth = 2 }));
    try equal("(1(2(..)2)1((..))1)", comptime strFoldBracketsCt("(1(2(3)2)1((3))1)", .{ .fold_depth = 3 }));
    try equal("(1(2(3)2)1)", comptime strFoldBracketsCt("(1(2(3)2)1)", .{ .fold_depth = 3, .bracket = .Angle }));
    try equal("(1(2<..>2)1)", comptime strFoldBracketsCt("(1(2<3>2)1)", .{ .fold_depth = 3, .bracket = .Angle }));
    try equal("(1(2<3>2{..}2)1)", comptime strFoldBracketsCt("(1(2<3>2{3}2)1)", .{ .fold_depth = 3, .bracket = .Curly }));
    try equal("(1(2<..>2{..}2)1)", comptime strFoldBracketsCt("(1(2<3>2{3}2)1)", .{ .fold_depth = 3, .bracket = .Any }));
}

/// Embraces the specified string with pre- and post-fixes.
pub fn strEmbraceWith(alloc: Allocator, str: []const u8, pre: []const u8, post: []const u8) !ArrayList(u8) {
    var out = try ArrayList(u8).initCapacity(alloc, str.len + pre.len + post.len);
    out.appendSliceAssumeCapacity(pre);
    out.appendSliceAssumeCapacity(str);
    out.appendSliceAssumeCapacity(post);
    return out;
}

test strEmbraceWith {
    const run = struct {
        pub fn case(expect: []const u8, input: []const u8, arg: struct { pre: []const u8, post: []const u8 }) !void {
            const actual = try strEmbraceWith(std.testing.allocator, input, arg.pre, arg.post);
            defer actual.deinit();
            try std.testing.expectEqualStrings(expect, actual.items);
        }
    };

    try run.case("[]", "", .{ .pre = "[", .post = "]" });
    try run.case("[Hello world]", "Hello world", .{ .pre = "[", .post = "]" });
}

/// Trims the string if its length exceeds the maximum, appending the `with`
/// suffix and ensuring the combined length of the suffix + truncated string
/// remains within the limit. If max is 0, the function does not truncate.
/// (Function is comptime only)
///
/// Function provides three `mode`s:
///   - In: Truncate and try to fit the suffix inside; otherwise, omit it.
///   - Out: Truncate and try to fit the suffix outside; otherwise, omit it.
///   - Auto: Truncate and try to fit the suffix, possibly at the expense of payload chars
///     until at least one char is left.
fn strTrimCt(str: []const u8, max: usize, with: []const u8, mode: enum { In, Out, Auto }) []const u8 {
    if (!@inComptime()) @compileError("Must be called at comptime.");
    if (max != 0 and (str.len > max)) {
        // Suffix should fit inside truncated string
        if (mode == .In and with.len < max) {
            return str[0..max -| with.len] ++ with;
        }
        // Suffix should be outside truncated string but fit into string length
        else if (mode == .Out and (with.len + max <= str.len)) {
            return str[0..max] ++ with;
        }
        // Suffix should fit in any way but with at least one payload character left
        else if (mode == .Auto) {
            // Suffix fits fully outside
            if (max + with.len <= str.len) {
                return str[0..max] ++ with;
            }
            // Suffix fits fully inside or part inside and part outside
            else if (str.len > with.len) {
                return str[0 .. str.len - with.len] ++ with;
            }
        }
        // Cannot fit the suffix, omit it and cut as is
        return str[0..max];
    }
    return str;
}

/// Trims the string if its length exceeds the maximum. See trimStrCT.
fn strTrim(alloc: std.mem.Allocator, str: []const u8, max: usize, with: []const u8, mode: enum { In, Out, Auto }) !std.ArrayList(u8) {
    var out = std.ArrayList(u8).init(alloc);
    if (max != 0 and (str.len > max)) {
        if (mode == .In and with.len < max) {
            try out.appendSlice(str[0..max -| with.len]);
            try out.appendSlice(with);
        } else if (mode == .Out and (with.len + max <= str.len)) {
            try out.appendSlice(str[0..max]);
            try out.appendSlice(with);
        } else if (mode == .Auto and (max + with.len <= str.len)) {
            try out.appendSlice(str[0..max]);
            try out.appendSlice(with);
        } else if (mode == .Auto and str.len > with.len) {
            try out.appendSlice(str[0 .. str.len - with.len]);
            try out.appendSlice(with);
        } else {
            try out.appendSlice(str[0..max]);
        }
    } else {
        try out.appendSlice(str);
    }
    return out;
}

test strTrim {
    const run = struct {
        pub fn case(expect: []const u8, comptime str: []const u8, comptime max: usize, comptime with: []const u8, mode: @TypeOf(.e)) !void {
            try std.testing.expectEqualStrings(expect, comptime strTrimCt(str, max, with, mode));
            const res = try strTrim(std.testing.allocator, str, max, with, mode);
            defer res.deinit();
            try std.testing.expectEqualStrings(expect, res.items);
        }
    };

    // Any mode
    try run.case("", "", 5, "..", .In);
    try run.case("", "", 0, "..", .In);
    try run.case("abcd", "abcd", 0, "..", .In);
    // Trim inside
    try run.case("abcd", "abcd", 4, "..", .In);
    try run.case("a..", "abcd", 3, "..", .In);
    try run.case("ab", "abcd", 2, "..", .In);
    try run.case("a", "abcd", 1, "..", .In);
    try run.case("a", "abc", 1, "..", .In);
    try run.case("a", "ab", 1, "..", .In);
    // Trim outside
    try run.case("abcd", "abcd", 4, "..", .Out);
    try run.case("abc", "abcd", 3, "..", .Out);
    try run.case("ab..", "abcd", 2, "..", .Out);
    try run.case("a..", "abcd", 1, "..", .Out);
    try run.case("a", "ab", 1, "..", .Out);
    // Trim auto
    try run.case("abcd", "abcd", 4, "..", .Auto);
    try run.case("ab..", "abcd", 3, "..", .Auto);
    try run.case("ab..", "abcd", 2, "..", .Auto);
    try run.case("a..", "abcd", 1, "..", .Auto);
    try run.case("a..", "abc", 1, "..", .Auto);
    try run.case("a", "ab", 1, "..", .Auto);
}

/// Adds an offset to the value within the integer type boundaries.
fn addOffset(comptime T: type, val: T, offset: isize) !T {
    switch (@typeInfo(T)) {
        .Int, .ComptimeInt => {},
        else => return error.NumericTypeIsNotSupported,
    }

    var res: T = val;
    if (offset < 0) {
        res -|= @abs(offset);
    } else {
        res +|= @intCast(offset);
    }
    return res;
}

test addOffset {
    const run = struct {
        pub fn case(comptime T: type, input: T, offset: isize, expected: T) !void {
            try std.testing.expectEqual(try addOffset(T, input, offset), expected);
        }
    };
    try run.case(usize, 1, 1, 2);
    try run.case(usize, 0, 0, 0);
    try run.case(usize, 0, std.math.minInt(isize), 0);
    try run.case(usize, std.math.maxInt(usize), 0, std.math.maxInt(usize));
    try run.case(usize, std.math.maxInt(usize), std.math.maxInt(isize), std.math.maxInt(usize));
}

/// Basic stack structure.
fn Stack(comptime T: type, comptime length: usize) type {
    return struct {
        const Self = @This();

        stack: [length]T = undefined,
        top: usize = 0,
        nil: bool = true,

        pub fn empty(self: *Self) bool {
            return self.nil;
        }

        pub fn cap(self: *Self) usize {
            return self.stack.len;
        }

        pub fn left(self: *Self) usize {
            if (self.nil) return self.cap();
            return self.cap() - self.len();
        }

        pub fn len(self: *Self) usize {
            return self.top;
        }

        pub fn fits(self: *Self, count: usize) !void {
            if ((self.len() + count) > self.cap()) return error.Overflow;
        }

        pub fn push(self: *Self, val: T) !void {
            try self.fits(1);
            self.stack[self.top] = val;
            self.top +|= 1;
            self.nil = false;
        }

        pub fn pop(self: *Self) ?T {
            if (self.nil) return null;
            if (self.top == 0) {
                self.nil = true;
            } else {
                self.top -= 1;
            }
            return self.stack[self.top];
        }
    };
}

test Stack {
    const equal = std.testing.expectEqual;

    const stack_size = 100;
    var stack = Stack(usize, stack_size){};

    try equal(stack_size, stack.cap());
    for (0..stack_size) |i| {
        try equal(i, stack.len());
        try equal(stack_size - i, stack.left());
        try stack.push(i);
    }
    try equal(stack_size, stack.len());
    var i = stack.len();
    while (i > 0) : (i -= 1) {
        try equal(i - 1, stack.pop());
    }
    try equal(0, stack.len());
    try equal(stack_size, stack.left());
}

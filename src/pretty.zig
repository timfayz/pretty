const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const meta = std.meta;

/// Retrieves the type tag (std.builtin.TypeId) of the value.
fn typeTag(comptime T: type) std.builtin.TypeId {
    return std.meta.activeTag(@typeInfo(T));
}

test typeTag {
    try std.testing.expect(typeTag(@TypeOf(struct {}{})) == .Struct);
    try std.testing.expect(typeTag(@TypeOf(42)) == .ComptimeInt);
    try std.testing.expect(typeTag(@TypeOf(null)) == .Null);
}

/// Checks whether the value type tag (std.builtin.TypeId) is
/// present in the provided list of tags.
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

/// Inserts the specified separator between the provided arguments in comptime.
fn strAddSepCt(sep: []const u8, args: anytype) []const u8 {
    assertComptime();
    const args_len = meta.fields(@TypeOf(args)).len;
    const items: [args_len][]const u8 = args;

    var out: []const u8 = "";
    for (items) |field| {
        if (field.len == 0) continue;
        out = out ++ field ++ sep;
    }

    return out[0..out.len -| sep.len];
}

test strAddSepCt {
    const equal = std.testing.expectEqualStrings;
    try equal("a=b=c", comptime strAddSepCt("=", .{ "a", "b", "c" }));
    try equal("a", comptime strAddSepCt("=", .{ "", "a" }));
    try equal("a", comptime strAddSepCt("=", .{ "a", "" }));
    try equal("a", comptime strAddSepCt("=", .{"a"}));
    try equal("", comptime strAddSepCt("=", .{ "", "" }));
    try equal("", comptime strAddSepCt("=", .{""}));
}

/// Configuration structure for strFoldBracketsCt.
const FoldBracketsConf = struct {
    fold_depth: u8 = 0,
    bracket: enum { Round, Square, Curly, Angle, Any } = .Round,
    max_cap: usize = 32,
};

/// Folds content inside brackets with ".." based on the specified configuration
/// in comptime. Defaults are: nesting level = 0 (top level starts with 0), bracket
/// type = (), and max capacity = 32. Returns unchanged input if brackets are
/// unbalanced or their nesting level, as well as the number of pairs, exceeds the
/// max capacity.
fn strFoldBracketsCt(stream: []const u8, conf: FoldBracketsConf) []const u8 {
    assertComptime();
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
                if (paren_depth == conf.fold_depth and (c == closing or conf.bracket == .Any)) {
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
    try equal("(0(..)0(..)0)", comptime strFoldBracketsCt("(0(1)0(1)0)", .{ .fold_depth = 1 }));
    try equal("(0(1(..)1)0((..))0)", comptime strFoldBracketsCt("(0(1(2)1)0((2))0)", .{ .fold_depth = 2 }));
    try equal("(0(1(2)1)0)", comptime strFoldBracketsCt("(0(1(2)1)0)", .{ .fold_depth = 2, .bracket = .Angle }));
    try equal("(0(1<..>1)0)", comptime strFoldBracketsCt("(0(1<2>1)0)", .{ .fold_depth = 2, .bracket = .Angle }));
    try equal("(0(1<2>1{..}1)0)", comptime strFoldBracketsCt("(0(1<2>1{2}1)0)", .{ .fold_depth = 2, .bracket = .Curly }));
    try equal("(0(1<..>1{..}1)0)", comptime strFoldBracketsCt("(0(1<2>1{2}1)0)", .{ .fold_depth = 2, .bracket = .Any }));
}

pub fn strEmbraceWithCt(str: []const u8, pre: []const u8, post: []const u8) []const u8 {
    assertComptime();
    return pre ++ str ++ post;
}

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
///
/// Function provides three `mode`s:
///   - In: Truncate and try to fit the suffix inside; otherwise, omit it.
///   - Out: Truncate and try to fit the suffix outside; otherwise, omit it.
///   - Auto: Truncate and try to fit the suffix, possibly at the expense of payload chars
///     until at least one char is left.
fn strTrimCt(str: []const u8, max: usize, with: []const u8, mode: enum { In, Out, Auto }) []const u8 {
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

/// Trims the string if its length exceeds the maximum. See trimStrCT for full docs.
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

/// Adds an offset to val within the integer type boundaries.
fn addOffset(comptime T: type, val: T, offset: isize) !T {
    switch (@typeInfo(T)) {
        .Int, .ComptimeInt => {},
        else => return error.ValueTypeNotSupported,
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

fn assertComptime() void {
    if (!@inComptime()) @compileError("Must be called at comptime");
}

/// Basic stack data structure
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

/// pretty's formatting options.
const Options = struct {
    // *_max options set to 0 means unlimited

    // Generic options
    depth_max: u8 = 5,
    length_max: u8 = 20,
    tab_size: u8 = 2,

    // Type visibility options
    type_show: bool = true,
    type_show_field: bool = true,
    type_show_tag: bool = false,
    type_show_name: bool = true,

    // Type shortening options
    type_shorten_name: bool = false, // TODO
    type_fold_brackets: bool = true,
    type_fold_brackets_except_fn: bool = true,
    type_name_max_len: usize = 60,

    // Value visibility options
    val_show: bool = true, // TODO
    val_show_empty: bool = true, // TODO
    val_show_default: bool = false, // TODO
    val_on_same_line: bool = false, // TODO

    // Pointer printing options
    ptr_do_not_deref: bool = false, // TODO
    ptr_skip_dup_unfold: bool = true, // TODO

    // Optional printing options
    optional_skip_dup_unfold: bool = true,

    // Struct printing options
    struct_max_len: usize = 5,
    struct_show_empty: bool = true,

    // Array printing options
    arr_max_len: usize = 20,
    arr_show_item_idx: bool = true,
    arr_skip_item_type: bool = true,
    arr_item_types_to_skip: []const std.builtin.TypeId = &.{
        .Int,
        .ComptimeInt,
        .Float,
        .ComptimeFloat,
        .Void,
        .Bool,
    },

    // Slice printing options
    slice_max_len: usize = 5,
    str_max_len: usize = 80,

    // Float printing options
    float_fmt: []const u8 = "", // TODO
};

/// pretty's implementation structure.
fn Pretty(options: Options) type {
    return struct {
        alloc: Allocator,
        out: ArrayList(u8),

        const Self = @This();
        const opt: Options = options;

        const Ctx = struct {
            depth: usize = 0,
            skip: bool = false,
            field: []const u8 = "",
            idx: ?usize = null,
            // prev: type

            fn incDepth(self: Ctx) Ctx {
                return Ctx{
                    .depth = self.depth + 1,
                    .skip = self.skip,
                    .field = self.field,
                    .idx = self.idx,
                };
            }

            fn setSkip(self: Ctx, comptime s: bool) Ctx {
                return Ctx{
                    .depth = self.depth,
                    .skip = s,
                    .field = self.field,
                    .idx = self.idx,
                };
            }

            fn setField(self: Ctx, f: []const u8) Ctx {
                return Ctx{
                    .depth = self.depth,
                    .skip = self.skip,
                    .field = f,
                    .idx = self.idx,
                };
            }

            fn setIdx(self: Ctx, comptime i: ?usize) Ctx {
                return Ctx{
                    .depth = self.depth,
                    .skip = self.skip,
                    .field = self.field,
                    .idx = i,
                };
            }
        };

        pub fn init(alloc: Allocator) Self {
            return Self{
                .alloc = alloc,
                .out = ArrayList(u8).init(alloc),
            };
        }

        pub fn deinit(self: *Self) void {
            self.out.deinit();
        }

        fn fmtTypeCt(comptime T: type) []const u8 {
            assertComptime();

            var name: []const u8 = @typeName(T);

            // [Option]
            if (opt.type_fold_brackets) {
                var level = 0;

                // [Option]
                if (opt.type_fold_brackets_except_fn and typeIsFnPtr(T)) {
                    level = 1;
                }

                // [Fixed] If type name starts with '@' (rare case)
                else if (name.len != 0 and name[0] == '@') {
                    level = 1;
                }

                name = comptime strFoldBracketsCt(name, .{ .fold_depth = level });
            }

            // [Option]
            if (opt.type_name_max_len != 0 and name.len > opt.type_name_max_len) {
                name = name[0..opt.type_name_max_len] ++ "..";
            }

            return name;
        }

        pub fn appendType(self: *Self, val: anytype, comptime ctx: Ctx) !void {
            const T = @TypeOf(val);

                    // [Option]
            if (!opt.type_show)
                return;

                    // [Option]
            // if (opt.optional_skip_dup_unfold and typeIs(ctx.parent, .Optional))
            //     return;

            comptime var field_name: []const u8 = "";
            comptime var tag_name: []const u8 = "";
            comptime var type_name: []const u8 = "";

                    // [Option]
            if (opt.type_show_field and ctx.field.len != 0) {
                field_name = comptime strEmbraceWithCt(ctx.field, ".", ":");
                    }

            // [Option]
            if (opt.type_show_tag) {
                const tag = @tagName(@typeInfo(T));
                tag_name = comptime strEmbraceWithCt(tag, "[", "]");
                }

                // [Option]
            if (opt.type_show_name) {
                type_name = comptime fmtTypeCt(T);
                    }

            // Flash
            try self.appendIndent(ctx);
            try self.out.appendSlice(comptime strAddSepCt(" ", .{ field_name, tag_name, type_name }));

            // [Option]
            if (opt.val_on_same_line)
                return;

            try self.appendNewline();
        }

        pub fn appendValue(self: *Self, val: []const u8, comptime ctx: Ctx) !void {
            // [Option]
            if (!opt.val_show)
                return;

            // [Option]
            if (opt.val_on_same_line) {
                // Print type` = `value
                const out = self.out.items;
                if (out.len != 0 and out[out.len -| 1] != ' ')
                    try self.out.appendSlice(" = ");
            } else {
                try self.appendIndent(ctx);
            }

            try self.out.appendSlice(val);
            try self.appendNewline();
        }

        pub fn appendValueEmpty(self: *Self, comptime ctx: Ctx) !void {
            // [Option]
            if (opt.val_show_empty)
                try self.appendValue("(empty)", ctx);
        }

        pub fn appendValueNull(self: *Self, comptime ctx: Ctx) !void {
            // [Option-less]
            try self.appendValue("null", ctx);
        }

        pub fn appendValueString(self: *Self, str: []const u8, comptime ctx: Ctx) !void {
            // [Option]
            const trimmed = try strTrim(self.alloc, str, opt.str_max_len, "..", .Auto);
            defer trimmed.deinit();

            // Embrace with quotes
            const quoted = try strEmbraceWith(self.alloc, trimmed.items);
            defer quoted.deinit();

            try self.appendValue(quoted.items, ctx);
        }

        pub fn appendValueAny(self: *Self, val: anytype, comptime ctx: Ctx) !void {
            const any = try std.fmt.allocPrint(self.alloc, "{any}", .{val});
            defer self.alloc.free(any);
            try self.appendValue(any, ctx);
        }

        pub fn appendValueType(self: *Self, comptime T: type, comptime ctx: Ctx) !void {
            // [Fixed]
            const type_name = comptime strFoldBracketsCt(@typeName(T), .{ .fold_depth = 1, .bracket = .Any });
            try self.appendValue(type_name, ctx);
        }

        pub fn appendIndex(self: *Self, comptime ctx: Ctx) !void {
            var buf: [32]u8 = undefined;
            try self.out.appendSlice(try std.fmt.bufPrint(&buf, "{d}", .{ctx.idx}));
            try self.out.appendSlice(": ");
        }

        pub fn appendIndent(self: *Self, comptime ctx: Ctx) !void {
            try self.out.appendNTimes(' ', (ctx.depth) * 2);
        }

        pub fn appendNewline(self: *Self) !void {
            try self.out.append('\n');
        }

        pub fn dump(self: *Self, val: anytype, comptime ctx: Ctx) !void {
            const T = @TypeOf(val);
            comptime var c = ctx;

            // [Option]
            if (opt.depth_max != 0 and ctx.depth > opt.depth_max) return;

            if (!c.skip) {
                // [Type]
                try self.appendType(val, c);
            } else {
                // Reset skip flag from previous call
                c = comptime c.setSkip(false);
            }

            // [Value]
            switch (@typeInfo(T)) {
                // TODO ErrorUnion, ErrorSet, Frame, Vector
                .Pointer => try self.dumpPointer(val, c),
                .Array => try self.dumpArray(val, c),
                // .Enum => try self.dumpEnum(val, c),
                // .Union => try self.dumpUnion(val, c),
                .Type => try self.appendValueType(val, c.incDepth()),
                .Null => try self.appendValueNull(c.incDepth()),
                .Optional => try self.dumpOptional(val, c),
                .Struct => try self.dumpStruct(val, c),
                .Int,
                .ComptimeInt,
                .Float,
                .ComptimeFloat,
                .Bool,
                => try self.appendValueAny(val, c.incDepth()),
                else => {
                    try self.appendValue("error.ValueTypeNotSupported(" ++ @typeName(T) ++ ")", c.incDepth());
                },
            }
        }

        pub fn dumpStruct(self: *Self, val: anytype, comptime ctx: Ctx) !void {
            const T = @TypeOf(val);

            // [Value] Empty struct
            if (meta.fields(T).len == 0) {
                // [Option]
                if (opt.struct_show_empty)
                    try self.appendValueEmpty(ctx.incDepth());
                return;
            }

            // [Value] Fields available
            inline for (meta.fields(T), 1..) |field, len| {
                // [Option]
                if (opt.struct_max_len != 0 and len > opt.struct_max_len) break;

                const field_value = @field(val, field.name);
                try self.dump(field_value, ctx.incDepth().setField(field.name));
            }
        }

        pub fn dumpPointer(self: *Self, val: anytype, comptime ctx: Ctx) !void {
            const T = @TypeOf(val);

            switch (@typeInfo(T).Pointer.size) {
                .One => try self.dumpPointerOne(val, ctx),
                .Many, .C => try self.dumpPointerMany(val, ctx),
                .Slice => try self.dumpPointerSlice(val, ctx),
            }
        }

        pub fn dumpPointerOne(self: *Self, val: anytype, comptime ctx: Ctx) !void {
            const T = @TypeOf(val);
            comptime var c = ctx;

            // [Value] Opaque or fn pointer
            if (meta.Child(T) == anyopaque or @typeInfo(meta.Child(T)) == .Fn)
                return; // [Option-less]

            // [Option]
            comptime {
                c = if (opt.ptr_skip_dup_unfold) c.setSkip(true) else c.incDepth();
            }

            // [Value] Other
            try self.dump(val.*, c.setField(""));
        }

        pub fn dumpPointerMany(self: *Self, val: anytype, comptime ctx: Ctx) !void {
            _ = val;

            // [Value]
            try self.appendValue("?", ctx);
        }

        pub fn dumpPointerSlice(self: *Self, val: anytype, comptime ctx: Ctx) !void {
            const T = @TypeOf(val);

            // TODO print ptr as address

            // [Value] Empty
            if (val.len == 0) {
                try self.appendValueEmpty(ctx);
                return;
            }

            // [Value] String
            @compileLog(meta.sentinel(T));
            if (meta.Child(T) == u8) {
                try self.appendValueString(val, ctx);
                return;
            }

            // [Value] Other
            for (val, 1..) |item, len| {
                    // [Option]
                if (opt.slice_max_len != 0 and len > opt.slice_max_len) break;

                try self.dump(item, ctx.setSkip(true));
            }
        }

        pub fn dumpArray(self: *Self, val: anytype, comptime ctx: Ctx) !void {
            const T = @TypeOf(val);
            comptime var c = ctx;

            // [Value] String
            if (meta.Child(T) == u8 and meta.sentinel(T) != null and meta.sentinel(T) == 0) {
                try self.appendValueString(val, ctx.incDepth());
                return;
            }

            // [Option]
            comptime {
                if (opt.arr_skip_item_type and typeTagIn(meta.Child(T), opt.arr_item_types_to_skip)) {
                    c = c.setSkip(true);
                } else {
                    c = c.incDepth();
                }
            }

            // [Value] Other
            inline for (val, 1..) |item, len| {
                    // [Option]
                if (opt.arr_max_len != 0 and len > opt.arr_max_len) break;

                try self.dump(item, comptime c.setIdx(len - 1));
            }
        }

        pub fn dumpUnion(self: *Self, val: anytype, comptime ctx: Ctx) !void {
            const T = @TypeOf(val);

            // [Value] Tagged union
            if (@typeInfo(T).Union.tag_type) |tag_type| {
                switch (@as(tag_type, val)) {
                    inline else => |tag| {
                        // try self.dump(@field(value, @tagName(tag)), @tagName(tag));
                        const field_val = meta.TagPayload(val, tag);
                        try self.dump(field_val, ctx.setField(@tagName(tag)).incDepth());
                    },
                }
                return;
            }

            // [Value] Normal one
            try self.appendValue("?", ctx.incDepth());
        }

        pub fn dumpEnum(self: *Self, val: anytype) !void {
            try self.appendType(val);
            try self.appendValue(@tagName(val));
        }

        pub fn dumpOptional(self: *Self, val: anytype, comptime ctx: Ctx) !void {
            // [Value] Optional has payload
            if (val) |unwrapped| {

                // [Option]
                const c = if (opt.optional_skip_dup_unfold) ctx.setSkip(true) else ctx.incDepth();

                try self.dump(unwrapped, c);
                return;
            }

            // [Value] Optional is null
            try self.appendValueNull(ctx.incDepth());
        }
    };
}

/// Generates pretty formatted string for an arbitrary input value, designed to
/// inspect data structures.
pub fn dump(allocator: Allocator, value: anytype, comptime options: Options) !ArrayList(u8) {
    var p = Pretty(options).init(allocator);
    try p.dump(value, .{});
    return p.out;
}

/// Prints pretty formatted string for an arbitrary input value, designed to
/// inspect data structures.
pub fn print(alloc: Allocator, value: anytype, comptime options: Options) !void {
    var out = try dump(alloc, value, options);
    defer out.deinit();
    std.debug.print("{s}", .{out.items});
}

test prettyDump {
    const testFunc = struct {
        pub fn run(input: anytype, expected: []const u8, comptime opt: PrettyOptions) !void {
            const actual = try prettyDump(std.testing.allocator, input, opt);
            defer actual.deinit();
            try std.testing.expectEqualStrings(expected, actual.items);
        }
    };

    try testFunc.run(std.testing.allocator,
        \\mem.Allocator
        \\  .ptr: *anyopaque
        \\  .vtable: *const mem.Allocator.VTable
        \\    mem.Allocator.VTable
        \\      .alloc: *const fn (*anyopaque, usize, u8, usize) ?[*]u8
        \\      .resize: *const fn (*anyopaque, []u8, u8, usize, usize) bool
        \\      .free: *const fn (*anyopaque, []u8, u8, usize) void
        \\
    , .{});
}

const std = @import("std");
const Allocator = std.mem.Allocator;
pub const ArrayList = std.ArrayList;

/// Retrieves the number of fields present in the struct (arg).
fn typeStructLen(comptime arg: anytype) comptime_int {
    const arg_T = @TypeOf(arg);
    const arg_T_info = @typeInfo(arg_T);
    if (arg_T_info != .Struct) {
        @compileError("expected tuple or struct, found " ++ @typeName(arg_T));
    }
    return arg_T_info.Struct.fields.len;
}

/// Retrieves the default value of a field.
fn typeDefaultValue(comptime T: type, comptime field: @TypeOf(.enum_literal)) std.meta.fieldInfo(T, field).type {
    const val_T = std.meta.fieldInfo(T, field).type;
    const def_val_ptr = std.meta.fieldInfo(T, field).default_value;
    if (def_val_ptr == null) @compileError("Field doesn't have a default value");
    const val = @as(*const val_T, @ptrCast(def_val_ptr.?)).*;
    return val;
}

/// Checks whether a type is a function pointer.
fn typeIsFnPtr(comptime T: type) bool {
    return @typeInfo(T) == .Pointer and @typeInfo(std.meta.Child(T)) == .Fn;
}

/// Inserts the specified separator between the provided arguments in comptime.
fn addSepCT(sep: []const u8, args: anytype) []const u8 {
    if (!@inComptime()) @compileError("Must be called at comptime");
    const args_len = typeStructLen(args);
    const items: [args_len][]const u8 = args;

    var out: []const u8 = "";
    for (items) |field| {
        if (field.len == 0) continue;
        out = out ++ field ++ sep;
    }

    return out[0..out.len -| sep.len];
}

test addSepCT {
    const equal = std.testing.expectEqualStrings;
    try equal("a=b=c", comptime addSepCT("=", .{ "a", "b", "c" }));
    try equal("a", comptime addSepCT("=", .{ "", "a" }));
    try equal("a", comptime addSepCT("=", .{ "a", "" }));
    try equal("a", comptime addSepCT("=", .{"a"}));
    try equal("", comptime addSepCT("=", .{ "", "" }));
    try equal("", comptime addSepCT("=", .{""}));
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

/// Configuration structure for trimParens.
const TrimBracketsConf = struct {
    trim_lvl: u8 = 0,
    bracket: enum { Round, Square, Curly, Angle, Any } = .Round,
    max_cap: usize = 32,
};

/// Folds content inside brackets with ".." based on the specified configuration
/// in comptime. Defaults are: nesting level = 0 (top level starts with 0), bracket
/// type = (), and max capacity = 32. Returns unchanged input if brackets are
/// unbalanced or their nesting level, as well as the number of pairs, exceeds the
/// max capacity.
fn trimBracketsCT(stream: []const u8, conf: TrimBracketsConf) []const u8 {
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
    var to_trim = Stack(IndexPair, conf.max_cap){};

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
                if (paren_depth == conf.trim_lvl and (c == closing or conf.bracket == .Any)) {
                    // Save open and closed indices
                    to_trim.push(.{ .start = start_bracket.idx, .end = i }) catch return stream;
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
    while (i < to_trim.len()) : (i += 1) {
        const start = to_trim.stack[i].start;
        const end = to_trim.stack[i].end;
        out = out ++ stream[next .. start + 1] ++ if ((end - start) > 1) ".." else stream[start + 1 .. end];
        next = end;
    }

    // Append leftovers
    out = out ++ stream[next..];

    return out;
}

test trimBracketsCT {
    const equal = std.testing.expectEqualStrings;
    try equal("", comptime trimBracketsCT("", .{}));
    try equal("()", comptime trimBracketsCT("()", .{}));
    try equal("f", comptime trimBracketsCT("f", .{}));
    try equal("foo", comptime trimBracketsCT("foo", .{}));
    try equal("foo()", comptime trimBracketsCT("foo()", .{}));
    try equal("foo(..)", comptime trimBracketsCT("foo(bar)", .{}));
    try equal("(..)foo(..)", comptime trimBracketsCT("(bar)foo(bar)", .{}));
    try equal("(0(..)0(..)0)", comptime trimBracketsCT("(0(1)0(1)0)", .{ .trim_lvl = 1 }));
    try equal("(0(1(..)1)0((..))0)", comptime trimBracketsCT("(0(1(2)1)0((2))0)", .{ .trim_lvl = 2 }));
    try equal("(0(1(2)1)0)", comptime trimBracketsCT("(0(1(2)1)0)", .{ .trim_lvl = 2, .bracket = .Angle }));
    try equal("(0(1<..>1)0)", comptime trimBracketsCT("(0(1<2>1)0)", .{ .trim_lvl = 2, .bracket = .Angle }));
    try equal("(0(1<2>1{..}1)0)", comptime trimBracketsCT("(0(1<2>1{2}1)0)", .{ .trim_lvl = 2, .bracket = .Curly }));
    try equal("(0(1<..>1{..}1)0)", comptime trimBracketsCT("(0(1<2>1{2}1)0)", .{ .trim_lvl = 2, .bracket = .Any }));
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
fn trimStrCT(str: []const u8, max: usize, with: []const u8, mode: enum { In, Out, Auto }) []const u8 {
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
fn trimStr(alloc: std.mem.Allocator, str: []const u8, max: usize, with: []const u8, mode: enum { In, Out, Auto }) !std.ArrayList(u8) {
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

test trimStrCT {
    const run = struct {
        pub fn case(expect: []const u8, comptime str: []const u8, comptime max: usize, comptime with: []const u8, mode: @TypeOf(.e)) !void {
            try std.testing.expectEqualStrings(expect, comptime trimStrCT(str, max, with, mode));
            const res = try trimStr(std.testing.allocator, str, max, with, mode);
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

/// Encloses the string with double quotes.
fn withQuotes(alloc: Allocator, str: []const u8) !ArrayList(u8) {
    var out = try std.ArrayList(u8).initCapacity(alloc, str.len + 2);
    out.appendAssumeCapacity('"');
    out.appendSliceAssumeCapacity(str);
    out.appendAssumeCapacity('"');
    return out;
}

test withQuotes {
    const run = struct {
        pub fn case(expect: []const u8, input: []const u8) !void {
            const actual = try withQuotes(std.testing.allocator, input);
            defer actual.deinit();
            try std.testing.expectEqualStrings(expect, actual.items);
        }
    };

    try run.case("\"\"", "");
    try run.case("\"Hello world\"", "Hello world");
}


/// pretty's formatting options.
const PrettyOptions = struct {
    // *_max options set to 0 means unlimited

    // Generic_*
    depth_max: u8 = 5, // TODO
    length_max: u8 = 20, // TODO
    tab_size: u8 = 2, // TODO

    // Field_* options
    field_show: bool = true, // TODO

    // Type_* options
    type_show: bool = true,
    type_show_name: bool = true,
    type_show_tag: bool = false,

    // Type_shorten_name_* options
    type_shorten_name: bool = false, // TODO
    type_fold_brackets: bool = true, // TODO
    type_fold_brackets_except_fn: bool = true, // TODO
    type_fold_brackets_opt: TrimBracketsConf = .{}, // TODO
    type_name_smart: bool = false, // TODO
    type_name_len_max: usize = 60, // TODO

    // Value_* options
    val_show: bool = true, // TODO
    val_show_empty: bool = true, // TODO
    val_show_default: bool = false, // TODO
    val_on_same_line: bool = false, // TODO

    // Value_pointers_* options
    ptr_skip_deref: bool = false, // TODO
    ptr_skip_dupe_unfold: bool = true, // TODO

    // Value_{struct, array, slice, string}_* options
    struct_len_max: usize = 5,
    arr_len_max: usize = 5, // TODO
    slice_len_max: usize = 5,
    str_len_max: usize = 80, // TODO

    // Value_float_* options
    float_fmt: []const u8 = "", // TODO
};

/// Generates pretty formatted string for an arbitrary input value, designed to
/// inspect data structures.
pub fn prettyDump(allocator: Allocator, val: anytype, comptime options: PrettyOptions) !ArrayList(u8) {

    // Implementation structure
    const Dump = struct {
        alloc: Allocator,
        opt: PrettyOptions,
        depth: usize = 0,
        length: usize = 0,
        out: ArrayList(u8),

        const Self = @This();

        /// Options for next dump invocation
        const DumpOptions = struct {
            field_name: ?[]const u8 = null,
            skip_field_type: bool = false,
        };

        pub fn init(alloc: Allocator, opt: PrettyOptions) Self {
            return Self{
                .alloc = alloc,
                .out = ArrayList(u8).init(alloc),
                .opt = opt,
            };
        }

        pub fn deinit(self: *Self) void {
            self.out.deinit();
        }

        /// Appends type name at the current nesting level to the output with
        /// optional indentation. It is always printed on the same line as
        /// the type name.
        pub fn appendField(self: *Self, field_name: ?[]const u8) !void {
            // [Option]
            if (!self.opt.field_show)
                return;

            try self.appendIndent();

            if (field_name) |name| {
                try self.out.appendSlice(".");
                try self.out.appendSlice(name);
                try self.out.appendSlice(": ");
            }
        }

        /// Appends struct field name to the output at the current nesting
        /// level.
        pub fn appendType(self: *Self, comptime T: type) !void {
            if (self.skip_type)
                return;

            // [Option]
            if (!self.opt.type_show)
                return;

            // [Option]
            if (!self.opt.field_show)
                try self.appendIndent();

            const fmt = struct {
                pub fn Type(comptime t: type, comptime opt: PrettyOptions) []const u8 {
                    var fmt: []const u8 = "";

                    // [Option]
                    if (opt.type_show_tag)
                        fmt = fmt ++ "[" ++ @tagName(@typeInfo(t)) ++ "]";

                    // [Option]
                    if (opt.type_show_name) {
                    var name: []const u8 = undefined;
                        name = @typeName(t);

                // [Option]
                        if (opt.type_fold_brackets) {
                        var level = 0;

                    // [Option]
                            if (opt.type_fold_brackets_except_fn and typeIsFnPtr(t)) {
                            level = 1;
                        }
                    // [Nonparametric] If type name starts with '@' (rare case)
                    else if (name.len != 0 and name[0] == '@') {
                        level = 1;
                    }

                            name = trimBracketsCt(name, .{ .trim_lvl = level });
                }

                // [Option]
                        if (opt.type_name_len_max != 0 and name.len > opt.type_name_len_max) {
                            name = name[0..opt.type_name_len_max] ++ "..";
                    }

                        // Separate [type_tag] and type_name
                        fmt = addSepCt(" ", .{ fmt, name });
            }

            return fmt;
        }
            };

            try self.out.appendSlice(comptime fmt.Type(T, options));

            // [Option]
            if (self.opt.val_on_same_line)
                return;

            try self.appendNewline();
        }

        /// Appends value at the current nesting level to the output.
        pub fn appendValue(self: *Self, value: []const u8) !void {
            // [Option]
            if (!self.opt.val_show)
                return;

            // [Option]
            if (!self.opt.val_show_empty and std.mem.eql(u8, value, "(empty)")) {
                return;
            }

            // [Option]
            if (self.opt.val_on_same_line) {
                try self.appendEqualSign();
            } else {
                try self.appendIndent();
            }

            try self.out.appendSlice(value);
            try self.appendNewline();
        }

        /// Appends ...
        pub fn appendValueEmpty(self: *Self) !void {
            try self.appendValue("(empty)");
        }

        /// Appends ...
        pub fn appendValueString(self: *Self, str: []const u8) !void {
            // [Option]
            const trimmed = try trimStr(self.alloc, str, self.opt.str_len_max, "..", .Auto);
            defer trimmed.deinit();

            // Embrace with quotes
            const quoted = try withQuotes(self.alloc, trimmed.items);
            defer quoted.deinit();

            try self.appendValue(quoted.items);
        }

        /// Appends ...
        pub fn appendEqualSign(self: *Self) !void {
            const out = self.out.items;
            if (out.len == 0 or out[out.len -| 1] == ' ')
                return;
            try self.out.appendSlice(" = ");
        }

        /// Appends ...
        pub fn appendBreak(self: *Self) !void {
            try self.appendNewline();
            try self.appendIndent();
        }

        /// Appends space indentation to the output according to the current nesting level.
        pub fn appendIndent(self: *Self) !void {
            try self.out.appendNTimes(' ', (self.depth) * 2);
        }

        /// Appends newline to the output.
        pub fn appendNewline(self: *Self) !void {
            if (self.out.items.len != 0)
            try self.out.append('\n');
        }

        pub fn dump(self: *Self, value: anytype) !void {
            const T = @TypeOf(value);

            // [Option]
            if (self.opt.depth_max != 0 and self.depth > self.opt.depth_max) return;

            self.depth += 1;
            switch (@typeInfo(T)) {
                .Pointer => try self.dumpPointer(value),
                .Struct => try self.dumpStruct(value),
                .Array => try self.dumpArray(value),
                .Enum => try self.dumpEnum(value),
                .Union => try self.dumpUnion(value),
                .Int,
                .ComptimeInt,
                .Float,
                .ComptimeFloat,
                .Bool,
                => try self.dumpLiteral(value),
                .Type => try self.dumpType(value),
                .Null => try self.dumpNull(value),
                .Optional => try self.dumpOptional(value),
                // TODO add error union
                else => {
                    try self.appendValue("error.ValueTypeNotSupported(" ++ @typeName(T) ++ ")");
                },
            }
            self.depth -= 1;
        }

        pub fn dumpStruct(self: *Self, value: anytype) !void {
            const T = @TypeOf(value);

            // [Type]
            try self.appendType(T);

            // [Value] Struct is empty
            if (std.meta.fields(T).len == 0) {
                // try self.appendValueEmpty(); // Empty struct is not a value
                return;
            }

            // [Value] Per each field
            inline for (std.meta.fields(T), 1..) |field, len| {
                // [Option]
                if (self.opt.struct_len_max != 0 and len > self.opt.struct_len_max) break;

                try self.appendField(field.name);
                // try self.appendType(field.type);
                try self.dump(@field(value, field.name));
            }
        }

        pub fn dumpPointer(self: *Self, value: anytype) !void {
            const T = @TypeOf(value);

            switch (@typeInfo(T).Pointer.size) {
                .One => try self.dumpPointerOne(value),
                .Many, .C => try self.dumpPointerMany(value),
                .Slice => try self.dumpPointerSlice(value),
            }
        }

        pub fn dumpPointerOne(self: *Self, value: anytype) !void {
            const T = @TypeOf(value);

            // [Type]
            try self.appendType(T);

            // [Value] Opaque or function pointer
            if (std.meta.Child(T) == anyopaque or @typeInfo(std.meta.Child(T)) == .Fn) {}

            // [Value] Other
            else {
                try self.dump(value.*);
            }
        }

        pub fn dumpPointerMany(self: *Self, value: anytype) !void {
            const T = @TypeOf(value);

            // [Type]
            try self.appendType(T);

            // [Value]
            try self.appendValue("?");
        }

        pub fn dumpPointerSlice(self: *Self, value: anytype) !void {
            const T = @TypeOf(value);

            // TODO print ptr as address

            // [Value] Empty
            if (value.len == 0) {
                try self.appendValueEmpty();
                return;
            }

            // [Value] String
            if (std.meta.Child(T) == u8) {
                try self.appendValueString(value);
            }

            // [Value] Other
            else {
                for (value, 1..) |item, len| {
                    // [Option]
                    if (self.opt.slice_len_max != 0 and len > self.opt.slice_len_max) break;

                    try self.dump(item);
                }
            }
        }

        pub fn dumpArray(self: *Self, value: anytype) !void {
            const T = @TypeOf(value);

            // [Type]
            try self.appendType(T);

            // [Value] String
            if (std.meta.Child(T) == u8 and std.meta.sentinel(T) != null and std.meta.sentinel(T) == 0) {
                try self.appendValueString(value);
            }

            // [Value] Other
            else {
                for (value, 1..) |item, len| {
                    // [Option]
                    if (self.opt.arr_len_max != 0 and len > self.opt.arr_len_max) break;

                    try self.dump(item);
                }
            }
        }

        pub fn dumpUnion(self: *Self, value: anytype) !void {
            const T = @TypeOf(value);
            const Tag = @typeInfo(T).Union.tag_type;

            // [Type]
            try self.appendType(T);

            // [Value] Tagged union
            if (Tag) |tag_type| {
                switch (@as(tag_type, value)) {
                    inline else => |tag| {
                        // try self.dump(@field(value, @tagName(tag)), @tagName(tag));
                        try self.dump(std.meta.TagPayload(value, tag), @tagName(tag));
                    },
                }
            }

            // [Value] Normal one
            else {
                try self.appendValue("?");
            }
        }

        pub fn dumpEnum(self: *Self, value: anytype) !void {
            const T = @TypeOf(value);

            // [Type]
            try self.appendType(T);

            // [Value]
            try self.appendValue(@tagName(value));
        }

        pub fn dumpNull(self: *Self, value: anytype) !void {
            const T = @TypeOf(value);
            try self.appendType(T);
            try self.appendValue("null");
        }

        pub fn dumpOptional(self: *Self, value: anytype) !void {
            const T = @TypeOf(value);

            // [Type]
            try self.appendType(T);

            // [Value]
            if (value) |unwrapped| {
                try self.dump(unwrapped, null);
            } else {
                try self.appendValue("null");
            }
        }

        pub fn dumpType(self: *Self, comptime value: type) !void {
            // [Type]
            try self.appendType(value);

            // [Value]
            const type_name = comptime trimBracketsCt(@typeName(value), .{ .trim_lvl = 1, .bracket = .Any });
            try self.appendValue(type_name);
        }

        pub fn dumpLiteral(self: *Self, value: anytype) !void {
            // [Type]
            try self.appendType(@TypeOf(value));

            // [Value]
            const val_fmt = try std.fmt.allocPrint(self.alloc, "{any}", .{value});
            defer self.alloc.free(val_fmt);
            try self.appendValue(val_fmt);
        }
    };

    var p = Dump.init(allocator, options);
    try p.dump(val);
    return p.out;
}

/// Prints pretty formatted string for an arbitrary input value, designed to
/// inspect data structures.
pub fn prettyPrint(allocator: Allocator, val: anytype, comptime opt: PrettyOptions) !void {
    var out = try prettyDump(allocator, val, opt);
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

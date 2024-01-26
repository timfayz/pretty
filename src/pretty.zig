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
    field_skip: bool = false,

    // Type_* options
    type_skip: bool = false,
    type_skip_name: bool = false,
    type_skip_tag: bool = true,

    // Type_shorten_name_* options
    type_shorten_name: bool = false, // TODO
    type_fold_brackets: bool = true, // TODO
    type_fold_brackets_except_fn: bool = true, // TODO
    type_fold_brackets_opt: TrimBracketsConf = .{}, // TODO
    type_name_smart: bool = false, // TODO
    type_name_len_max: usize = 60, // TODO

    // Value_* options
    val_on_same_line: bool = true, // TODO
    val_show_empty: bool = true, // TODO

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

        pub fn fmtType(comptime T: type) []const u8 {
            const T_info = @typeInfo(T);
            var fmt: []const u8 = "";

            // [Option]
            if (!options.type_skip_tag)
                    fmt = fmt ++ "[" ++ @tagName(T_info) ++ "]";

            // [Option]
            if (!options.type_skip_name) {
                    var name: []const u8 = undefined;
                name = @typeName(T);

                // [Option]
                if (options.type_fold_brackets) {
                        var level = 0;

                    // [Option]
                    if (options.type_fold_brackets_except_fn and typeIsFnPtr(T)) {
                            level = 1;
                        }
                    // [Nonparametric] If type name starts with '@' (rare case)
                    else if (name.len != 0 and name[0] == '@') {
                        level = 1;
                    }

                    name = trimBracketsCT(name, .{ .trim_lvl = level });
                }

                // [Option]
                if (options.type_name_len_max != 0 and name.len > options.type_name_len_max) {
                    name = name[0..options.type_name_len_max] ++ "..";
                    }

                // Separate `[type_tag] type_name`
                    fmt = addSepCT(" ", .{ fmt, name });
            }

            return fmt;
        }

        /// Appends space indentation based on the nesting level.
        pub fn appendPadding(self: *Self) !void {
            try self.out.appendNTimes(' ', (self.depth -| 1) * 2);
        }

        /// Appends a structure field name at the current nesting level.
        pub fn appendFieldType(self: *Self, field_name: ?[]const u8, comptime T: type) !void {
            try self.appendPadding();

            // [Option]
            if (!options.field_skip) {
            if (field_name) |name| {
                try self.out.appendSlice(".");
                try self.out.appendSlice(name);
                try self.out.appendSlice(": ");
            }
            }

            // [Option]
            if (!options.type_skip) {
            try self.out.appendSlice(comptime fmtType(T));
            }

            try self.out.append('\n');
        }

        /// Appends a value at the current nesting level.
        pub fn appendValue(self: *Self, value: []const u8) !void {
            // [Option]
            if (options.val_on_same_line)
                self.depth +|= 1;
            try self.appendPadding();
            try self.out.appendSlice(value);
            try self.out.append('\n');
            self.depth -|= 1;
        }

        pub fn dump(self: *Self, value: anytype, field_name: ?[]const u8) anyerror!void {
            // [Option]
            if (options.depth_max != 0 and self.depth +| 1 > options.depth_max) return;

            const T = @TypeOf(value);
            const T_info = @typeInfo(T);

            self.depth += 1;
            switch (T_info) {
                .Pointer => try self.dumpPointer(value, field_name),
                .Struct => try self.dumpStruct(value, field_name),
                .Array => try self.dumpArray(value, field_name),
                .Enum => try self.dumpEnum(value, field_name),
                .Union => try self.dumpUnion(value, field_name),
                .Int,
                .ComptimeInt,
                .Float,
                .ComptimeFloat,
                .Bool,
                => try self.dumpLiteral(value, field_name),
                .Type => try self.dumpType(value, field_name),
                .Null => try self.dumpNull(value, field_name),
                .Optional => try self.dumpOptional(value, field_name),
                // TODO add error union
                else => {
                    try self.appendValue("error.ValueTypeNotSupported(" ++ @typeName(T) ++ ")");
                },
            }
            self.depth -= 1;
        }

        pub fn dumpStruct(self: *Self, value: anytype, field_name: ?[]const u8) anyerror!void {
            const T = @TypeOf(value);

            // [Field + Type]
            try self.appendFieldType(field_name, T);

            // [Value] Per each struct fields
            inline for (std.meta.fields(T), 1..) |field, len| {
                // [Option]
                if (options.struct_len_max != 0 and len > options.struct_len_max) break;

                try self.dump(@field(value, field.name), field.name);
            }
        }

        pub fn dumpPointer(self: *Self, value: anytype, field_name: ?[]const u8) anyerror!void {
            const T = @TypeOf(value);
            const T_info = @typeInfo(T);

            switch (T_info.Pointer.size) {
                .One => try self.dumpPointerOne(value, field_name),
                .Many, .C => try self.dumpPointerMany(value, field_name),
                .Slice => try self.dumpPointerSlice(value, field_name),
            }
        }

        pub fn dumpPointerOne(self: *Self, value: anytype, field_name: ?[]const u8) anyerror!void {
            const T = @TypeOf(value);

            // [Field + Type]
            try self.appendFieldType(field_name, T);

            // [Value] Opaque or function pointer
            if (std.meta.Child(T) == anyopaque or @typeInfo(std.meta.Child(T)) == .Fn) {}

            // [Value] Other
            else {
                try self.dump(value.*, null);
            }
        }

        pub fn dumpPointerMany(self: *Self, value: anytype, field_name: ?[]const u8) anyerror!void {
            const T = @TypeOf(value);

            // [Field + Type]
            try self.appendFieldType(field_name, T);

            // [Value]
            try self.appendValue("?");
        }

        pub fn dumpPointerSlice(self: *Self, value: anytype, field_name: ?[]const u8) anyerror!void {
            const T = @TypeOf(value);

            // [Field + Type]
            try self.appendFieldType(field_name, T);

            // [Value] Empty
            if (value.len == 0) {
                try self.appendValue("(empty)");
                return;
            }

            // [Value] String
            if (std.meta.Child(T) == u8) {
                // [Option]
                const str = try trimStr(self.alloc, value, options.str_len_max, "..", .Auto);
                defer str.deinit();

                // Embrace with quotes
                const str_quoted = try withQuotes(self.alloc, str.items);
                defer str_quoted.deinit();

                try self.appendValue(str_quoted.items);
            }

            // [Value] Other
            else {
                for (value, 1..) |item, len| {
                    // [Option]
                    if (options.slice_len_max != 0 and len > options.slice_len_max) break;

                    try self.dump(item, null);
                }
            }
        }

        pub fn dumpArray(self: *Self, value: anytype, field_name: ?[]const u8) anyerror!void {
            const T = @TypeOf(value);
            const T_info = @typeInfo(T);
            _ = T_info;

            // [Field + Type]
            try self.appendFieldType(field_name, T);

            // [Value] String
            if (std.meta.Child(T) == u8 and std.meta.sentinel(T) != null and std.meta.sentinel(T) == 0) {
                var buf: [256]u8 = undefined;
                var fmt: []const u8 = undefined;
                fmt = try std.fmt.bufPrint(&buf, "\"{s}\"", .{value});
                try self.appendValue(fmt);
            }

            // [Value] Other
            else {
                for (value, 1..) |item, len| {
                    // [Option]
                    if (options.arr_len_max != 0 and len > options.arr_len_max) break;

                    try self.dump(item, null);
                }
            }
        }

        pub fn dumpUnion(self: *Self, value: anytype, field_name: ?[]const u8) anyerror!void {
            const T = @TypeOf(value);
            const Tag = @typeInfo(T).Union.tag_type;

            // [Field + Type]
            try self.appendFieldType(field_name, T);

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

        pub fn dumpEnum(self: *Self, value: anytype, field_name: ?[]const u8) anyerror!void {
            const T = @TypeOf(value);

            // [Field + Type]
            try self.appendFieldType(field_name, T);

            // [Value]
            try self.appendValue(@tagName(value));
        }

        pub fn dumpNull(self: *Self, value: anytype, field_name: ?[]const u8) anyerror!void {
            const T = @TypeOf(value);
            try self.appendFieldType(field_name, T);
            try self.appendValue("null");
        }

        pub fn dumpOptional(self: *Self, value: anytype, field_name: ?[]const u8) anyerror!void {
            const T = @TypeOf(value);

            // [Field + Type]
            try self.appendFieldType(field_name, T);

            // [Value]
            if (value) |unwrapped| {
                try self.dump(unwrapped, null);
            } else {
                try self.appendValue("null");
            }
        }

        pub fn dumpType(self: *Self, comptime value: type, field_name: ?[]const u8) anyerror!void {
            // [Field + Type]
            try self.appendFieldType(field_name, value);

            // [Value]
            const type_name = comptime trimBracketsCT(@typeName(value), .{ .trim_lvl = 1, .bracket = .Any });
            try self.appendValue(type_name);
        }

        pub fn dumpLiteral(self: *Self, value: anytype, field_name: ?[]const u8) anyerror!void {
            const T = @TypeOf(value);
            var buf: [256]u8 = undefined;
            var fmt: []const u8 = undefined;

            // [Field + Type]
            try self.appendFieldType(field_name, T);

            // [Value]
            fmt = try std.fmt.bufPrint(&buf, "{any}", .{value});
            try self.appendValue(fmt);
        }

        pub fn deinit(self: *Self) void {
            self.out.deinit();
        }
    };

    var p = Dump.init(allocator, options);
    try p.dump(val, null);
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

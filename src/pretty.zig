const std = @import("std");
const Allocator = std.mem.Allocator;
pub const String = std.ArrayList(u8);

fn argsStructLen(comptime args: anytype) comptime_int {
    const args_T = @TypeOf(args);
    const args_T_info = @typeInfo(args_T);
    if (args_T_info != .Struct) {
        @compileError("expected tuple or struct, found " ++ @typeName(args_T));
    }
    return args_T_info.Struct.fields.len;
}

/// Inserts the specified separator between the provided arguments in comptime.
fn addSepCT(sep: []const u8, args: anytype) []const u8 {
    if (!@inComptime()) @compileError("Must be called at comptime");
    const args_len = argsStructLen(args);
    const items: [args_len][]const u8 = args;

    var out: []const u8 = "";
    for (items) |field| {
        if (field.len == 0) continue;
        out = out ++ field ++ sep;
    }

    return out[0..out.len -| sep.len];
}

/// Inserts the specified separator between the provided arguments in runtime.
fn addSepRuntime(sep: []const u8, args: anytype) ![]const u8 {
    const args_T = @TypeOf(args);
    const args_T_info = @typeInfo(args_T);
    if (args_T_info != .Struct) {
        @compileError("expected tuple or struct, found " ++ @typeName(args_T));
    }

    const args_len = args_T_info.Struct.fields.len;
    const items: [args_len][]const u8 = args;
    if (items.len == 0)
        return "";

    var out = std.ArrayList(u8).init(std.heap.c_allocator);
    for (items) |field| {
        if (field.len == 0) continue;
        try out.appendSlice(field);
        try out.appendSlice(sep);
    }

    return out.items[0..out.items.len -| sep.len];
}

test addSepCT {
    try std.testing.expectEqualSlices(u8, "a=b=c", comptime addSepCT("=", .{ "a", "b", "c" }));
    try std.testing.expectEqualSlices(u8, "a", comptime addSepCT("=", .{ "", "a" }));
    try std.testing.expectEqualSlices(u8, "a", comptime addSepCT("=", .{ "a", "" }));
    try std.testing.expectEqualSlices(u8, "a", comptime addSepCT("=", .{"a"}));
    try std.testing.expectEqualSlices(u8, "", comptime addSepCT("=", .{ "", "" }));
    try std.testing.expectEqualSlices(u8, "", comptime addSepCT("=", .{""}));
}

fn addOffset(comptime T: type, val: T, offset: isize) !T {
    switch (@typeInfo(T)) {
        .Int,
        .ComptimeInt,
        => {},
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
const TrimParensConf = struct {
    trim_lvl: u8 = 0,
    brackets: enum { Round, Square, Curly, Angle } = .Round,
    max_cap: usize = 32,
};

/// Folds content inside brackets with ".." based on the specified configuration
/// in comptime. Defaults are: nesting level = 0 (top level starts with 0), bracket
/// type = (), and max capacity = 32. Returns unchanged input if brackets are
/// unbalanced or their nesting level, as well as the number of pairs, exceeds the
/// max capacity.
fn trimParensCT(stream: []const u8, conf: TrimParensConf) []const u8 {
    var br_indices = Stack(usize, conf.max_cap){};
    var trim_indices = Stack(usize, conf.max_cap * 2){};

    const closed = switch (conf.brackets) {
        .Round => ')', // 40 41
        .Square => ']', // 91 93
        .Curly => '}', // 123 125
        .Angle => '>', // 60 62
    };

    // Collect indices for trimming
    for (stream, 0..) |c, i| {
        if (c == '(' or c == '[' or c == '{' or c == '<') {
            br_indices.push(i) catch return stream;
        } else if (c == ')' or c == ']' or c == '}' or c == '>') {
            if (br_indices.pop()) |start| {
                // Skip unnecessary levels and non-target brackets
                if (br_indices.len() != conf.trim_lvl or c != closed) continue;
                trim_indices.push(start) catch return stream; // Save open bracket index
                trim_indices.push(i) catch return stream; // Save closed bracket index
            } else {
                return stream; // Unbalanced brackets
            }
        }
    }

    // Brackets should be balanced
    if (br_indices.empty()) return stream;

    // Trim according to the collected data
    var out: []const u8 = "";
    var idx: usize = 0;
    var next: usize = 0;

    // Go over index pairs
    while (idx < trim_indices.len()) : (idx += 2) {
        const start = trim_indices.stack[idx];
        const end = trim_indices.stack[idx + 1];
        out = out ++ stream[next .. start + 1] ++ if ((end - start) > 1) ".." else stream[start + 1 .. end];
        next = end;
    }

    // Append leftovers
    out = out ++ stream[next..];

    return out;
}

test trimParensCT {
    const equal = std.testing.expectEqualStrings;
    try equal("", comptime trimParensCT("", .{}));
    try equal("()", comptime trimParensCT("()", .{}));
    try equal("f", comptime trimParensCT("f", .{}));
    try equal("foo", comptime trimParensCT("foo", .{}));
    try equal("foo()", comptime trimParensCT("foo()", .{}));
    try equal("foo(..)", comptime trimParensCT("foo(bar)", .{}));
    try equal("(..)foo(..)", comptime trimParensCT("(bar)foo(bar)", .{}));
    try equal("(0(..)0(..)0)", comptime trimParensCT("(0(1)0(1)0)", .{ .trim_lvl = 1 }));
    try equal("(0(1(..)1)0((..))0)", comptime trimParensCT("(0(1(2)1)0((2))0)", .{ .trim_lvl = 2 }));
    try equal("(0(1(2)1)0)", comptime trimParensCT("(0(1(2)1)0)", .{ .trim_lvl = 2, .brackets = .Angle }));
    try equal("(0(1<..>1)0)", comptime trimParensCT("(0(1<2>1)0)", .{ .trim_lvl = 2, .brackets = .Angle }));
    try equal("(0(1<2>1{..}1)0)", comptime trimParensCT("(0(1<2>1{2}1)0)", .{ .trim_lvl = 2, .brackets = .Curly }));
}

/// pretty's formatting options.
const PrettyOptions = struct {
    type_skip: bool = false,
    type_skip_name: bool = false,
    type_skip_tag: bool = true,
    type_short_name: bool = false,
    type_fold_parens: bool = true,
    type_fold_parens_except_fn: bool = true,
    type_fold_parens_opt: TrimParensConf = .{},
    val_use_single_line: bool = true,
    val_skip_empty: bool = true,
    slice_max_len: usize = 3,
    ptr_skip_deref: bool = false,
    ptr_skip_dupe_unfold: bool = true,
    str_max_len: usize = 80,
    tab_size: u8 = 2,
    max_depth: u8 = 4,
};

/// Checks whether a type is a function pointer.
fn typeIsFnPtr(comptime T: type) bool {
    return @typeInfo(T) == .Pointer and @typeInfo(std.meta.Child(T)) == .Fn;
}

/// Generates pretty formatted string for an arbitrary input value, designed to
/// inspect data structures.
pub fn prettyDump(allocator: Allocator, val: anytype, comptime opt: PrettyOptions) !String {

    // Implementation structure
    const PrettyPrinter = struct {
        lvl: usize = 0,
        alloc: Allocator,
        out: String,

        const Self = @This();

        pub fn init(alloc: Allocator) Self {
            return Self{ .out = String.init(alloc), .alloc = alloc };
        }

        pub fn incNestLevel(self: *Self) void {
            self.lvl +|= 1;
        }

        pub fn decNestLevel(self: *Self) void {
            self.lvl -|= 1;
        }

        pub fn fmtType(comptime T: type) []const u8 {
            const T_info = @typeInfo(T);
            var fmt: []const u8 = "";

            if (!opt.type_skip) {
                // Type tag
                if (!opt.type_skip_tag) {
                    fmt = fmt ++ "[" ++ @tagName(T_info) ++ "]";
                }

                // Type name
                if (!opt.type_skip_name) {
                    var name: []const u8 = undefined;
                    // Folding
                    if (opt.type_fold_parens) {
                        var level = 0;
                        // Except function(brackets)
                        if (opt.type_fold_parens_except_fn and typeIsFnPtr(T)) {
                            level = 1;
                        }
                        name = trimParensCT(@typeName(T), .{ .trim_lvl = level });
                    } else {
                        name = @typeName(T);
                    }
                    fmt = addSepCT(" ", .{ fmt, name });
                }
            }

            return fmt;
        }

        /// Appends space indentation based on the nesting level.
        pub fn appendPadding(self: *Self) !void {
            try self.out.appendNTimes(' ', self.lvl * 2);
        }

        /// Appends a structure field name at the current nesting level.
        pub fn appendFieldTypeN(self: *Self, field_name: ?[]const u8, comptime T: type) !void {
            try self.appendPadding();
            if (field_name) |name| {
                try self.out.appendSlice(".");
                try self.out.appendSlice(name);
                try self.out.appendSlice(": ");
            }
            try self.out.appendSlice(comptime fmtType(T));
            try self.out.append('\n');
        }

        /// Appends a value at the current nesting level.
        pub fn appendValueN(self: *Self, value: []const u8) !void {
            try self.appendPadding();
            try self.out.appendSlice(value);
            try self.out.append('\n');
        }

        /// Appends a value at the next nesting level, then returns it back.
        pub fn appendValueNestOnceN(self: *Self, value: []const u8) !void {
            self.incNestLevel();
            try self.appendValueN(value);
            self.decNestLevel();
        }

        pub fn dumpPointerOne(self: *Self, value: anytype, field_name: ?[]const u8) anyerror!void {
            const T = @TypeOf(value);
            const T_info = @typeInfo(T);

            // Field + Type
            try self.appendFieldTypeN(field_name, T);

            // Opaque or function pointer
            if (T_info.Pointer.child == anyopaque or @typeInfo(T_info.Pointer.child) == .Fn) {}

            // Other
            else {
                self.incNestLevel();
                try self.dump(value.*, null);
                self.decNestLevel();
            }
        }

        pub fn dumpPointerMany(self: *Self, value: anytype, field_name: ?[]const u8) anyerror!void {
            const T = @TypeOf(value);

            // Field + Type
            try self.appendFieldTypeN(field_name, T);

            // Value
            try self.appendValueNestOnceN("?");
        }

        pub fn dumpSlice(self: *Self, value: anytype, field_name: ?[]const u8) anyerror!void {
            const T = @TypeOf(value);
            const T_info = @typeInfo(T);

            var buf: [256]u8 = undefined;
            var fmt: []const u8 = undefined;

            // Field + Type
            try self.appendFieldTypeN(field_name, T);
            if (value.len == 0) {
                try self.appendValueNestOnceN("(empty)");
                return;
            }

            // String
            if (T_info.Pointer.child == u8) {
                fmt = try std.fmt.bufPrint(&buf, "\"{s}\"", .{value});
                try self.appendValueNestOnceN(value);
            }

            // Other
            else {
                self.incNestLevel();
                for (value) |item|
                    try self.dump(item, null);
                self.decNestLevel();
            }
        }

        pub fn dumpPointer(self: *Self, value: anytype, field_name: ?[]const u8) anyerror!void {
            const T = @TypeOf(value);
            const T_info = @typeInfo(T);

            switch (T_info.Pointer.size) {
                .Slice => try self.dumpSlice(value, field_name),
                .One => try self.dumpPointerOne(value, field_name),
                .Many, .C => try self.dumpPointerMany(value, field_name),
            }
        }

        pub fn dumpArray(self: *Self, value: anytype, field_name: ?[]const u8) anyerror!void {
            const T = @TypeOf(value);
            const T_info = @typeInfo(T);

            // Field + Type
            try self.appendFieldTypeN(field_name, T);

            // String
            if (T_info.Array.child == u8 and T_info.Array.sentinel != null and value[value.len] == 0) {
                var buf: [256]u8 = undefined;
                var fmt: []const u8 = undefined;
                fmt = try std.fmt.bufPrint(&buf, "\"{s}\"", .{value});
                try self.appendValueNestOnceN(fmt);
            }

            // Other
            else {
                self.incNestLevel();
                for (value) |item|
                    try self.dump(item, null);
                self.decNestLevel();
            }
        }

        pub fn dumpUnion(self: *Self, value: anytype, field_name: ?[]const u8) anyerror!void {
            const T = @TypeOf(value);
            const Tag = @typeInfo(T).Union.tag_type;

            // Field + Type
            try self.appendFieldTypeN(field_name, T);

            // Tagged union
            if (Tag) |tag_type| {
                self.incNestLevel();
                switch (@as(tag_type, value)) {
                    inline else => |tag| {
                        try self.dump(@field(value, @tagName(tag)), @tagName(tag));
                    },
                }
                self.decNestLevel();
            }

            // Normal one
            else {
                try self.appendValueNestOnceN("?");
            }
        }

        pub fn dumpEnum(self: *Self, value: anytype, field_name: ?[]const u8) anyerror!void {
            const T = @TypeOf(value);

            // Field + Type
            try self.appendFieldTypeN(field_name, T);

            // Value
            try self.appendValueNestOnceN(@tagName(value));
        }

        pub fn dumpNull(self: *Self, value: anytype, field_name: ?[]const u8) anyerror!void {
            const T = @TypeOf(value);
            try self.appendFieldTypeN(field_name, T);
            try self.appendValueNestOnceN("null");
        }

        pub fn dumpOptional(self: *Self, value: anytype, field_name: ?[]const u8) anyerror!void {
            const T = @TypeOf(value);

            // Field + Type
            try self.appendFieldTypeN(field_name, T);

            // Value
            if (value) |unwrapped| {
                self.incNestLevel();
                try self.dump(unwrapped, null);
                self.decNestLevel();
            } else {
                try self.appendValueNestOnceN("null");
            }
        }

        pub fn dumpType(self: *Self, comptime value: type, field_name: ?[]const u8) anyerror!void {
            // Field + Type
            try self.appendFieldTypeN(field_name, value);

            // Value
            try self.appendValueNestOnceN(comptime trimParensCT(@typeName(value), .{ .trim_lvl = 1, .brackets = .Curly }));
        }

        pub fn dumpLiteral(self: *Self, value: anytype, field_name: ?[]const u8) anyerror!void {
            const T = @TypeOf(value);
            var buf: [256]u8 = undefined;
            var fmt: []const u8 = undefined;

            // Field + Type
            try self.appendFieldTypeN(field_name, T);

            // Value
            fmt = try std.fmt.bufPrint(&buf, "{any}", .{value});
            try self.appendValueNestOnceN(fmt);
        }

        pub fn dumpStruct(self: *Self, value: anytype, field_name: ?[]const u8) anyerror!void {
            const T = @TypeOf(value);

            // Field + Type
            try self.appendFieldTypeN(field_name, T);

            // Per each struct fields
            self.incNestLevel();
            inline for (std.meta.fields(T)) |field| {
                try self.dump(@field(value, field.name), field.name);
            }
            self.decNestLevel();
        }

        pub fn dump(self: *Self, value: anytype, field_name: ?[]const u8) anyerror!void {
            const T = @TypeOf(value);
            const T_info = @typeInfo(T);

            switch (T_info) {
                .Pointer => try self.dumpPointer(value, field_name),
                .Array => try self.dumpArray(value, field_name),
                .Struct => try self.dumpStruct(value, field_name),
                .Enum => try self.dumpEnum(value, field_name),
                .Union => try self.dumpUnion(value, field_name),
                .Int, .ComptimeInt, .Float, .ComptimeFloat, .Bool => try self.dumpLiteral(value, field_name),
                .Type => try self.dumpType(value, field_name),
                .Null => try self.dumpNull(value, field_name),
                .Optional => try self.dumpOptional(value, field_name),
                else => {
                    try self.appendValueNestOnceN("Error.ValueTypeNotSupported(" ++ @typeName(T) ++ ")");
                },
            }
        }

        pub fn deinit(self: *Self) void {
            self.out.deinit();
        }
    };

    var p = PrettyPrinter.init(allocator);
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

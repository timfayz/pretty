const std = @import("std");

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


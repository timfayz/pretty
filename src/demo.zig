const std = @import("std");
// Step 0: Import pretty
const pretty = @import("pretty.zig");
const alloc = std.heap.c_allocator;

// Step 1: Define a structure
const NodePtr = *const Node;
const Node = struct {
    value: []const u8,
    // List of nullable node pointers
    next: []const ?NodePtr = &.{null},
};

pub fn main() !void {
    // Step 2: Initialize it
    const leaf = Node{ .value = "leaf" };
    var tree = Node{ .value = "root" };
    tree.next = &.{ &leaf, &leaf, null };

    // Step 3: Print!
    try pretty.print(alloc, tree, .{});

    // Step 4: Try options (eg. to reduce the output)
    try pretty.print(alloc, tree, .{
        // Do not show item indices
        .arr_show_item_idx = false,
        // Interpret pointers as address values
        .ptr_deref = false,
        // Exclude some fields from the output
        .filter_field_names = .{ .exclude = &.{"value"} },
        // Treat optionals as primitives to inline .next items
        .arr_prim_types = .{ .include = &.{.Optional} },
    });

    // Step 5: Do not print, use it as string!
    var out = try pretty.dump(alloc, tree, .{});
    defer alloc.free(out);
    std.debug.print("{s}..\n", .{out[0..9]});

    // Bonus: Try the default printer instead
    // std.debug.print("\n{any}\n", .{tree});
}

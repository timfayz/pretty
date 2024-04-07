// MIT License (c) Timur Fayzrakhmanov.
// tim.fayzrakhmanov@gmail.com (github.com/timfayz)

const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const meta = std.meta;

/// Pretty formatting options.
pub const Options = struct {

    // [Generic printing options]

    /// Activate single line printing mode.
    inline_mode: bool = false,
    /// Limit the printing depth (0 does not limit).
    max_depth: u8 = 10,
    /// Specify depths to include or exclude from the output: `Filter(usize)`.
    filter_depths: Filter(usize) = .{ .exclude = &.{} },
    /// Indentation size for multi-line printing mode.
    tab_size: u8 = 2,
    /// Add extra empty line at the end of the output (to stack up multiple prints).
    empty_line_at_end: bool = false,
    /// Indicate empty output with a message (otherwise leave as `""`).
    indicate_empty_output: bool = true,
    /// Specify a custom format string (eg. `"pre{s}post"`) to surround the resulting output.
    fmt: []const u8 = "",

    // [Generic type printing options]

    /// Display type tags (ie. `std.builtin.TypeId`, such as `.Union`, `.Int`).
    show_type_tags: bool = false,
    /// Display type names.
    show_type_names: bool = true,
    /// Limit the length of type names (0 does not limit).
    type_name_max_len: usize = 60,
    /// Specify level of folding brackets in type names with `..` (0 does not fold).
    type_name_fold_brackets: usize = 1,
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
    /// Display pointer addresses (TODO).
    ptr_show_addr: bool = true,

    // [Optional printing options]

    /// Reduce duplicating depths when unfolding optional types.
    optional_skip_dup_unfold: bool = true,

    // [Struct and unions printing options]

    /// Display struct fields.
    struct_show_field_names: bool = true,
    /// Treat empty structs as having `(empty)` value.
    struct_show_empty: bool = true,
    /// Inline primitive type values to save vertical space.
    struct_inline_prim_types: bool = true,
    /// Limit the number of fields in the output (0 does not limit).
    struct_max_len: usize = 15,
    /// Specify field names to include or exclude from the output.
    filter_field_names: Filter([]const u8) = .{ .exclude = &.{} },
    /// Specify field type tags to include or exclude from the output.
    filter_field_type_tags: Filter(std.builtin.TypeId) = .{ .exclude = &.{} },
    /// Specify field types to include or exclude from the output.
    filter_field_types: Filter(type) = .{ .exclude = &.{} },

    // [Array and slice printing options]

    /// Limit the number of items in the output (0 does not limit).
    array_max_len: usize = 20,
    /// Display item indices.
    array_show_item_idx: bool = true,
    /// Inline primitive type values to save vertical space.
    array_inline_prim_types: bool = true,
    /// Display primitive type names.
    array_hide_prim_types: bool = true,

    // [Primitive types options]

    /// Specify type tags to treat as primitives.
    prim_type_tags: Filter(std.builtin.TypeId) = .{ .include = &.{
        .Int,
        .ComptimeInt,
        .Float,
        .ComptimeFloat,
        .Void,
        .Bool,
    } },
    /// Specify concrete types to treat as primitives.
    prim_types: Filter(type) = .{ .include = &.{} },

    // [String printing options]

    /// Limit the length of strings (0 does not limit).
    str_max_len: usize = 80,
    /// Treat `[]u8` as `"string"`.
    slice_u8_is_str: bool = true,
    /// Treat `[:0]u8` as `"string"`.
    slice_u8z_is_str: bool = true,
    /// Treat `[n]u8` as `"string"`.
    array_u8_is_str: bool = false,
    /// Treat `[n:0]u8` as `"string"`.
    array_u8z_is_str: bool = true,

    // TODO
    // show_colors
    // solo_mode (for certain fields, recursively)
    // ?inline_unions = true
    // ?inline_small_struct = true
    // ?small_struct_size = 3
    show_tree_lines: bool = false, // '├' '─' '│' '└'
};

/// Prints pretty formatted string for an arbitrary input value.
pub fn print(allocator: Allocator, value: anytype, comptime options: Options) !void {
    var output = try dumpAsList(allocator, value, options);
    defer output.deinit();

    // [Option] If custom formatting is not specified
    if (options.fmt.len == 0) {
        // [Option] Insert an extra newline (to stack up multiple outputs)
        try output.appendSlice(if (options.empty_line_at_end) "\n\n" else "\n");
    }

    std.debug.print("{s}", .{output.items});
}

/// Prints pretty formatted string for an arbitrary input value (forced inline mode).
pub fn printInline(alloc: Allocator, value: anytype, comptime options: Options) !void {
    comptime var copy = options;
    copy.inline_mode = true;
    try print(alloc, value, copy);
}

/// Generates a pretty formatted string and returns it as a []u8 slice.
pub fn dump(allocator: Allocator, value: anytype, comptime options: Options) ![]u8 {
    var list = try dumpAsList(allocator, value, options);
    return list.toOwnedSlice();
}

/// Generates a pretty formatted string and returns it as std.ArrayList interface.
pub fn dumpAsList(allocator: Allocator, value: anytype, comptime options: Options) !ArrayList(u8) {
    var pretty = Pretty(options).init(allocator);
    try pretty.traverse(value, .{});

    // [Option] Indicate empty output
    if (pretty.writer.items.len == 0 and options.indicate_empty_output)
        try pretty.writer.appendSlice("(empty output)");

    // [Option] Apply custom formatting if specified
    if (options.fmt.len > 0) {
        const fmt_output = try std.fmt.allocPrint(allocator, options.fmt, .{pretty.writer.items});
        pretty.writer.deinit(); // release original output
        return std.ArrayList(u8).fromOwnedSlice(allocator, fmt_output);
    }

    return pretty.writer;
}

// TODO
// pub fn write(writer: anytype, value: anytype, comptime options: Options) !void {
//     const out = try dump(alloc, value, options);
//     defer alloc.free(out);
// }

/// pretty implementation structure.
fn Pretty(options: Options) type {
    if (options.tab_size < 1) @compileError(".tab_size cannot be less than 1.");
    return struct {
        const Self = @This();
        alloc: Allocator,
        writer: ArrayList(u8),

        /// Array/slice indices used between recursive calls.
        idx: usize = 0,

        /// Token written during the last recursive call.
        last_tkn: Token = .None,
        last_child: bool = false,

        /// Types of tokens that can be written to the output.
        const Token = enum {
            // Format ::= None | `[Index]:` `Field:` `[Tag]` `Name` `*Addr` = `Value`
            None,
            InfoIndex,
            InfoField,
            InfoTag,
            InfoName,
            InfoAddr,
            Value,

            fn is(token: Token, t: Token) bool {
                return token == t;
            }

            fn isInfo(token: Token) bool {
                const tkn_int = @intFromEnum(token);
                if (tkn_int >= @intFromEnum(Token.InfoIndex) and tkn_int <= @intFromEnum(Token.InfoAddr))
                    return true;
                return false;
            }

            fn isSameOrInvalidOrder(token: Token, prev: Token) bool {
                return if (@intFromEnum(prev) >= @intFromEnum(token)) true else false;
            }
        };

        /// Pretty options.
        const opt: Options = options;

        /// Comptime context between recursive calls.
        const Ctx = struct {
            depth: usize = 0,
            depth_skip: usize = 0,
            prev_type: ?type = null,
            is_field: []const u8 = "",
            is_idx: bool = false, // idx resides on self.idx
            is_inline: bool = if (options.inline_mode) true else false,
            is_type_hidden: bool = false,

            fn setTypeHidden(c: Ctx, comptime hide: bool) Ctx {
                var upd = c;
                upd.is_type_hidden = hide;
                return upd;
            }

            fn hasTypeHidden(c: Ctx) bool {
                return c.is_type_hidden;
            }

            fn skipDepth(c: Ctx) Ctx {
                var upd = c;
                upd.depth_skip = c.depth_skip + 1;
                return upd;
            }

            fn getDepth(c: Ctx) usize {
                return c.depth -| c.depth_skip;
            }

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

            fn hasField(c: Ctx) bool {
                return c.is_field.len != 0;
            }

            fn setField(c: Ctx, comptime field: []const u8) Ctx {
                var upd = c;
                upd.is_field = field;
                return upd;
            }

            fn hasIdx(c: Ctx) bool {
                return c.is_idx;
            }

            fn setIdx(c: Ctx, comptime avail: bool) Ctx {
                var upd = c;
                upd.is_idx = avail;
                return upd;
            }

            fn setPrev(c: Ctx, comptime prev: ?type) Ctx {
                var upd = c;
                upd.prev_type = prev;
                return upd;
            }

            fn setInline(c: Ctx, comptime inl: bool) Ctx {
                var upd = c;
                upd.is_inline = inl;
                return upd;
            }

            fn prevIs(c: Ctx, comptime tag: std.builtin.TypeId) bool {
                if (c.prev_type == null) return false;
                return typeTag(c.prev_type.?) == tag;
            }

            fn prevIsSlice(c: Ctx) bool {
                if (c.prev_type == null) return false;
                return typeIsSlice(c.prev_type.?);
            }

            fn infoAvailable(c: Ctx) bool {
                return ((opt.struct_show_field_names and c.hasField()) or
                    ((opt.show_type_tags or opt.show_type_names) and !c.hasTypeHidden()) or
                    (opt.array_show_item_idx and c.hasIdx()));
            }
        };

        pub fn init(alloc: Allocator) Self {
            return Self{
                .alloc = alloc,
                .writer = ArrayList(u8).init(alloc),
            };
        }

        pub fn deinit(self: *Self) void {
            self.writer.deinit();
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

        fn multilineIndent(comptime ctx: Ctx) []const u8 {
            // [Option] Show tree lines
            if (opt.show_tree_lines and ctx.getDepth() > 0) {
                const prefix = if (ctx.is_last) "\n└" else "\n├";
                return prefix ++ ("─" ** ((ctx.getDepth() * opt.tab_size) - 1));
            } else {
                return "\n" ++ (" " ** (ctx.getDepth() * opt.tab_size));
            }
        }

        fn resolveTokenSep(self: *Self, tkn: Token, ctx: Ctx) []const u8 {
            const last_tkn = self.last_tkn;
            // If we write token for the first time
            if (last_tkn.is(.None)) {
                return "";
            }
            // If we write value token after info
            else if (last_tkn.isInfo() and tkn.is(.Value)) {
                if (ctx.is_inline) {
                    if (last_tkn.is(.InfoName)) {
                        if (ctx.is_type_hidden) return "";
                        return if (opt.inline_mode) " = " else " => ";
                    } else {
                        return " ";
                    }
                } else {
                    return comptime multilineIndent(ctx);
                }
            }
            // If we write two consecutive value or info tokens
            else if (tkn.isSameOrInvalidOrder(last_tkn)) {
                if (ctx.is_inline) {
                    return if (self.idx == 0) "" else ", ";
                } else {
                    return comptime multilineIndent(ctx);
                }
            }
            // If we write two consecutive info tokens
            else {
                return " ";
            }
        }

        fn write(self: *Self, tkn: Token, text: []const u8, ctx: Ctx) !void {
            // [Option] Stop if depth exceeds
            if (opt.max_depth > 0 and ctx.depth > opt.max_depth)
                return;

            // [Option] Stop if depth is not included
            if (!opt.filter_depths.includes(ctx.depth))
                return;

            // std.log.debug("{s}: {s}", .{ @tagName(tkn), text });

            // Resolve token separator (the most difficult part)
            const sep = self.resolveTokenSep(tkn, ctx);
            if (sep.len > 0) try self.writer.appendSlice(sep);
            try self.writer.appendSlice(text);
            self.last_tkn = tkn;
        }

        fn writeInfo(self: *Self, comptime T: type, comptime ctx: Ctx) !void {
            // [Option] Show item indices (if available)
            if (opt.array_show_item_idx and ctx.hasIdx()) {
                const index = try std.fmt.allocPrint(self.alloc, "[{d}]:", .{self.idx});
                defer self.alloc.free(index);
                try self.write(.InfoIndex, index, ctx);
            }

            // [Option] Show field name (if available)
            if (opt.struct_show_field_names and ctx.hasField()) {
                const field = try std.fmt.allocPrint(self.alloc, ".{s}:", .{ctx.is_field});
                defer self.alloc.free(field);
                try self.write(.InfoField, field, ctx);
            }

            // [Option] Show type tag (if available)
            if (opt.show_type_tags and !ctx.hasTypeHidden()) {
                const tag_name = @tagName(@typeInfo(T));
                const tag = try strEmbraceWith(self.alloc, tag_name, "[", "]");
                defer tag.deinit();
                try self.write(.InfoTag, tag.items, ctx);
            }

            // [Option] Show type name (if available)
            if (opt.show_type_names and !ctx.hasTypeHidden()) {
                const type_name = comptime fmtTypeName(T);
                try self.write(.InfoName, type_name, ctx);
            }
        }

        fn writeBracket(self: *Self, comptime bracket: enum { Open, Closed }, comptime ctx: Ctx) !void {
            // [Option] Show only in inline mode
            if (ctx.is_inline) {
                const empty_type_prefix = if (comptime !ctx.infoAvailable()) "." else "";
                if (bracket == .Open) {
                    if (self.last_tkn.isInfo() and self.last_tkn != .InfoName)
                        try self.writer.appendSlice(" ");
                    try self.writer.appendSlice(empty_type_prefix ++ "{ ");
                }
                // bracket == .Closed
                else {
                    try self.writer.appendSlice(" }");
                }
            }
        }

        fn writeValue(self: *Self, str_val: []const u8, comptime ctx: Ctx) !void {
            // [Option] Show value
            if (opt.show_vals) {
                try self.write(.Value, str_val, ctx);
            }
        }

        fn writeValueSpecial(self: *Self, comptime tag: enum { Skip, Empty, Null, Unknown }, comptime ctx: Ctx) !void {
            switch (tag) {
                .Skip => {
                    // [Option]-less Show skip sign
                    try self.writeValue("..", ctx);
                },
                .Null => {
                    // [Option]-less
                    try self.writeValue("null", ctx);
                },
                .Empty => {
                    // [Option] Show empty values
                    if (opt.show_empty_vals)
                        try self.writeValue("(empty)", ctx);
                },
                .Unknown => {
                    // [Option]-less
                    try self.writeValue("?", ctx);
                },
            }
        }

        fn writeValueString(self: *Self, str: []const u8, comptime ctx: Ctx) !void {
            // [Option] Trim string if exceeds
            const trimmed = try strTrim(self.alloc, str, opt.str_max_len, "..", .Auto);
            defer trimmed.deinit();

            // [Option]-less Embrace with double quotes
            const quoted = try strEmbraceWith(self.alloc, trimmed.items, "\"", "\"");
            defer quoted.deinit();

            try self.writeValue(quoted.items, ctx);
        }

        fn writeValueFmt(self: *Self, comptime fmt: []const u8, val: anytype, comptime ctx: Ctx) !void {
            const any = try std.fmt.allocPrint(self.alloc, fmt, .{val});
            defer self.alloc.free(any);
            try self.writeValue(any, ctx);
        }

        fn writeValueType(self: *Self, comptime T: type, comptime ctx: Ctx) !void {
            // [Option]-less Fold brackets in type names at the second level.
            const type_name = comptime strFoldBracketsCt(@typeName(T), .{
                .fold_depth = 2,
                .bracket = .Any,
            });
            try self.writeValue(type_name, ctx);
        }

        fn traverse(self: *Self, val: anytype, comptime ctx: Ctx) !void {
            const T = @TypeOf(val);
            comptime var c = ctx;

            // [Option] Stop if depth exceeds
            if (opt.max_depth > 0 and c.depth > opt.max_depth)
                return;

            // Resolve how to write INFO
            writeInfo: {
                c = c.setTypeHidden(false); // reset from previous call
                comptime var force_inline_after_info = false;

                // [Option] Exclude writing depth if filtered
                if (comptime !opt.filter_depths.includes(c.depth)) {
                    c = c.skipDepth();
                    break :writeInfo;
                }

                // Within array or slice
                if (comptime c.prevIs(.Array) or c.prevIsSlice()) {
                    // [Option] Show primitive types on the same line as index
                    if (comptime opt.array_inline_prim_types and
                        (opt.prim_type_tags.includes(typeTag(T)) or opt.prim_types.includes(T)))
                    {
                        force_inline_after_info = true;

                        // [Option] Hide primitive type names along with inlining
                        if (opt.array_hide_prim_types) {
                            c = c.setTypeHidden(true);
                            if (comptime !c.infoAvailable())
                                force_inline_after_info = false;
                        }
                    }
                }

                // Within struct
                else if (comptime c.prevIs(.Struct) or c.prevIs(.Union)) {
                    // [Option] Show primitive types on the same line as field
                    if (comptime opt.struct_inline_prim_types and
                        (opt.prim_type_tags.includes(typeTag(T)) or opt.prim_types.includes(T)))
                    {
                        force_inline_after_info = true;
                    }
                }

                // Within pointer
                else if (comptime c.prevIs(.Pointer)) {
                    // [Option] Reduce dereferencing
                    if (opt.ptr_skip_dup_unfold) {
                        // TODO? ptr_skip_unfold_ex_last and typeTag(T) != .Pointer
                        c = c.decDepth();
                        break :writeInfo;
                    }
                }

                // Within optional
                else if (comptime c.prevIs(.Optional)) {
                    // [Option] Reduce duplicate unfolding
                    if (opt.optional_skip_dup_unfold) {
                        c = c.decDepth();
                        break :writeInfo;
                    }
                }

                // Default
                try self.writeInfo(T, c);
                if (comptime !c.infoAvailable()) // if info wasn't written
                    c = c.skipDepth(); // adjust indentation
                c = c.setField("").setIdx(false); // clean
                if (force_inline_after_info) // activate inlining for primitive types
                    c = c.setInline(true);
            }
            c = c.incDepth(); // assume info is always written to go in-depth
            c = c.setPrev(T); // update previous type as current

            // Resolve how to write VALUE or jump back to the above through recursion
            switch (@typeInfo(T)) {
                // Recursive
                .Pointer => {
                    switch (@typeInfo(T).Pointer.size) {
                        .One => {
                            // [Option]-less Do not show opaque or function pointers
                            if (meta.Child(T) == anyopaque or @typeInfo(meta.Child(T)) == .Fn)
                                return;

                            // [Option] Follow the pointer
                            if (opt.ptr_deref) {
                                try self.traverse(val.*, c);
                            } else {
                                try self.writeValueFmt("{*}", val, c);
                            }
                        },
                        .Many, .C => {
                            // TODO support [*:0]u8 as string
                            try self.writeValueSpecial(.Unknown, c);
                        },
                        .Slice => {
                            // Slice is empty
                            if (val.len == 0) {
                                try self.writeValueSpecial(.Empty, c);
                                return;
                            }

                            // Slice has a single element
                            // TODO remove?
                            if (val.len == 1) {
                                // Re-interpret it as a single-item pointer
                                try self.traverse(val[0], c.setPrev(@TypeOf(&val[0])));
                                return;
                            }

                            // [Option] Interpret slice []u8 as string
                            if (opt.slice_u8_is_str and comptime meta.Child(T) == u8) {
                                try self.writeValueString(val, c);
                                return;
                            }

                            // [Option] Interpret slice [:0]u8 as string
                            if (opt.slice_u8z_is_str and
                                comptime (meta.Child(T) == u8 and meta.sentinel(T) != null and meta.sentinel(T) == 0))
                            {
                                try self.writeValueString(val, c);
                                return;
                            }

                            // Slice has multiple elements
                            try self.writeBracket(.Open, c); // inline mode only

                            // Comptime-known
                            if (isComptime(val)) {
                                inline for (val, 1..) |item, len| {
                                    // [Option] Stop if the length of a slice exceeds
                                    if (opt.array_max_len > 0 and len > opt.array_max_len)
                                        break;

                                    self.idx = len - 1;
                                    self.last_child = if (len == val.len) true else false;
                                    try self.traverse(item, c.setIdx(true));
                                }
                            }
                            // Runtime
                            else {
                                for (val, 1..) |item, len| {
                                    // [Option] Stop if the length of a slice exceeds
                                    if (opt.array_max_len > 0 and len > opt.array_max_len)
                                        break;

                                    self.idx = len - 1;
                                    self.last_child = if (len == val.len) true else false;
                                    try self.traverse(item, c.setIdx(true));
                                }
                            }
                            self.idx = 0;
                            try self.writeBracket(.Closed, c); // inline mode only
                        },
                    }
                },
                .Struct => {
                    // [Option] Show empty struct as empty value
                    if (meta.fields(T).len == 0 and opt.struct_show_empty) {
                        try self.writeValueSpecial(.Empty, c);
                        return;
                    }

                    try self.writeBracket(.Open, c); // inline mode only

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
                            try self.writeValueSpecial(.Skip, c);
                            break;
                        }

                        // Prepare next traverse context
                        self.idx = len - 1;
                        self.last_child = if (meta.fields(T).len == len) true else false;

                        try self.traverse(@field(val, field.name), c.setField(field.name));
                    }

                    self.idx = 0; // reset
                    try self.writeBracket(.Closed, c);
                },
                .Array => {
                    // [Option] Interpret array [:0]u8 as string
                    if (opt.array_u8_is_str and comptime meta.Child(T) == u8) {
                        try self.writeValueString(&val, c);
                        return;
                    }

                    // [Option] Interpret array [:0]u8 as string
                    if (opt.array_u8z_is_str and
                        comptime (meta.Child(T) == u8 and meta.sentinel(T) != null and meta.sentinel(T) == 0))
                    {
                        try self.writeValueString(&val, c);
                        return;
                    }

                    // Other
                    try self.writeBracket(.Open, c); // inline mode only
                    self.idx = 0; // reset

                    inline for (val, 1..) |item, len| {
                        // [Option] Stop if the length of an array exceeds
                        if (opt.array_max_len > 0 and len > opt.array_max_len)
                            break;

                        self.idx = len - 1;
                        self.last_child = if (len == val.len) true else false;
                        try self.traverse(item, c.setIdx(true));
                    }

                    self.idx = 0; // reset
                    try self.writeBracket(.Closed, c);
                },
                .Optional => {
                    // Optional has payload
                    if (val) |unwrapped| {
                        try self.traverse(unwrapped, c);
                        return;
                    }

                    // Optional is null
                    try self.writeValueSpecial(.Null, c);
                },
                // Non-recursive
                .Enum => {
                    const enum_name = try fmtEnumValue(self.alloc, val);
                    defer self.alloc.free(enum_name);
                    try self.writeValue(enum_name, c);
                },
                .Union => {
                    // Tagged union
                    if (@typeInfo(T).Union.tag_type) |tag_type| {
                        try self.writeBracket(.Open, c); // inline mode only
                        self.idx = 0;
                        switch (@as(tag_type, val)) {
                            inline else => |tag| {
                                try self.traverse(@field(val, @tagName(tag)), c.setField(@tagName(tag)));
                            },
                        }
                        try self.writeBracket(.Closed, c); // inline mode only
                        return;
                    }

                    // Normal one
                    try self.writeValueSpecial(.Unknown, c);
                },
                else => {
                    // Fall back to standard "{any}" formatter if it's a
                    // primitive or unsupported value
                    try self.writeValueFmt("{any}", val, c);
                },
            }
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
                // TODO
                // include/exclude_from/until/range
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
    return meta.activeTag(@typeInfo(T));
}

test typeTag {
    try std.testing.expect(typeTag(@TypeOf(typeTag)) == .Fn);
    try std.testing.expect(typeTag(@TypeOf(struct {}{})) == .Struct);
    try std.testing.expect(typeTag(@TypeOf(42)) == .ComptimeInt);
    try std.testing.expect(typeTag(@TypeOf(null)) == .Null);
}

/// Checks whether a type is a function pointer.
fn typeIsFnPtr(comptime T: type) bool {
    return @typeInfo(T) == .Pointer and @typeInfo(meta.Child(T)) == .Fn;
}

/// Checks whether a type is a slice.
fn typeIsSlice(comptime T: type) bool {
    return @typeInfo(T) == .Pointer and @typeInfo(T).Pointer.size == .Slice;
}

/// Retrieves the default value of a struct field.
fn typeDefaultValue(comptime T: type, comptime field: @TypeOf(.enum_literal)) meta.fieldInfo(T, field).type {
    const f = meta.fieldInfo(T, field);
    if (f.default_value == null) @compileError("Field doesn't have a default value");
    const dval_ptr = @as(*align(f.alignment) const anyopaque, @alignCast(f.default_value.?));
    const val = @as(*const f.type, @ptrCast(dval_ptr)).*;
    return val;
}

test typeDefaultValue {
    const Data = struct {
        val1: []const u8 = "test",
        val2: u8 = 42,
        val3: ?u8 = null,
        val4: type = ?u8,
        val5: struct {} = .{},
    };

    const equal = std.testing.expectEqual;
    try equal("test", typeDefaultValue(Data, .val1));
    try equal(42, typeDefaultValue(Data, .val2));
    try equal(null, typeDefaultValue(Data, .val3));
    try equal(?u8, typeDefaultValue(Data, .val4));
    try equal(std.meta.fieldInfo(Data, .val5).type{}, typeDefaultValue(Data, .val5));
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
const FoldBracketOptions = struct {
    fold_depth: u8 = 1, // 0 is do not fold
    bracket: enum { Round, Square, Curly, Angle, Any } = .Any,
    max_cap: usize = 32,
};

/// Folds content inside brackets with ".." based on the specified
/// configuration. Defaults are: nesting level = 1 (0 means no folding),
/// bracket type = any, and max capacity = 32. Returns unchanged input if
/// brackets are unbalanced or their nesting level, as well as the number of
/// pairs, exceeds the max capacity. (Function is comptime only)
fn strFoldBracketsCt(stream: []const u8, conf: FoldBracketOptions) []const u8 {
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

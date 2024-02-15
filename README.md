# pretty 🪶

a simple pretty printer for arbitrary data structures in [Zig](https://ziglang.org/)⚡️

designed to inspect complex (recursive and comptime-generated) tree structures.

[download latest](https://github.com/timfayz/pretty/blob/main/src/pretty.zig) (0.8.1) version of `pretty.zig` and use it directly within your project.

## demo

simplified version of [`src/demo.zig`](https://github.com/timfayz/pretty/blob/main/src/demo.zig):

```zig
// main.zig
const std = @import("std");
const pretty = @import("pretty.zig");
const alloc = std.heap.page_allocator;

pub fn main() !void {
    const array = [3]u8{ 4, 5, 6 };

    // print with default options
    try pretty.print(alloc, array, .{});

    // customized print
    try pretty.print(alloc, array, .{
        .arr_show_item_idx = false,
        .arr_max_len = 2,
    });

    // don't print, get a string!
    var out = try pretty.dump(alloc, array, .{});
    defer alloc.free(out);
    std.debug.print("{s}..\n", .{out[0..5]});
}
```

output:

```
$ zig run main.zig
[3]u8
  0: 4
  1: 5
  2: 6

[3]u8
  4
  5

[3]u8..
```

## api

pretty offers three main functions:

1. `pretty.print` – generates a string and prints it to stdout.
1. `pretty.dump` – generates a string and returns it as a `[]u8` slice (user has to manually free it).
1. `pretty.dumpAsList` – generates a string and returns it as `std.ArrayList` interface (user has to manually `deinit()` it).

## options

pretty output can be customized with the follow options (defaults are in the option code):

#### generic printing options

- limit the depth:

```
max_depth: u8 = 10
```

- specify depths to include or exclude from the output:

```
filter_depths: Filter(usize) = .{ .exclude = &.{} }
```

- indentation size for depth levels:

```
tab_size: u8 = 2
```

- add an empty line at the end of the output (to separate several prints):

```
empty_line_at_end: bool = true
```

- format for the sign used to indicate skipping:

```
skip_sign: []const u8 = ".."
```

#### generic type printing options

- display type tags:

```
show_type_tags: bool = false
```

- display type names:

```
show_type_names: bool = true
```

- limit the length of type names:

```
type_name_max_len: usize = 60
```

- fold brackets in type names (with '..'):

```
type_name_fold_brackets: bool = true
```

- do not fold brackets for function signatures:

```
type_name_fold_except_fn: bool = true
```

#### generic value printing options

- display values:

```
show_vals: bool = true
```

- display empty values:

```
show_empty_vals: bool = true
```

#### pointer printing options

- follow pointers instead of printing their address:

```
ptr_deref: bool = true
```

- reduce duplicating depths when dereferencing pointers:

```
ptr_skip_dup_unfold: bool = true
```

#### optional printing options

- reduce duplicating depths when unfolding optional types:

```
optional_skip_dup_unfold: bool = true
```

#### struct printing options

- display struct fields:

```
struct_show_fields: bool = true
```

- treat empty structs as having a 'null' value:

```
struct_show_empty: bool = true
```

- limit the number of fields in the output:

```
struct_max_len: usize = 10
```

- specify field names to include or exclude from the output:

```
filter_field_names: Filter([]const u8) = .{ .exclude = &.{} }
```

- specify field types to include or exclude from the output:

```
filter_field_types: Filter(std.builtin.TypeId) = .{ .exclude = &.{} }
```

#### array and slice printing options

- limit the number of items in the output:

```
arr_max_len: usize = 20
```

- display item indices:

```
arr_show_item_idx: bool = true
```

- display values of primitive types on the same line as the index:

```
arr_inline_prim_types: bool = true
```

- specify types to treat as primitives:

```
arr_prim_types: Filter(std.builtin.TypeId) = .{ .include = &.{
    .Int,
    .ComptimeInt,
    .Float,
    .ComptimeFloat,
    .Void,
    .Bool,
} }
```

#### string printing options

- treat []u8 as "string":

```
str_is_u8: bool = true
```

- limit the length of strings:

```
str_max_len: usize = 80
```

## examples

derived from [`src/tests.zig`](https://github.com/timfayz/pretty/blob/main/src/tests.zig):

```zig
const example_1: struct {
    field1: bool = true,
    field2: u8 = 42,
    field3: f32 = 1.1,
} = .{};
```

```zig
try pretty.print(alloc, example_1, .{});
```

```
tmp.main__struct_1652
  .field1: bool
    true
  .field2: u8
    42
  .field3: f32
    1.10000002e+00
```

use options to query and "cut off" unnecessary output:

```zig
try pretty.print(alloc, example_1, .{
    .filter_depths = .{ .exclude = &.{ 0, 2 } },
});
```

```
.field1: bool
.field2: u8
.field3: f32
```

```zig
try pretty.print(alloc, example_1, .{
      .filter_depths = .{ .include = &.{2} },
});
```

```
true
42
1.10000002e+00
```

```zig
try pretty.print(alloc, example_1, .{
    .filter_depths = .{ .exclude = &.{0} },
    .filter_field_names = .{ .include = &.{"field2"} },
});
```

```
.field2: u8
  42
```

## contributing ❤️

please feel free to:

- open an issue to discuss the change you'd like to see.
- fork the repository and submit your pull request.

## license

all codebase are belong to MIT.

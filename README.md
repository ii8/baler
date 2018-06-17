
# Baler

A tool for working with [bale](https://github.com/ii8/bale) schemas and data.

## Installing

The easiest way is to use [Stack](https://docs.haskellstack.org/):
```
stack build
sudo stack install --local-bin-path=/usr/local/bin
```

## Schema validation
By default, baler will validate a schema from stdin and output its type with
all binding calls reduced.
You can also specify the file to read the schema from.

The `-i` option is used to specify where to look for
[included files](#file-inclusion), defaults to the current working directory.

```
baler [-i include_dir] [schema_file]
```

## Encoding
With the `encode` option, baler will read data expressed in a
[diagnostic notation](#the-diagnostic-notation) and convert it to
binary using the bale encoding.

```
baler encode [input_file]
```

## Decoding
The `decode` option can be used to convert binary data to the
[diagnostic notation](#the-diagnostic-notation) according to a given schema.

The schema may also [include other files](#file-inclusion) from a given
directory, defaults to the current working directory.

Both the schema and input data default to stdin, so at least one must be
provided on the command line.

```
baler decode [-s schema_file] [-i include_dir] [data_file]
```

## File inclusion
Baler will interpret include directives in schema files written as the word
`include` followed by a double quoted filename. Baler behaves as if
the contents of the file were encountered at the point of inclusion.

Include directives must come before any let bindings and types in each file.

```
include "some-file.bale"
```

## The diagnostic notation

- `uv`s are written as normal decimal numbers: `51`.
- Fixed size numbers are postfixed with a `'` and their type: `-2.31e4'f32`.
- Tuples are written using braces `{ 1 2 }`, words ending with colons before each item are ignored.
- Unions have their index and value separated by an `@`: `2 @ 12'u8`.
- Arrays are enclosed in brackets: `[ 12 3 1 ]`
- Quotation marks can be used as a convenience for `array u8`: `"abc"`.

As an example, for this schema:
```
tuple
  n: uv
  i: i64
  b: bool
  s: string
end
```
you can write:
```
{
  n: 12
  i: 99'i64
  b: 1@{}
  s: "lel"
}
```


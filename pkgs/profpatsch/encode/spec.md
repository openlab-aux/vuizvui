# encode 0.1-unreleased

[bencode][] and [netstring][]-inspired pipe format that should be trivial to parse (100 lines of code or less), mostly human-decipherable for easy debugging, and support nested record and sum types.


## scalars

Scalars have the format `[type prefix][size]:[value],`.

### unit

The unit (`u`) has only one value.

The unit is: `u,`

### numbers

Naturals (`n`) and Integers (`i`), with a maximum size in bits.

The allowed bit sizes are: 8, 16, 32, 64, 128. (TODO: does that make sense?)

Natural `1234` that fits in 32 bits: `n32:1234,`
Integer `-42` that fits in 8 bits: `i8:-42,`
Integer `23` that fits in 64 bits: `i64:23,`

Floats elided by choice.

### text

Text (`t`) that *must* be encoded as UTF-8, starting with its length in bytes:

The string `hello world` (11 bytes): `t11:hello world,`
The string `今日は` (9 bytes): `t9:今日は,`
The string `:,` (2 bytes): `t2::,,`
The empty sting `` (0 bytes): t0:,`

Binary data elided by choice.


## tagged values

### tags

A tag (`<`) gives a value a name. The tag is UTF-8 encoded, starting with its length in bytes and proceeding with the value.

The tag `foo` (3 bytes) tagging the text `hello` (5 bytes): `<3:foo|t5:hello,`
The tag `` (0 bytes) tagging the 8-bit integer 0: `<0:|i8:0,`

### products (dicts/records), also maps

Multiple tags concatenated, if tag names repeat the later ones should be ignored. (TODO: should there be a marker indicating products, and should maps get a different marker? We don’t have a concept of types here, so probably not.)
Ordering does not matter.

### sums (tagged unions)

Simply a tagged value (TODO: should there be a marker?).


## lists

TODO: necessary?

A list (`[`) imposes an ordering on a sequence of values. It needs to be closed with `]`. Values in it are simply concatenated.

The empty list: `[]`
The list with one element, the string `foo`: `[t3:foo,]`
The list with text `foo` followed by i8 `-42`: `[t3:foo,i8:-42,]`
The list with `Some` and `None` tags: `[<4:Some|t3:foo,<4None|u,<4None|u,]`


## motivation

Using

## guarantees

TODO: do I want unique representation (bijection like bencode?) This would put more restrictions on the generator, like sorting records in lexicographic order, but would make it possible to compare without decoding


[bencode]: https://en.wikipedia.org/wiki/Bencode
[netstring]: https://en.wikipedia.org/wiki/Netstring

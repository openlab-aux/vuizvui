# netencode 0.1-unreleased

[bencode][] and [netstring][]-inspired pipe format that should be trivial to parse (100 lines of code or less), mostly human-decipherable for easy debugging, and support nested record and sum types.


## scalars

Scalars have the format `[type prefix][size]:[value],`.

where size is a natural number without leading zeroes.

### unit

The unit (`u`) has only one value.

* The unit is: `u,`

### numbers

Naturals (`n`) and Integers (`i`), with a maximum size in bits.

Bit sizes are specified in 2^n increments, 1 to 9 (`n1`..`n9`, `i1`..`n9`).

* Natural `1234` that fits in 32 bits (2^5): `n5:1234,`
* Integer `-42` that fits in 8 bits (2^3): `i3:-42,`
* Integer `23` that fits in 64 bits (2^6): `i6:23,`
* Integer `-1` that fits in 512 bits (2^9): `i9:-1,`
* Natural `0` that fits in 1 bit (2^1): `n1:0,`

An implementation can define the biggest numbers it supports, and has to throw an error for anything bigger. It has to support everything smaller, so for example if you support up to i6/n6, you have to support 1–6 as well. An implementation could support up to the current architecture’s wordsize for example.

Floats are not supported, you can implement fixed-size decimals or ratios using integers.

### booleans

A boolean is represented as `n1`.

* `n1:0,`: false
* `n1:1,`: true

TODO: should we add `f,` and `t,`?

### text

Text (`t`) that *must* be encoded as UTF-8, starting with its length in bytes:

* The string `hello world` (11 bytes): `t11:hello world,`
* The string `今日は` (9 bytes): `t9:今日は,`
* The string `:,` (2 bytes): `t2::,,`
* The empty sting `` (0 bytes): `t0:,`

### binary
:LOGBOOK:
CLOCK: [2020-06-26 Fr 23:21]
:END:

Arbitrary binary strings (`b`) that can contain any data, starting with its length in bytes.

* The ASCII string `hello world` as binary data (11 bytes): `b11:hello world,`
* The empty binary string (0 bytes): `b0:,`
* The bytestring with `^D` (1 byte): `b1:,`

Since the binary strings are length-prefixd, they can contain `\0` and no escaping is required. Care has to be taken in languages with `\0`-terminated bytestrings.

Use text (`t`) if you have utf-8 encoded data.

## tagged values

### tags

A tag (`<`) gives a value a name. The tag is UTF-8 encoded, starting with its length in bytes and proceeding with the value.

* The tag `foo` (3 bytes) tagging the text `hello` (5 bytes): `<3:foo|t5:hello,`
* The tag `` (0 bytes) tagging the 8-bit integer 0: `<0:|i3:0,`

### records (products/records), also maps

A record (`{`) is a concatenation of tags (`<`). It needs to be closed with `}`.
If tag names repeat the later ones should be ignored. Ordering does not matter.

Similar to text, records start with the length of their *whole encoded content*, in bytes. This makes it possible to treat their contents as opaque bytestrings.

* There is no empty record. (TODO: make the empty record the unit type, remove `u,`?)
* A record with one empty field, `foo`: `{9:<3:foo|u,}`
* A record with two fields, `foo` and `x`: `{21:<3:foo|u,<1:x|t3:baz,}`
* The same record: `{21:<1:x|t3:baz,<3:foo|u,}`
* The same record (later occurences of fields are ignored): `{28:<1:x|t3:baz,<3:foo|u,<1:x|u,}`

### sums (tagged unions)

Simply a tagged value. The tag marker `<` indicates it is a sum if it appears outside of a record.

## lists

A list (`[`) imposes an ordering on a sequence of values. It needs to be closed with `]`. Values in it are simply concatenated.

Similar to records, lists start with the length of their whole encoded content.

* The empty list: `[0:]`
* The list with one element, the string `foo`: `[7:t3:foo,]`
* The list with text `foo` followed by i3 `-42`: `[14:t3:foo,i3:-42,]`
* The list with `Some` and `None` tags: `[33:<4:Some|t3:foo,<4None|u,<4None|u,]`

## motivation

TODO

## guarantees

TODO: do I want unique representation (bijection like bencode?) This would put more restrictions on the generator, like sorting records in lexicographic order, but would make it possible to compare without decoding


[bencode]: https://en.wikipedia.org/wiki/Bencode
[netstring]: https://en.wikipedia.org/wiki/Netstring

# tep — tep emoji picker

`tep` is a simple (maybe even trivial) emoji picker.
It lets you search for emojis using [`bemenu`](https://github.com/Cloudef/bemenu)
and the selected one then can be copied to your clipboard
or printed to `stdout`.

It's main strength is that the list of emojis is generated
(at build time) from `emoji-test.txt`, a file provided by
the Unicode Consortium for every emoji version. It describes
what the Unicode Consortium thinks should be a emoji keyboard.
See [Technical Standard #51](https://unicode.org/reports/tr51/index.html#Data_Files)
for a (pretty unenlightening) description of the emoji data
files.

This way we get a list of emojis associated with their CLDR
short name and their group and subgroup in the keyboard
(e. g. Travel & Transport → transport-air).

The list includes the following types of emojis or rather
character sequences:

* fully qualified and minimally qualified emojis: these are
  what you would generally consider an emoji: a character
  or sequence of characters that show up as a single emoji
  like 🥺 and 👩🏿‍🦰.
* unqualified emojis: are characters which don't have a
  emoji representation, usually it is required to use them
  as part of a emoji presentation sequence to force them
  to be rendered as an emoji. Probably will be filtered
  out in the future.
* components: These change other emojis by changing their
  skin tone or hair etc.

## usage

```
tep [copy] [bemenu args]
```

* `copy` is the first argument, `tep` will copy the emoji
  to clip board, otherwise its printed to `stdout`
* any other arguments are passed to `bemenu`.

```
tep copy -l 25 -i
```

Shows you 25 emojis vertically at once and lets you search case
insensitively through it and will copy it to your clipboard
as soon as you hit enter.

## tep-data

`tep-data` is the core component of `tep`. It is a small Haskell
program which reads `emoji-test.txt` from `stdin` and converts it
to a searchable format for `bemenu`. To improve performance and to
limit run time dependencies it is only run at build time.

It uses the `emoji-test.txt` parser from [`emoji-generic`](../../lib/hs/emoji-generic)
apart from that it depends on `bytestring`, `text`, `attoparsec` and
`utf8-light`. If you want to use it separately, it can be built
from `tepData.hs`.

The format is not stable, but it's guaranteed that each line starts
with the emoji it describes which is terminated by an ascii space
(`0x20`).

## building / configuration

`tep` is currently built with `nix`. For default settings
run `nix-build -A tep` from the root of this repository.

The derivation can be called with the following optional
arguments to customize its behavior:

* `copy`: command to call to copy the emoji to the clipboard,
  defaults to `wl-copy --trim-newline` for wayland support
* `fromTep`: command to use to extract the emoji from the
  tep data line, defaults to `cut -d' ' -f1`
* `emojiTestTxt`: derivation for `emoji-test.txt` to be used

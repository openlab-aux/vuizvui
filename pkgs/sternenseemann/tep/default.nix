{ writeHaskell, writeBashBin, writeText, runCommandLocal
, emoji-generic, utf8-light, attoparsec, text, bytestring
, bemenu, dmenu
, fromTep ? "cut -d' ' -f1"
, copy ? "wl-copy --trim-newline"
, emojiTestTxt
}:

let
  tepData = writeHaskell "tep-data" {
    libraries = [ emoji-generic utf8-light attoparsec text bytestring ];
  } ./tepData.hs;

  static = writeText "static-tep.txt" ''
    · dot time character
  '';

  emojis = runCommandLocal "emojis.txt" {} ''
    ${tepData} < ${emojiTestTxt} > tep-data.txt
    cat ${static} tep-data.txt > "$out"
  '';

  makeTep = menu: writeBashBin "tep" ''
    copy=false
    if [[ "$1" = "copy" ]]; then
      copy=true
      shift
    fi
    ${menu} $@ < ${emojis} | ${fromTep} | \
    if $copy; then
      ${copy}
    else
      cat
    fi
  '';
in

{
  wayland = makeTep "${bemenu}/bin/bemenu";
  x11 = makeTep "${dmenu}/bin/dmenu";
}

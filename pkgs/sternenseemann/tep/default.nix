{ writeHaskell, writeBashBin, writeText, runCommandLocal
, emoji-generic, utf8-light, attoparsec, text, bytestring
, bemenu
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
in

writeBashBin "tep" ''
  copy=false
  if [[ "$1" = "copy" ]]; then
    copy=true
    shift
  fi
  ${bemenu}/bin/bemenu $@ < ${emojis} | ${fromTep} | \
  if $copy; then
    ${copy}
  else
    cat
  fi
''

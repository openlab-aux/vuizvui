{ writeBashBin, writeText, packageScriptFile, runCommandNoCC
, lib
, gawk
, bemenu
, fromTep ? "cut -d' ' -f1"
, copy ? "wl-copy --trim-newline"
, emojiTestTxt
}:

let
  tep-data = packageScriptFile {
    name = "tep-data";
    interpreter = gawk;
    file = ./tep-data.awk;
  };

  static = writeText "static-tep.txt" ''
    · dot time character
  '';

  emojis = runCommandNoCC "emojis.txt" {} ''
    ${lib.getExe tep-data} < ${emojiTestTxt} > tep-data.txt
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

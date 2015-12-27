{ stdenv, writeScriptBin, gtkdialog, qrencode }:

let script = writeScriptBin "show-qr-code" ''
  #!/bin/sh
  TMP=$(mktemp)
  ${qrencode}/bin/qrencode -s 8 -o "$TMP" -t PNG "$1"

  export DIALOG='
  <vbox>
      <pixmap>
          <input file>'$TMP'</input>
      </pixmap>
  </vbox>
  '

  ${gtkdialog}/bin/gtkdialog --program=DIALOG > /dev/null &

  sleep 0.2

  rm "$TMP"

  '';

in script // {
  meta = {
    description = "Show the given string as qr code in a gtk window";
  };
}

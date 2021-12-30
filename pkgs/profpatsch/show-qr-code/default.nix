{ stdenv, writeScriptBin, gtkdialog, qrencode }:

let script = writeScriptBin "show-qr-code" ''
  #!/bin/sh
  TMP=$(mktemp)
  trap 'rm "$TMP"' EXIT
  ${qrencode}/bin/qrencode -s 8 -o "$TMP" -t PNG

  export DIALOG='
  <vbox>
      <pixmap>
          <input file>'$TMP'</input>
      </pixmap>
  </vbox>
  '

  ${gtkdialog}/bin/gtkdialog --program=DIALOG > /dev/null &

  sleep 0.2


  '';

in script // {
  meta = {
    description = "Show the given string as qr code in a gtk window";
  };
}

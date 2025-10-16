{ pkgs, ... }:

let script = pkgs.writeScriptBin "show-qr-code" ''
  #!/bin/sh
  TMP=$(mktemp)
  trap 'rm "$TMP"' EXIT

  if [ "$1" = "" ]; then
    # read from stdin
    ${pkgs.qrencode}/bin/qrencode -s 8 --8bit -o "$TMP" -t PNG
  else
    # read from first arg
    ${pkgs.qrencode}/bin/qrencode -s 8 -o "$TMP" -t PNG "$1"
  fi

  export DIALOG='
  <vbox>
      <pixmap>
          <input file>'$TMP'</input>
      </pixmap>
  </vbox>
  '

  ${pkgs.gtkdialog}/bin/gtkdialog --program=DIALOG > /dev/null &

  sleep 0.2


  '';

in script // {
  meta = {
    description = "Show the given string as qr code in a gtk window";
  };
}

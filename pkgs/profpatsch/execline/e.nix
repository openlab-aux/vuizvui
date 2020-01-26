{ writeExecline, getBins, pkgs }:
let

  bins = getBins pkgs.rlwrap [ "rlwrap" ]
    // getBins pkgs.s6-portable-utils [ { use = "s6-cat"; as = "cat"; } ]
    // getBins pkgs.execline [ "execlineb" ];

  # minimal execline shell
  e = writeExecline "e" {} [
    bins.rlwrap
      "--substitute-prompt" "e> "
      "--remember"
      "--quote-characters" "\""
      "--complete-filenames"
    "forstdin" "-d\n" "cmd"
    "importas" "cmd" "cmd"
    bins.execlineb "-Pc" "$cmd"
  ];

in { inherit e; }

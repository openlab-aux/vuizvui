{ writeExecline, getBins, pkgs }:
let

  bins = getBins pkgs.rlwrap [ "rlwrap" ]
    // getBins pkgs.s6-portable-utils [ { use = "s6-cat"; as = "cat"; } ]
    // getBins pkgs.execline [ "execlineb" ];

  # minimal execline shell
  e =
    let
      prompt = [ "if" [ "printf" ''\e[0;33me>\e[0m '' ] ];
    in
      writeExecline "e" {} ([
        bins.rlwrap
          "--remember"
          "--quote-characters" "\""
          "--complete-filenames"
      ] ++ prompt ++ [
        "forstdin" "-d\n" "cmd"
        "importas" "cmd" "cmd"
        "foreground" [ bins.execlineb "-Pc" "$cmd" ]
      ] ++ prompt);

in { inherit e; }

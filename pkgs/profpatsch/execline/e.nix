{ writeExecline, getBins, pkgs }:
let

  bins = getBins pkgs.rlwrap [ "rlwrap" ]
    // getBins pkgs.s6-portable-utils [ { use = "s6-cat"; as = "cat"; } "s6-test" ]
    // getBins pkgs.execline [ "execlineb" ];

  # minimal execline shell
  shell =
    let
      prompt = [ "if" [ "printf" ''\e[0;33me>\e[0m '' ] ];
    in
      writeExecline "execline-shell" {} ([
        bins.rlwrap
          "--remember"
          "--quote-characters" "\""
          "--complete-filenames"
          "--ansi-colour-aware"
      ] ++ prompt ++ [
        "forstdin" "-d\n" "cmd"
        "importas" "cmd" "cmd"
        "foreground" [ bins.execlineb "-Pc" "$cmd" ]
      ] ++ prompt);

  # TODO: should definitely still pass a command line to rlwrap, because then it keeps the history!
  e = writeExecline "e" { argMode = "env"; } [
    # if there is no argument, we start the shell, else we call execlineb directly
    "ifelse" [ "importas" "#" "#" bins.s6-test "$#" "=" "0" ]
    [ shell ]
    # call execlineb with the arguments as script
    "backtick" "-i" "cmd" [ "dollarat" "-d" " " ]
    "importas" "-ui" "cmd" "cmd"
    bins.execlineb "-Pc" "$cmd"
  ];

in { inherit e; }

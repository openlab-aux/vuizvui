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

  e = pkgs.writers.writeDash "e" ''
    set -e
    # if there is no argument, we start the shell, else we call execlineb directly
    if [ $# -eq 0 ]; then
      ${shell}
    else
      cmd=
      # substitute "[" and "]" to execline’s "{" and "}"
      for arg in "$@"; do
        if [ "$arg" = "[" ]; then
          cmd="$cmd {"
        else if [ "$arg" = "]" ]; then
          cmd="$cmd }"
        else
          cmd="$cmd $arg"
        fi; fi
      done
      # call execlineb with the arguments as script
      ${bins.execlineb} -Pc "$cmd"
    fi
  '';

in { inherit e; }

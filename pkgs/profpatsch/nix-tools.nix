{ pkgs, writeExecline, getBins, runblock }:

let
  bins = getBins pkgs.nix [ "nix-build" "nix-instantiate" ];

  # Usage (execline syntax):
  #    nix-run { -A foo <more_nix_options> } args...
  #
  # Takes an execline block of `nix-build` arguments, which should produce an executable store path.
  # Then runs the store path with `prog...`.
  nix-run = writeExecline "nix-run" { argMode = "env"; } [
    "backtick" "-iE" "storepath" [
      runblock "1" bins.nix-build
    ]
    runblock "-r" "2" "$storepath"
  ];

  # Usage (execline syntax):
  #    nix-run-bin { -A foo <more_nix_options> } <foo_bin_name> args...
  #
  # Takes an execline block of `nix-build` arguments, which should produce a store path with a bin/ directory in it.
  # Then runs the `/bin/<foo_bin_name>` executable in the store path with the given arguments.
  nix-run-bin = writeExecline "nix-run-bin" { argMode = "env"; } [
    "backtick" "-i" "storepath" [
      runblock "1" bins.nix-build
    ]
    runblock "-r" "2"
      # workaround, runblock does not set # and $0 and so forth in its blocks (yet)
      (writeExecline "nix-run-bin-rest-block" { readNArgs = 1; } [
        "importas" "-ui" "storepath" "storepath"
        "if" [ "echo" "\${storepath}/bin/\${1}" ]
        "\${storepath}/bin/\${1}" "$@"
      ])
  ];

  nix-eval = writeExecline "nix-eval" {} [
    bins.nix-instantiate
      "--read-write-mode"
      "--eval"
      "--strict"
      "$@"
  ];

in {
  inherit
    nix-run
    nix-run-bin
    nix-eval
    ;
}

{ pkgs, writeExecline, getBins, runblock }:

let
  bins = getBins pkgs.nix [ "nix-build" "nix-instantiate" ];

  # TODO: both of these don’t prevent `result` from being created. good? bad?

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
  # Then runs the given command line with the given arguments. All executables in the built storepath’s bin directory are prepended to `PATH`.
  nix-run-bin = writeExecline "nix-run-bin" { argMode = "env"; } [
    "backtick" "-iE" "storepath" [
      runblock "1" bins.nix-build
    ]
    "importas" "-ui" "PATH" "PATH"
    "export" "PATH" "\${storepath}/bin:\${PATH}"
    runblock "-r" "2"
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

{ pkgs, writeExecline, backtick, runblock, getBins }:

let
  bins = getBins pkgs.nix [ "nix-build" "nix-instantiate" ];

  nix-run = writeExecline "nix-run" { argMode = "env"; } [
    (backtick {
      var = "storepath";
      cmd = [ runblock "1" bins.nix-build ];
    })
    runblock "-r" "2" "exec" "$storepath"
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
    nix-eval
    ;
}

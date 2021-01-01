{ pkgs, writeExecline, backtick, runblock, getBins }:

let
  bins = getBins pkgs.nix [ "nix-build" ];

  nix-run = writeExecline "nix-run" { argMode = "env"; } [
    (backtick {
      var = "storepath";
      cmd = [ runblock "1" bins.nix-build ];
    })
    runblock "-r" "2" "exec" "$storepath"
  ];

in {
  inherit
    nix-run
    ;
}

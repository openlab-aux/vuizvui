{ stdenv, lib, pkgs, sternenseemann, lazy-packages }:

let
  getBins = import ./getBins.nix { inherit lib; };

  testing = import ./testing { inherit (pkgs) runCommandLocal; inherit lib; };

  writeRust = import ./write-rust.nix {
    inherit pkgs; inherit (testing) drvSeqL;
  };

  nman = import ./nman {
    inherit lib;
    inherit (sternenseemann) temp;
    inherit (writeRust) writeRustSimpleBin;
  };
in

{
  # Re-export for use by other packages
  inherit getBins nman;
  inherit (writeRust) writeRustSimple writeRustSimpleBin writeRustSimpleLib;
}

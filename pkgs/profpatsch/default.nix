{ stdenv, lib, pkgs, sternenseemann, lazy-packages }:

let
  inherit (pkgs) callPackage;

  getBins = import ./getBins.nix { inherit lib; };

  # various nix utils and fun experiments
  nixperiments = import (pkgs.fetchFromGitHub {
    owner = "Profpatsch"; repo = "nixperiments";
    rev = "e04abced1a4fef7ad63f67ec621f6484226ed104";
    sha256 = "1if9szjj1g1l5n31pmbiyp5f3llwl6xlqxklc6g2xf4rq59d2d6w";
  }) { nixpkgs = pkgs; };

  writeRust = import ./write-rust.nix {
    inherit pkgs; inherit (nixperiments) drvSeqL;
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

{ stdenv, lib, pkgs, sternenseemann, lazy-packages }:

let
  inherit (pkgs) callPackage;

  readTree = import ../../lib/readTree {};

  # Manual imports of utilities for use within default.nix itself
  # (These are also auto-discovered by readTree for use by other packages)
  getBins = import ./utils/getBins.nix { inherit lib; };

  exactSource = import ./exact-source.nix;

  # various nix utils and fun experiments
  nixperiments = import (pkgs.fetchFromGitHub {
    owner = "Profpatsch"; repo = "nixperiments";
    rev = "e04abced1a4fef7ad63f67ec621f6484226ed104";
    sha256 = "1if9szjj1g1l5n31pmbiyp5f3llwl6xlqxklc6g2xf4rq59d2d6w";
  }) { nixpkgs = pkgs; };

  writeRust = import ./write-rust.nix {
    inherit pkgs; inherit (nixperiments) drvSeqL;
  };

  # All utility functions and libraries that packages might need
  utilityArgs = {
    inherit stdenv lib pkgs sternenseemann lazy-packages;
    inherit exactSource;
    inherit (nixperiments) match script drvSeq drvSeqL withTests filterSourceGitignoreWith readGitignoreFile;
    inherit (writeRust) writeRustSimple writeRustSimpleBin writeRustSimpleLib;
    inherit (sternenseemann) temp;  # For nman
  };

in readTree.fix (self: let
  # Discover all packages using readTree
  discovered = readTree {
    path = ./.;
    args = utilityArgs // {
      profpatsch = self;
    };
  };

in discovered // {
  # Re-export for use by other packages
  inherit getBins;
  inherit (writeRust) writeRustSimple writeRustSimpleBin writeRustSimpleLib;
})

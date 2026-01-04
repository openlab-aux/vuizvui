{ stdenv, lib, pkgs, sternenseemann, lazy-packages }:

let
  inherit (pkgs) callPackage;

  readTree = import ../../lib/readTree {};

  haskellPackagesProfpatsch = import ./haskell-overlay.nix { inherit pkgs; };

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

  testing = import ./testing {
    inherit lib; inherit (runExeclineFns) runExecline; inherit (pkgs) runCommandLocal;
    bin = getBins pkgs.s6-portable-utils [ "s6-touch" "s6-echo" ];
  };

  runExeclineFns = import ./execline/runExeclineFns.nix {
    inherit stdenv lib pkgs getBins testing;
  };

  writeExeclineFns = callPackage ./execline/write-execline.nix {};

  homeRepo = import ./home-repo.nix { inherit pkgs; };

  writeRust = import ./write-rust.nix {
    inherit pkgs getBins; inherit (runExeclineFns) runExeclineLocal; inherit (nixperiments) drvSeqL;
  };

  sandboxFns = import ./sandbox.nix { inherit pkgs; inherit (writeExeclineFns) writeExecline; };

  # All utility functions and libraries that packages might need
  utilityArgs = {
    inherit stdenv lib pkgs sternenseemann lazy-packages homeRepo;
    inherit exactSource;
    inherit (nixperiments) match script drvSeq drvSeqL withTests filterSourceGitignoreWith readGitignoreFile;
    inherit (runExeclineFns) runExecline runExeclineLocal runExeclineLocalNoSeqL;
    inherit (writeExeclineFns) writeExecline writeExeclineBin;
    inherit (writeRust) writeRustSimple writeRustSimpleBin writeRustSimpleLib;
    inherit (sandboxFns) sandbox runInEmptyEnv;
    inherit haskellPackagesProfpatsch testing;
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

  # Additional packages that need special handling (not auto-discovered)
  specialPackages = rec {
    # Re-exports from homeRepo
    inherit (homeRepo.users.Profpatsch) lyric alacritty;

    # .nix files that aren't auto-discovered because default.nix exists in root
    rust-deps = import ./rust-deps.nix { inherit (pkgs) buildRustCrate; };
    importPurescript = import ./importPurescript.nix { inherit pkgs exactSource; inherit (pkgs) haskellPackages; };

    # Standalone package files (import with utilityArgs + pkgs + discovered packages)
    standaloneArgs = utilityArgs // pkgs // {
      runblock = discovered.execline.runblock;
      profpatsch = self;
    };
    read-qr-code = import ./read-qr-code.nix standaloneArgs;
    read-qr-code-from-camera = import ./read-qr-code-from-camera.nix standaloneArgs;
    xrandr = import ./xrandr.nix standaloneArgs;
    deploy = (import ./deploy.nix standaloneArgs).deploy;
    nix-tools-set = import ./nix-tools.nix standaloneArgs;
    nix-run = nix-tools-set.nix-run;
    nix-run-bin = nix-tools-set.nix-run-bin;
    nix-eval = nix-tools-set.nix-eval;
    blight = import ./blight.nix standaloneArgs;
    e = (import ./execline/e.nix (standaloneArgs // { inherit (writeExeclineFns) writeExecline; })).e;

    # Packages that need complex setup
    droopy = import ./special-packages/droopy.nix { inherit pkgs; };
    gitit = import ./special-packages/gitit.nix { inherit pkgs; };
  };

in discovered // specialPackages // {
  # Re-export for backward compatibility (used by machine configs and other packages)
  inherit homeRepo getBins;
  inherit (writeExeclineFns) writeExecline writeExeclineBin;
  inherit (writeRust) writeRustSimple writeRustSimpleBin writeRustSimpleLib;
})

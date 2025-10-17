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
  nixperiments =
    let
      src = pkgs.fetchFromGitHub {
        owner = "Profpatsch";
        repo = "nixperiments";
        rev = "e04abced1a4fef7ad63f67ec621f6484226ed104";
        sha256 = "1if9szjj1g1l5n31pmbiyp5f3llwl6xlqxklc6g2xf4rq59d2d6w";
      };
    in import src { nixpkgs = pkgs; };

  testing = import ./testing {
    inherit lib;
    inherit (runExeclineFns) runExecline;
    inherit (pkgs) runCommandLocal;
    bin = getBins pkgs.s6-portable-utils [ "s6-touch" "s6-echo" ];
  };

  runExeclineFns =
    # todo: factor out calling tests
    let
      it = import ./execline/run-execline.nix {
        bin = getBins pkgs.execline [ "execlineb" "redirfd" "importas" "exec" ];
        inherit stdenv lib;
      };
      itLocal = name: args: execline:
        it name (args // {
          derivationArgs = args.derivationArgs or {} // {
            preferLocalBuild = true;
            allowSubstitutes = false;
          };
        }) execline;

      tests = import ./execline/run-execline-tests.nix {
        # can't use runExeclineLocal in the tests,
        # because it is tested by the tests (well, it does
        # work, but then you have to run the tests every time)
        runExecline = it;
        inherit (testing) drvSeqL;
        inherit (pkgs) coreutils;
        inherit stdenv;
        bin = (getBins pkgs.execline [
                 "execlineb"
                 "eltest"
                 { use = "if"; as = "execlineIf"; }
                 "redirfd" "importas"
               ])
           // (getBins pkgs.s6-portable-utils
                [ "s6-cat" "s6-grep" "s6-touch" "s6-chmod" ]);
       };
    in {
      runExecline = it;
      runExeclineLocal = name: args: execline:
        testing.drvSeqL tests (itLocal name args execline);

      # This is just a stupid workaround to prevent nix restricted mode
      # from stumbling over the symlink in the derivation output.
      runExeclineLocalNoSeqL = name: args: execline:
        itLocal name args execline;
    };

  writeExeclineFns = callPackage ./execline/write-execline.nix {};

  homeRepo = import ./home-repo.nix { inherit pkgs; };

  writeRust = import ./write-rust.nix {
    inherit pkgs getBins;
    inherit (runExeclineFns) runExeclineLocal;
    inherit (nixperiments) drvSeqL;
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
    droopy = pkgs.droopy.overrideDerivation (old: {
      src = pkgs.fetchFromGitHub {
        owner = "Profpatsch";
        repo = "Droopy";
        rev = "55c60c612b913f9fbce9fceebbcb3a332152f1a4";
        sha256 = "0jcazj9gkdf4k7vsi33dpw9myyy02gjihwsy36dfqq4bas312cq1";
      };
      installPhase = old.installPhase or "" + ''
        mkdir -p $out/share/droopy
        cp -r $src/static $out/share/droopy
      '';
      makeWrapperArgs = old.makeWrapperArgs or [] ++ [
        "--set DROOPY_STATIC \"$out/share/droopy/static\""
      ];
    });

    gitit = import (pkgs.fetchFromGitHub {
      owner = "Profpatsch";
      repo = "gitit";
      rev = "bcfba01472f09abec211c3509ec33726629068ea";
      sha256 = "sha256-v8s6GN4FTCfcsAxLdaP3HBYIDrJlZeKv+TOiRRt4bf4=";
    }) { inherit pkgs; };
  };

in discovered // specialPackages // {
  # Re-export for backward compatibility (used by machine configs and other packages)
  inherit homeRepo;
  inherit (writeExeclineFns) writeExecline writeExeclineBin;
  inherit (writeRust) writeRustSimple writeRustSimpleBin writeRustSimpleLib;
})

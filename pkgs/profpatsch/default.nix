{ stdenv, lib, pkgs, sternenseemann, lazy-packages }:

let
  inherit (pkgs) callPackage;

  readTree = import ../../lib/readTree {};

  haskellPackagesProfpatsch = import ./haskell-overlay.nix { inherit pkgs; };

  # Takes a derivation and a list of binary names
  # and returns an attribute set of `name -> path`.
  # The list can also contain renames in the form of
  # { use, as }, which goes `as -> usePath`.
  getBins = drv: xs:
    let f = x:
      # TODO: typecheck
      let x' = if builtins.isString x then { use = x; as = x; } else x;
      in {
        name = x'.as;
        value = "${lib.getBin drv}/bin/${x'.use}";
      };
    in builtins.listToAttrs (builtins.map f xs);

  # Create a store path where the executable `exe`
  # is linked to $out/bin/${name}.
  # This is useful for e.g. including it as a "package"
  # in `buildInputs` of a shell.nix.
  #
  # For example, if I have the exeutable /nix/store/…-hello,
  # I can make it into /nix/store/…-binify-hello/bin/hello
  # with `binify { exe = …; name = "hello" }`.
  binify = { exe, name }:
    pkgs.runCommandLocal "binify-${name}" {} ''
      mkdir -p $out/bin
      ln -sT ${lib.escapeShellArg exe} $out/bin/${lib.escapeShellArg name}
    '';

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

  # TODO: upstream
  writeHaskellInterpret = nameOrPath: { withPackages ? lib.const [] }: content:
    let ghc = pkgs.haskellPackages.ghcWithPackages withPackages; in
    pkgs.writers.makeScriptWriter {
      interpreter = "${ghc}/bin/runhaskell";
      check = pkgs.writers.writeDash "ghc-typecheck" ''
        ln -s "$1" ./Main.hs
        ${ghc}/bin/ghc -fno-code -Wall ./Main.hs
      '';
    } nameOrPath content;

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

  toNetstring = s:
    "${toString (builtins.stringLength s)}:${s},";

  toNetstringList = xs:
    lib.concatStrings (map toNetstring xs);

  toNetstringKeyVal = attrs:
    lib.concatStrings
      (lib.mapAttrsToList
        (k: v: toNetstring (toNetstring k + toNetstring v))
        attrs);

  writeRust = import ./write-rust.nix {
    inherit pkgs getBins;
    inherit (runExeclineFns) runExeclineLocal;
    inherit (nixperiments) drvSeqL;
  };

  sandboxFns = import ./sandbox.nix { inherit pkgs; inherit (writeExeclineFns) writeExecline; };

  # All utility functions and libraries that packages might need
  utilityArgs = {
    inherit stdenv lib pkgs sternenseemann lazy-packages homeRepo;
    inherit getBins binify exactSource;
    inherit writeHaskellInterpret;
    inherit (nixperiments) match script drvSeq drvSeqL withTests filterSourceGitignoreWith readGitignoreFile;
    inherit (runExeclineFns) runExecline runExeclineLocal runExeclineLocalNoSeqL;
    inherit (writeExeclineFns) writeExecline writeExeclineBin;
    inherit (writeRust) writeRustSimple writeRustSimpleBin writeRustSimpleLib;
    inherit (sandboxFns) sandbox runInEmptyEnv;
    inherit toNetstring toNetstringList toNetstringKeyVal;
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
    standaloneArgs = utilityArgs // pkgs // { runblock = discovered.execline.runblock; };
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
  # Export utility functions for backwards compatibility
  inherit getBins binify;
  inherit toNetstring toNetstringList toNetstringKeyVal;
  inherit (runExeclineFns) runExecline runExeclineLocal;
  inherit (writeExeclineFns) writeExecline writeExeclineBin;
  inherit (writeRust) writeRustSimple writeRustSimpleBin writeRustSimpleLib;
  inherit (sandboxFns) sandbox runInEmptyEnv;
  inherit homeRepo;
  inherit (nixperiments) match script drvSeq drvSeqL withTests filterSourceGitignoreWith readGitignoreFile;
})

{ stdenv, lib, pkgs }:

let
  inherit (pkgs) callPackage;

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

  exactSource = import ./exact-source.nix;

  # various nix utils and fun experiments
  nixperiments =
    let
      src = pkgs.fetchFromGitHub {
        owner = "Profpatsch";
        repo = "nixperiments";
        rev = "519cddb867af054f242cb318ea0c61efe56a2471";
        sha256 = "1i11yr2q40l2ghccn5lydp3dbag8m7y9vl456ghzygyz48jzavf9";
      };
    in import src { nixpkgs = pkgs; };

  testing = import ./testing {
    inherit stdenv lib;
    inherit (runExeclineFns) runExecline;
    inherit (pkgs) runCommandLocal;
    bin = getBins pkgs.s6PortableUtils [ "s6-touch" "s6-echo" ];
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
        # can’t use runExeclineLocal in the tests,
        # because it is tested by the tests (well, it does
        # work, but then you have to run the tests every time)
        runExecline = it;
        inherit (testing) drvSeqL;
        inherit (pkgs) coreutils;
        inherit stdenv;
        bin = (getBins pkgs.execline [
                 "execlineb"
                 { use = "if"; as = "execlineIf"; }
                 "redirfd" "importas"
               ])
           // (getBins pkgs.s6PortableUtils
                [ "s6-cat" "s6-grep" "s6-touch" "s6-test" "s6-chmod" ]);
       };
    in {
      runExecline = it;
      runExeclineLocal = name: args: execline:
        testing.drvSeqL tests (itLocal name args execline);
    };

  writeExeclineFns = callPackage ./execline/write-execline.nix {};


in rec {
  inherit (nixperiments)
    # filterSource by parsing a .gitignore file
    filterSourceGitignore
    # canonical pattern matching primitive
    match
    # generate an option parser for scripts
    script
    # derivation testing
    drvSeq drvSeqL withTests
    # using the nix evaluator as a json transformation runtime
    json2json json2string;

  backlight = callPackage ./backlight { inherit (pkgs.xorg) xbacklight; };
  display-infos = callPackage ./display-infos { inherit sfttime; };
  git-commit-index = callPackage ./git-commit-index { inherit script; };
  nix-http-serve = callPackage ./nix-http-serve {};
  nman = callPackage ./nman {};
  sfttime = callPackage ./sfttime {};
  show-qr-code = callPackage ./show-qr-code {};
  warpspeed = callPackage ./warpspeed {
    inherit (pkgs.haskellPackages) ghcWithPackages;
  };
  youtube2audiopodcast = callPackage ./youtube2audiopodcast {
    inherit writeExecline writeHaskellInterpret getBins runInEmptyEnv sandbox;
  };

  inherit (callPackage ./utils-hs {})
    nix-gen until watch-server
    haskellPackages;

  query-audio-streams = callPackage ./query-album-streams {
    inherit writeExecline writeHaskellInterpret getBins;
  };

  # patched version of droopy, with javascript user-enhancement
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

  inherit (runExeclineFns)
    runExecline runExeclineLocal;
  inherit (writeExeclineFns)
    writeExecline writeExeclineBin;
  inherit (import ./execline/runblock.nix { inherit pkgs; })
    runblock;
  inherit (import ./execline/e.nix { inherit pkgs writeExecline getBins; })
    e;

  inherit getBins;

  inherit (import ./sandbox.nix {inherit pkgs writeExecline; })
    sandbox runInEmptyEnv;

  symlink = pkgs.callPackage ./execline/symlink.nix {
    inherit runExecline;
  };

  importer = pkgs.callPackage ./execline/importer.nix {
    inherit symlink;
  };


  easy-dhall-nix = (import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-dhall-nix";
    rev = "27edaa0c769b9c876f11c50c9ab542061d07def5";
    sha256 = "0w2ckl8fifnaq12ycqqa9ldqjsys3jbblbg7f1vkqa272y89zl08";
  }) { inherit pkgs; });

  dhall = easy-dhall-nix.dhall-simple;
  dhall-nix = easy-dhall-nix.dhall-nix-simple;
  dhall-json = easy-dhall-nix.dhall-json-simple;

  dhall-flycheck =
    (import "${pkgs.fetchFromGitHub {
      owner = "Profpatsch";
      repo = "dhall-flycheck";
      rev = "f3b908b22d0803711e58378b5fa291b987d1c91";
      sha256 = "1y9m4345da91rz6k1hc34ajlfrfglrw8mq725vr45a1fbrf3yzim";
    }}/overlay.nix" pkgs pkgs).dhall-flycheck;

  buildDhallPackage = pkgs.callPackage ./dhall/build-dhall-package-improved.nix { inherit dhall; };

}

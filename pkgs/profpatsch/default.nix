{ stdenv, lib, pkgs }:

let
  inherit (pkgs) callPackage;

  # wrapper for execlineb that doesn’t need the execline commands
  # in PATH to work (making them appear like “builtins”)
  # TODO: upstream into nixpkgs
  # TODO: the grep could be nicer
  execlineb-with-builtins =
    let eldir = "${pkgs.execline}/bin";
    in pkgs.writeScriptBin "execlineb" ''
      #!${eldir}/execlineb -s0
      # appends the execlineb bin dir to PATH if not yet in PATH
      ${eldir}/define eldir ${eldir}
      ''${eldir}/ifelse
      {
        # since this is nix, we can grep for the execline drv hash in PATH
        # to see whether it’s already in there
        ''${eldir}/pipeline
        { ${pkgs.coreutils}/bin/printenv PATH }
        ${pkgs.gnugrep}/bin/grep --quiet "${eldir}"
      }
      # it’s there already
      { ''${eldir}/execlineb $@ }
      # not there yet, add it
      ''${eldir}/importas oldpath PATH
      ''${eldir}/export PATH "''${eldir}:''${oldpath}"
      ''${eldir}/execlineb $@
    '';

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

  # various nix utils and fun experiments
  nixperiments =
    let
      src = pkgs.fetchFromGitHub {
        owner = "Profpatsch";
        repo = "nixperiments";
        rev = "1c654b42d46b680ceef8e2c65a40886f3f8614b9";
        sha256 = "1lflgly8zy4s212s2caj21si15ydbgbm0szck66vrnwqvw3v3nws";
      };
    in import src { nixpkgs = pkgs; };

  runCommandLocal = name: args: cmd:
    pkgs.runCommand name (args // {
      preferLocalBuild = true;
      allowSubstitutes = false;
    }) cmd;

  testing = import ./testing {
    inherit stdenv lib;
    inherit (runExeclineFns) runExecline;
    inherit (pkgs) runCommand;
    bin = getBins pkgs.s6PortableUtils [ "s6-touch" "s6-echo" ];
  };

  runExeclineFns =
    # todo: factor out calling tests
    let
      it = import ./execline/run-execline.nix {
        bin = (getBins execlineb-with-builtins [ "execlineb" ])
           // (getBins pkgs.execline [ "redirfd" "importas" "exec" ]);
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
        bin = (getBins execlineb-with-builtins [ "execlineb" ])
           // (getBins pkgs.execline [
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

  writeExeclineFns = import ./execline/write-execline.nix {
    inherit pkgs execlineb-with-builtins;
  };

  # remove everything but a few selected environment variables
  runInEmptyEnv = keepVars:
    let
        importas = pkgs.lib.concatMap (var: [ "importas" "-i" var var ]) keepVars;
        # we have to explicitely call export here, because PATH is probably empty
        export = pkgs.lib.concatMap (var: [ "${pkgs.execline}/bin/export" var ''''${${var}}'' ]) keepVars;
    in writeExeclineFns.writeExecline "empty-env" {}
         (importas ++ [ "emptyenv" ] ++ export ++ [ "${pkgs.execline}/bin/exec" "$@" ]);


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
  git-commit-index = callPackage ./git-commit-index { inherit script runCommandLocal; };
  nix-http-serve = callPackage ./nix-http-serve {};
  nman = callPackage ./nman {};
  sfttime = callPackage ./sfttime {};
  show-qr-code = callPackage ./show-qr-code {};
  warpspeed = callPackage ./warpspeed {
    inherit (pkgs.haskellPackages) ghcWithPackages;
  };

  inherit (callPackage ./utils-hs {})
    nix-gen until watch-server
    haskellPackages;

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

  inherit getBins;

  symlink = pkgs.callPackage ./execline/symlink.nix {
    inherit runExecline;
  };

  importer = pkgs.callPackage ./execline/importer.nix {
    inherit symlink;
  };

}

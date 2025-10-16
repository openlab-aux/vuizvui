{ stdenv, lib, pkgs, sternenseemann, tvl, lazy-packages }:

let
  inherit (pkgs) callPackage;

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
  # This is useful for e.g. including it as a “package”
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
        # can’t use runExeclineLocal in the tests,
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

in rec {
  # tvl = import /home/philip/depot {};

  inherit tvl;

  inherit (nixperiments)
    # canonical pattern matching primitive
    match
    # generate an option parser for scripts
    script
    # derivation testing
    drvSeq drvSeqL withTests
    # using the nix evaluator as a json transformation runtime
    json2json json2string;

  inherit (nixperiments.filterSourceGitignore)
    # filterSource by parsing a .gitignore file
    filterSourceGitignoreWith
    readGitignoreFile
    ;

  inherit (import ./deploy.nix { inherit pkgs getBins; })
    deploy
    ;

  backlight = callPackage ./backlight { inherit (pkgs) acpilight; };
  display-infos = callPackage ./display-infos { inherit sfttime; };
  git-commit-index = callPackage ./git-commit-index { inherit script; };
  nix-http-serve = callPackage ./nix-http-serve {};
  nman = callPackage ./nman {
    inherit writeRustSimpleBin;
    inherit (sternenseemann) temp;
  };
  sfttime = callPackage ./sfttime {};
  show-qr-code = callPackage ./show-qr-code {};
  read-qr-code = callPackage ./read-qr-code.nix {
    inherit writeExecline getBins;
  };
  read-qr-code-from-camera = callPackage ./read-qr-code-from-camera.nix {
    inherit writeExecline getBins;
  };
  youtube2audiopodcast = callPackage ./youtube2audiopodcast {
    inherit writeExecline writeHaskellInterpret getBins runInEmptyEnv sandbox;
  };

  xrandr = import ./xrandr.nix { inherit pkgs getBins runExeclineLocal writeExecline toNetstringKeyVal; };

  inherit (callPackage ./utils-hs {})
    until
    # watch-server
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

  inherit (import ./write-rust.nix { inherit pkgs runExeclineLocal getBins drvSeqL; })
    writeRustSimple
    writeRustSimpleBin
    writeRustSimpleLib
    ;

  inherit (runExeclineFns)
    runExecline runExeclineLocal;
  inherit (writeExeclineFns)
    writeExecline writeExeclineBin;
  inherit (import ./execline/runblock.nix { inherit pkgs; })
    runblock;
  inherit (import ./execline/e.nix { inherit pkgs writeExecline getBins writeRustSimple; })
    e;

  toNetstring = s:
    "${toString (builtins.stringLength s)}:${s},";

  toNetstringList = xs:
    lib.concatStrings (map toNetstring xs);

  toNetstringKeyVal = attrs:
    lib.concatStrings
      (lib.mapAttrsToList
        (k: v: toNetstring (toNetstring k + toNetstring v))
        attrs);

  inherit getBins binify;

  inherit (import ./sandbox.nix {inherit pkgs writeExecline; })
    sandbox runInEmptyEnv;

  symlink = pkgs.callPackage ./execline/symlink.nix {
    inherit runExecline toNetstring;
  };

  importer = pkgs.callPackage ./execline/importer.nix {
    inherit symlink;
  };

  inherit (import ./profpatsch.de {
    inherit pkgs lib tvl
      toNetstring toNetstringList writeExecline runExecline getBins writeRustSimple netencode-rs el-semicolon el-exec el-substitute netencode record-get;
  })
    websiteStatic
    index-server
    importas-if
    concatenatedCss
    ;

  inherit (import ./nix-tools.nix { inherit pkgs getBins writeExecline runblock; })
    nix-run
    nix-run-bin
    nix-eval
    ;

  # s6 = pkgs.callPackage ./s6 {
  #   inherit (haskellPackages) dhall-nix;
  #   inherit runExeclineLocal;
  # };

  # dhall-experiment = pkgs.callPackage ./dhall-experiment {
  #   inherit (haskellPackages) dhall-nix;
  # };

  # xmonad = pkgs.callPackage ./xmonad { };

  inherit (import ./importPurescript.nix { inherit pkgs exactSource; haskellPackages = haskellPackagesProfpatsch; })
    importPurescript
    ;

  rust-deps = (import ./rust-deps.nix { inherit (pkgs) buildRustCrate; });

  inherit (import ./xdg-open { inherit pkgs tvl getBins importPurescript writeExecline runExeclineLocal netencode-rs writeRustSimple record-get el-exec lazy-packages show-qr-code; })
    xdg-open
    xdg-open-module
    read-headers-and-follow-redirect
    mini-url
    assert-printf
    as-stdin
    printenv
    ;

  text-letter = import ./text-letter.nix { inherit pkgs rust-deps writeRustSimple writeExecline getBins; };

  inherit (import ./netencode { inherit pkgs writeRustSimpleLib writeRustSimple el-semicolon el-exec; })
    netencode-rs
    netencode-rs-tests
    record-get
    ;

  # inherit (import ./execline/el-semicolon.nix { inherit writeRustSimpleLib; });

  inherit (import ./execline/default.nix { inherit pkgs writeRustSimpleLib rust-deps; })
    el-semicolon
    el-exec
    el-substitute
    ;

  netencode = import ./netencode/netencode.nix;

  inherit (import ./lru.nix { inherit pkgs writeRustSimple; })
    lru-dir
    ;


  backup = import ./backup { inherit pkgs writeExecline getBins; };

  jaeger = import ./jaeger { inherit pkgs writeExecline; };

  gpg-private-offline-key = import ./gpg-private-offline-key { inherit pkgs writeExecline getBins; };

  blight = callPackage ./blight.nix { };

  gitit = import (pkgs.fetchFromGitHub {
    owner = "openlab-aux";
    repo = "gitit";
    rev = "94672a9ed8a7aca1c085e2120d9edb953117805e";
    sha256 = "sha256-PM7eQl0boFGQou/9FL0lv+dQxzM9b0I/hyJ1hxv4Om4=";
  }) { inherit pkgs; };

  micro = callPackage ./micro { };
  television = callPackage ./television { };

}

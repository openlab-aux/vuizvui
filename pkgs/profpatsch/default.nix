{ stdenv, pkgs }:

let
  inherit (pkgs) callPackage;

  # wrapper for execlineb that doesn’t need the execline commands
  # in PATH to work (making them appear like “builtins”)
  execlineb-with-builtins =
    let eldir = "${pkgs.execline}/bin";
    in pkgs.writeScriptBin "execlineb" ''
      #!${eldir}/execlineb -s0
      ${eldir}/define eldir ${eldir}
      ''${eldir}/importas oldpath PATH
      ''${eldir}/export PATH "''${eldir}:''${oldpath}"
      ''${eldir}/execlineb $@
    '';

  # TODO: use imports!
  execlinebCommand = "${execlineb-with-builtins}/bin/execlineb";
  redirfdCommand = "${pkgs.execline}/bin/redirfd";
  importasCommand = "${pkgs.execline}/bin/importas";
  s6TouchCommand = "${pkgs.s6-portable-utils}/bin/s6-touch";
  s6EchoCommand = "${pkgs.s6-portable-utils}/bin/s6-echo";
  ifCommand = "${pkgs.execline}/bin/if";
  s6GrepCommand = "${pkgs.s6-portable-utils}/bin/s6-grep";
  s6CatCommand = "${pkgs.s6-portable-utils}/bin/s6-cat";
  s6TestCommand = "${pkgs.s6-portable-utils}/bin/s6-test";
  s6ChmodCommand = "${pkgs.s6-portable-utils}/bin/s6-chmod";
  execCommand = "${pkgs.execline}/bin/exec";

in rec {
  backlight = callPackage ./backlight { inherit (pkgs.xorg) xbacklight; };
  display-infos = callPackage ./display-infos {};
  nix-http-serve = callPackage ./nix-http-serve {};
  nman = callPackage ./nman {};
  show-qr-code = callPackage ./show-qr-code {};
  warpspeed = callPackage ./warpspeed {
    inherit (pkgs.haskellPackages) ghcWithPackages;
  };

  inherit (callPackage ./utils-hs {})
    nix-gen until watch-server;

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

  runExecline =
    # todo: factor out calling tests
    let
      it = import ./execline/run-execline.nix {
        inherit stdenv execlinebCommand redirfdCommand
          importasCommand execCommand;
      };
      tests = import ./execline/run-execline-tests.nix {
        runExecline = it;
        inherit (testing) drvSeqL;
        inherit (pkgs) coreutils;
        inherit stdenv ifCommand redirfdCommand s6CatCommand
          s6GrepCommand importasCommand s6TouchCommand
          s6TestCommand execlinebCommand s6ChmodCommand;
       };
    in tests;


  testing = pkgs.callPackage ./testing {
    inherit runExecline s6TouchCommand s6EchoCommand;
  };

  symlink = pkgs.callPackage ./execline/symlink.nix {
    inherit runExecline;
  };

  importer = pkgs.callPackage ./execline/importer.nix {
    inherit symlink;
  };

}

{ stdenv, lib, pkgs }:

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

  # Takes a derivation and a list of binary names
  # and returns an attribute set of `name -> path`.
  # The list can also contain renames in the form of
  # { use, as }, which goes `as -> usePath`.
  bins = drv: xs:
    let f = x:
      # TODO: typecheck
      let x' = if builtins.isString x then { use = x; as = x; } else x;
      in {
        name = x'.as;
        value = "${lib.getBin drv}/bin/${x'.use}";
      };
    in builtins.listToAttrs (builtins.map f xs);


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

  runExecline =
    # todo: factor out calling tests
    let
      it = import ./execline/run-execline.nix {
        bin = (bins execlineb-with-builtins [ "execlineb" ])
           // (bins pkgs.execline [ "redirfd" "importas" "exec" ]);
        inherit stdenv;
      };
      tests = import ./execline/run-execline-tests.nix {
        runExecline = it;
        inherit (testing) drvSeqL;
        inherit (pkgs) coreutils;
        inherit stdenv;
        bin = (bins execlineb-with-builtins [ "execlineb" ])
           // (bins pkgs.execline [
                 { use = "if"; as = "execlineIf"; }
                 "redirfd" "importas"
               ])
           // (bins pkgs.s6PortableUtils
                [ "s6-cat" "s6-grep" "s6-touch" "s6-test" "s6-chmod" ]);
       };
    in tests;


  testing = import ./testing {
    inherit stdenv lib runExecline;
    inherit (pkgs) runCommand;
    bin = bins pkgs.s6PortableUtils [ "s6-touch" "s6-echo" ];
  };

  symlink = pkgs.callPackage ./execline/symlink.nix {
    inherit runExecline;
  };

  importer = pkgs.callPackage ./execline/importer.nix {
    inherit symlink;
  };

}

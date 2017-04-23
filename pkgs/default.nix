{ pkgs ? import (import ../nixpkgs-path.nix) {} }:

let
  inherit (pkgs.lib) callPackageWith isFunction;
  callPackage = callPackageWith (pkgs // self.vuizvui);
  callPackage_i686 = callPackageWith (pkgs.pkgsi686Linux // self.vuizvui);

  callPackageScope = fn: let
    f = if isFunction fn then fn else import fn;

    toplevel = pkgs // self.vuizvui;
    toplevel_i686 = pkgs.pkgsi686Linux // self.vuizvui;

    autoArgs = toplevel // {
      callPackage = callPackageWith (toplevel // super);
      callPackage_i686 = callPackageWith (toplevel_i686 // super);
    };
    args = builtins.intersectAttrs (builtins.functionArgs f) autoArgs;

    mkOverridable = overrideFun: origArgs: let
      superSet = overrideFun origArgs;
      overrideWith = newArgs: let
        overridden = if isFunction newArgs then newArgs origArgs else newArgs;
      in origArgs // overridden;
    in superSet // {
      override = newArgs: mkOverridable overrideFun (overrideWith newArgs);
    };

    super = mkOverridable f args;

  in pkgs.recurseIntoAttrs super;

  self.vuizvui = pkgs.recurseIntoAttrs {
    mkChannel = callPackage ./build-support/channel.nix { };

    list-gamecontrollers = callPackage ./list-gamecontrollers { };
    show-qr-code = callPackage ./show-qr-code { };

    games = import ./games {
      inherit pkgs;
      config = pkgs.config.vuizvui.games or null;
    };

    openlab = pkgs.recurseIntoAttrs {
      gitit = callPackage ./openlab/gitit { hlib = pkgs.haskell.lib; };
      stackenblocken = callPackage ./openlab/stackenblocken {};
    };

    aszlig = callPackageScope ./aszlig;

    profpatsch = pkgs.recurseIntoAttrs {
      display-infos = callPackage ./profpatsch/display-infos {};
      nman = callPackage ./profpatsch/nman {};
      warpspeed = callPackage ./profpatsch/warpspeed {
        inherit (pkgs.haskellPackages) ghcWithPackages;
      };

      jmtpfs = pkgs.jmtpfs.override {
        libmtp = pkgs.libmtp.overrideDerivation (old: {
          patches = old.patches or [] ++ [
            ./profpatsch/patches/mtp-jolla.patch
          ];
        });
      };

      # patched version of droopy, with javascript user-enhancement
      droopy = pkgs.droopy.overrideDerivation (old: {
        src = pkgs.fetchFromGitHub {
          owner = "Profpatsch";
          repo = "Droopy";
          rev = "dc63d0ac9cecd74cdff84ab9ea2a5849d6953e8a";
          sha256 = "09sms524wrnpdkhnpv9f2qbq30s8h02ljiv934g0dvmxy8571ph7";
        };
        installPhase = old.installPhase or "" + ''
          mkdir -p $out/share/droopy
          cp -r $src/static $out/share/droopy
        '';
        makeWrapperArgs = old.makeWrapperArgs or [] ++ [
          "--set DROOPY_STATIC \"$out/share/droopy/static\""
        ];

      });
    };
    sternenseemann = pkgs.recurseIntoAttrs {
      spacecookie = pkgs.haskellPackages.callPackage ./sternenseemann/spacecookie {};
      logbook = pkgs.ocamlPackages_4_02.callPackage ./sternenseemann/logbook {};
    };
  };
in pkgs // self

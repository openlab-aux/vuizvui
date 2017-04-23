{ pkgs ? import (import ../nixpkgs-path.nix) {} }:

let
  inherit (pkgs.lib) callPackageWith;
  callPackage = callPackageWith (pkgs // self.vuizvui);
  callPackage_i686 = callPackageWith (pkgs.pkgsi686Linux // self.vuizvui);

  self.vuizvui = {
    mkChannel = callPackage ./build-support/channel.nix { };

    aacolorize = callPackage ./aacolorize { };
    axbo = callPackage ./axbo { };
    git-detach = callPackage ./git-detach { };
    grandpa = callPackage ./grandpa { };
    nixops = callPackage ./nixops { };
    librxtx_java = callPackage ./librxtx-java { };
    list-gamecontrollers = callPackage ./list-gamecontrollers { };
    lockdev = callPackage ./lockdev { };
    pvolctrl = callPackage ./pvolctrl { };
    santander = callPackage_i686 ./santander { };
    show-qr-code = callPackage ./show-qr-code { };
    tomahawk = callPackage ./tomahawk {
      boost = pkgs.boost155;
    };

    games = import ./games {
      inherit pkgs;
      config = pkgs.config.vuizvui.games or null;
    };

    openlab = pkgs.recurseIntoAttrs {
      gitit = callPackage ./openlab/gitit { hlib = pkgs.haskell.lib; };
      stackenblocken = callPackage ./openlab/stackenblocken {};
    };

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

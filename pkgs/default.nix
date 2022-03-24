{ pkgs ? import (import ../nixpkgs-path.nix) {} }:

let
  inherit (pkgs.lib)
    callPackageWith
    recurseIntoAttrs
    dontRecurseIntoAttrs
    ;

  callPackage = callPackageWith (pkgs // self.vuizvui);
  callPackage_i686 = callPackageWith (pkgs.pkgsi686Linux // self.vuizvui);

  callPackageScope = import ./lib/call-package-scope.nix {
    pkgs = pkgs // self.vuizvui;
    pkgsi686Linux = pkgs.pkgsi686Linux // self.vuizvui;
  };

  self.vuizvui = recurseIntoAttrs {
    mkChannel = callPackage ./build-support/channel.nix { };
    buildSandbox = callPackage ./build-support/build-sandbox {
      inherit (pkgs.nixVersions) nix_2_3;
    };
    lazy-packages = callPackage ./build-support/lazy-packages {};

    list-gamecontrollers = callPackage ./list-gamecontrollers { };

    games = import ./games {
      pkgs = pkgs // self.vuizvui;
      pkgsi686Linux = pkgs.pkgsi686Linux // self.vuizvui;
      config = pkgs.config.vuizvui.games or null;
    };

    taalo-build = callPackage ./taalo-build { };

    aszlig = callPackageScope ./aszlig;
    devhell = callPackageScope ./devhell;
    openlab = callPackageScope ./openlab;
    profpatsch = callPackageScope ./profpatsch;
    sternenseemann = callPackageScope ./sternenseemann;

    tvl = dontRecurseIntoAttrs (callPackage ./tvl { });
  };
in self.vuizvui

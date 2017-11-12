{ pkgs ? import (import ../nixpkgs-path.nix) {} }:

let
  inherit (pkgs.lib) callPackageWith;
  callPackage = callPackageWith (pkgs // self.vuizvui);
  callPackage_i686 = callPackageWith (pkgs.pkgsi686Linux // self.vuizvui);

  callPackageScope = import ./lib/call-package-scope.nix {
    pkgs = pkgs // self.vuizvui;
    pkgsi686Linux = pkgs.pkgsi686Linux // self.vuizvui;
  };

  self.vuizvui = pkgs.recurseIntoAttrs {
    mkChannel = callPackage ./build-support/channel.nix { };

    list-gamecontrollers = callPackage ./list-gamecontrollers { };

    games = import ./games {
      inherit pkgs;
      config = pkgs.config.vuizvui.games or null;
    };

    aszlig = callPackageScope ./aszlig;
    openlab = callPackageScope ./openlab;
    profpatsch = callPackageScope ./profpatsch;
    sternenseemann = callPackageScope ./sternenseemann;
  };
in self.vuizvui

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

    aszlig = callPackageScope ./aszlig;
    openlab = callPackageScope ./openlab;
    profpatsch = callPackageScope ./profpatsch;
    sternenseemann = callPackageScope ./sternenseemann;
  };
in pkgs // self

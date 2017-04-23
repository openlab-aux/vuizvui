{ pkgs, pkgsi686Linux }:

fn: let
  inherit (builtins) isFunction intersectAttrs functionArgs;

  f = if isFunction fn then fn else import fn;

  autoArgs = pkgs // {
    callPackage = pkgs.lib.callPackageWith (pkgs // super);
    callPackage_i686 = pkgs.lib.callPackageWith (pkgsi686Linux // super);
  };
  args = intersectAttrs (functionArgs f) autoArgs;

  mkOverridable = overrideFun: origArgs: let
    superSet = overrideFun origArgs;
    overrideWith = newArgs: let
      overridden = if isFunction newArgs then newArgs origArgs else newArgs;
    in origArgs // overridden;
  in superSet // {
    override = newArgs: mkOverridable overrideFun (overrideWith newArgs);
  };

  super = mkOverridable f args;

in pkgs.recurseIntoAttrs super

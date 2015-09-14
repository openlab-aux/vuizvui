{ config, pkgs, lib, ... }:

with lib;

{
  nixpkgs.config.packageOverrides = pkgs: {
    gnupg = pkgs.gnupg21;

    # XXX: Very ugly workaround see https://github.com/openlab-aux/vuizvui/commit/a93b7583084ff9084d73873d80d8dc428406593c
    haskellPackages = pkgs.haskellPackages.override {
      ghc = pkgs.haskellPackages.ghc.overrideDerivation (const {
        forceRebuild = true;
      });
    };
  };
}

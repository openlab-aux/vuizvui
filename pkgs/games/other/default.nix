{ config, lib, pkgs, ... }:

let
  self = rec {
    callPackage = pkgs.lib.callPackageWith (pkgs // self);
    callPackage_i686 = pkgs.lib.callPackageWith (pkgs.pkgsi686Linux // self);

    soldat = callPackage ./soldat { };
  };
in with lib; {
  config.packages = {
    other = self;
  };
}

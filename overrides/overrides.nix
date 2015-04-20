{ config, pkgs, lib, ... }:

{
  nixpkgs.config.packageOverrides = pkgs: {
    gnupg = pkgs.gnupg21;
    haskellngPackages = pkgs.haskell-ng.packages.ghc784;
  };
}

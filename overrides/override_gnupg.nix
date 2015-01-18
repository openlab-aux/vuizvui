{ config, pkgs, lib, ... }:

{
  nixpkgs.config.packageOverrides = pkgs: {
    gnupg = pkgs.gnupg21;
  };
}

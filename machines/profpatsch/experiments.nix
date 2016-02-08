{ config, lib, pkgs, ...}:

{
  nixpkgs.config.packageOverrides = pkgs: {
    sane-backends = pkgs.callPackage ./tmp/sane/backends {};
  };

  hardware.sane.enable = true;
}

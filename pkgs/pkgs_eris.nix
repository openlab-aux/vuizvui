{ config, pkgs, lib, ... }:

{
  imports = [ ./pkgs_common.nix ];

  nixpkgs.config.mpv = {
    vaapiSupport = true;
  };

  environment.systemPackages = with pkgs; [
    aircrackng
    kismet
    minicom
    networkmanagerapplet
    pamixer
    pmtools
    pmutils
    reaverwps
    snort
    xbindkeys
    xorg.xbacklight
  ];
}

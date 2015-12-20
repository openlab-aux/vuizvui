{ config, pkgs, lib, ... }:

{
  imports = [ ./pkgs_common.nix ];

  nixpkgs.config.mpv = {
    vaapiSupport = true;
  };

  environment.systemPackages = with pkgs; [
    heimdall
    aircrackng
    horst
    kismet
    minicom
    networkmanagerapplet
    pamixer
    pmtools
    pmutils
    reaverwps
    snort
    wavemon
    xbindkeys
    xorg.xbacklight
  ];
}

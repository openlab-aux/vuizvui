{ config, pkgs, lib, ... }:

{
  imports = [ ./pkgs_common.nix ];

  nixpkgs.config.mpv = {
    vaapiSupport = true;
  };

  environment.systemPackages = with pkgs; [
    aircrackng
    atom
    kismet
    minicom
    msmtp
    mutt
    networkmanagerapplet
    notmuch
    offlineimap
    pamixer
    pmtools
    pmutils
    reaverwps
    snort
    xbindkeys
    xorg.xbacklight
  ];
}

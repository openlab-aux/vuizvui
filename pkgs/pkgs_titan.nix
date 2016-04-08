{ config, pkgs, lib, ... }:

{
  imports = [ ./pkgs_common.nix ];

  nixpkgs.config.mpv = {
    vaapiSupport = true;
  };

  environment.systemPackages = with pkgs; [
    skype
    ipmitool
    ipmiutil
    ipmiview
    aircrackng
    pythonPackages.alot
    connmanui
    cura
    horst
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
    wavemon
    xbindkeys
    xorg.xbacklight
  ];
}

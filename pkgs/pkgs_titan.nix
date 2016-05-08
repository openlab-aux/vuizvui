{ config, pkgs, lib, ... }:

{
  imports = [ ./pkgs_common.nix ];

  nixpkgs.config.mpv = {
    vaapiSupport = true;
  };

  environment.systemPackages = with pkgs; [
    calibre
    aircrackng
    claws-mail
    connmanui
    cura
    horst
    ipmitool
    ipmiutil
    ipmiview
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
    pythonPackages.alot
    reaverwps
    skype
    snort
    thunderbird
    wavemon
    xbindkeys
    xorg.xbacklight
  ];
}

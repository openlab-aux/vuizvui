{ config, pkgs, lib, ... }:

{
  imports = [ ./pkgs_common.nix ];

  nixpkgs.config.mpv = {
    vaapiSupport = true;
  };

  environment.systemPackages = with pkgs; [
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
    openjdk8
    pamixer
    pmtools
    pmutils
    pythonPackages.alot
    reaverwps
    skype
    snort
    thunderbird
    wavemon
    wirelesstools
    xbindkeys
    xorg.xbacklight
  ];
}

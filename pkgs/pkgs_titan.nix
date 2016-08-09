{ config, pkgs, lib, ... }:

{
  imports = [ ./pkgs_common.nix ];

  nixpkgs.config.mpv = {
    vaapiSupport = true;
  };

  environment.systemPackages = with pkgs; [
    libva
    aircrackng
    claws-mail
    connmanui
    cura
    glxinfo
    horst
    ipmitool
    ipmiutil
    ipmiview
    kismet
    libvdpau-va-gl
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
    vaapiVdpau
    vdpauinfo
    wavemon
    wirelesstools
    xbindkeys
    xorg.xbacklight
  ];
}

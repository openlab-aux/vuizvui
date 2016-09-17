{ config, pkgs, lib, ... }:

{
  imports = [ ./pkgs_common.nix ];

  nixpkgs.config.mpv = {
    vaapiSupport = true;
  };

  environment.systemPackages = with pkgs; [
    abcde
    aircrackng
    cdrtools
    claws-mail
    connmanui
    cura
    dvdplusrwtools
    glxinfo
    horst
    ipmitool
    ipmiutil
    ipmiview
    kismet
    libva
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

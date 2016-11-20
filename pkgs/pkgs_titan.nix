{ config, pkgs, lib, ... }:

{
  imports = [ ./pkgs_common.nix ];

  nixpkgs.config.mpv = {
    vaapiSupport = true;
  };

  environment.systemPackages = with pkgs; [
    #cura
    #openjdk8
    #skype
    abcde
    aircrackng
    cdrtools
    claws-mail
    connmanui
    dvdplusrwtools
    glxinfo
    horst
    ipmitool
    ipmiutil
    ipmiview
    kdenlive
    kismet
    libva
    libvdpau-va-gl
    minicom
    msmtp
    mutt
    networkmanagerapplet
    notmuch
    offlineimap
    openshot
    pamixer
    pmtools
    pmutils
    pythonPackages.alot
    reaverwps
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

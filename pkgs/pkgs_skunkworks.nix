{ config, pkgs, lib, ... }:

{
  imports = [ ./pkgs_common.nix ];

  nixpkgs.config = {
    chromium = {
      enablePepperFlash = true;
    };

    mpv = {
      bs2bSupport = true;
    };
  };

  environment.systemPackages = with pkgs; [
    abook
    beets
    canto-curses
    go15Packages.ipfs
    handbrake
    hplip
    mkvtoolnix
    mutt-with-sidebar
    nzbget
    scummvm
    slrn
    urlview
    twister
  ];
}

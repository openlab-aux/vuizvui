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
    cli-visualizer
    cmus
    #ipfs
    handbrake
    hplip
    mutt-with-sidebar
    nzbget
    #scummvm
    slrn
    twister
    urlview
  ];
}

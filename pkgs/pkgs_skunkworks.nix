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
#    goPackages.ipfs
    hplip
    mkvtoolnix
    mutt-with-sidebar
    urlview
  ];
}

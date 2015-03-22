{ config, pkgs, lib, ... }:

{
  imports = [ ./pkgs_common.nix ];

  nixpkgs.config.chromium.enablePepperFlash = true;

  environment.systemPackages = with pkgs; [
    abook
    beets
    canto-curses
    hplip
    mkvtoolnix
    mutt-with-sidebar
    urlview
  ];
}

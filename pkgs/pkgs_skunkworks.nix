{ config, pkgs, lib, ... }:

{
  nixpkgs.config.chromium.enablePepperFlash = true;
  nixpkgs.config.virtualbox.enableExtensionPack = false;

  environment.systemPackages = with pkgs; [
    abook
    beets
    hplip
    mkvtoolnix
    mutt-with-sidebar
    urlview
  ];
}

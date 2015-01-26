{ config, pkgs, lib, ... }:

{
  nixpkgs.config.chromium.enablePepperFlash = true;
  nixpkgs.config.virtualbox.enableExtensionPack = false;

  environment.systemPackages = with pkgs; [
    abook
    androidsdk_4_4
    beets
    heimdall
    hplip
    mutt-with-sidebar
    urlview
    gtkvnc
  ];
}

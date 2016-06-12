{ config, lib, pkgs, ... }:

{

  # TODO: create a true channel that only advances if these build.
  environment.systemPackages = [
    pkgs.vuizvui.openlab.gitit
  ];

  fileSystems."/".device = "/dev/null";
  boot.loader.grub.device = "/dev/null";

}

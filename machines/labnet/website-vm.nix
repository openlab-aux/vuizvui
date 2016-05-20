# stub for the openlab website VM, which isn’t a nixos,
# but still needs some stuff built.
{ pkgs, lib, config, ... }:

{
  environment.systemPackages = with pkgs; [
    vuizvui.openlab.gitit
  ];
}

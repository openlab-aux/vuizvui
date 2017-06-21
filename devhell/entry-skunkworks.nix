{ config, pkgs, lib, ... }:

{
  imports =
    [
      ./machines/machine_skunkworks.nix
      ./network/network_skunkworks.nix
      ./services/services_skunkworks.nix
      ./pkgs/pkgs_skunkworks.nix
      ./overrides/overrides.nix
      ./users/users_dev.nix
    ];
}


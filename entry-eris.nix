{ config, pkgs, lib, ... }:

{
  imports =
    [
      ./machines/machine_eris.nix
      ./network/network_eris.nix
      ./services/services_eris.nix
      ./pkgs/pkgs_eris.nix
      ./overrides/overrides.nix
      ./users/users_dev.nix
    ];
}


{ config, ... }:

{
  nixpkgs.config.packageOverrides = pkgs: {
    vuizvui = import ../../pkgs {
      # We need to make sure to incorporate other package overrides,
      # otherwise we are unable to override packages in vuizvui.*.
      pkgs = pkgs // config.nixpkgs.config.packageOverrides pkgs;
    };
  };
}

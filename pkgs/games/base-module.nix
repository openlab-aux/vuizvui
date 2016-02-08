{ lib, ... }:

with lib;

{
  options = {
    packages = mkOption {
      type = types.attrsOf types.unspecified;
      default = {};
      description = "Available NixGames packages.";
    };
  };

  config._module.args.pkgs = import <nixpkgs> {};
}

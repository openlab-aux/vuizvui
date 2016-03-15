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
}

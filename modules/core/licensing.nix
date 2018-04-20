{ config, lib, ... }:

let
  overrideConfig = newConfig: import (import ../../nixpkgs-path.nix) {
    inherit (config.nixpkgs) localSystem;
    config = config.nixpkgs.config // newConfig;
  };

in {
  _module.args = {
    unfreePkgs = overrideConfig {
      whitelistedLicenses = [ lib.licenses.unfreeRedistributable ];
    };

    unfreeAndNonDistributablePkgs = overrideConfig {
      allowUnfree = true;
    };
  };
}

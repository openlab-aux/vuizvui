{ lib, config, ... }:

let
  cfg = config.vuizvui.profiles.tvl;
in

{
  options = {
    vuizvui.profiles.tvl = {
      enable = lib.mkEnableOption "TVL-specific tweaks";
    };
  };

  config = lib.mkIf cfg.enable {
    nix = {
      binaryCachePublicKeys = [
        "cache.tvl.su:kjc6KOMupXc1vHVufJUoDUYeLzbwSr9abcAKdn/U1Jk="
      ];

      binaryCaches = [
        "https://cache.tvl.su"
      ];

    };

    # TVL short DNS
    networking.domain = lib.mkDefault "tvl.su";
  };
}

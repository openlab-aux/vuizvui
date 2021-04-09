{ lib, config, ... }:

let
  cfg = config.vuizvui.profiles.tvl;

  tvlShortDNS = [ "b" "cl" ];
in

{
  options = {
    vuizvui.profiles.tvl = {
      enable = lib.mkEnableOption "tvl-specific tweaks";
    };
  };

  config = lib.mkIf cfg.enable {
    nix = {
      binaryCachePublicKeys = lib.mkAfter [
        "cache.tvl.su:kjc6KOMupXc1vHVufJUoDUYeLzbwSr9abcAKdn/U1Jk="
      ];

      binaryCaches = lib.mkAfter [
        "https://cache.tvl.su"
      ];

    };

    networking.hosts = {
      "49.12.129.211" = tvlShortDNS;
      "2a01:4f8:242:5b21:0:feed:edef:beef" = tvlShortDNS;
    };
  };
}

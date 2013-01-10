{config, pkgs, ...}:

with pkgs.lib;

{
  require = singleton ../common.nix;

  boot = let
    linuxAszlig = pkgs.linuxManualConfig {
      version = pkgs.kernelSourceAszlig.version;
      src = pkgs.kernelSourceAszlig.src;

      configfile = let
        isNumber = c: elem c ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9"];
        mkValue = val:
          if val == "" then "\"\""
          else if val == "y" || val == "m" || val == "n" then val
          else if all isNumber (stringToCharacters val) then val
          else if substring 0 2 val == "0x" then val
          else "\"${val}\"";
        mkConfigLine = key: val: "${key}=${mkValue val}";
        mkConf = cfg: concatStringsSep "\n" (mapAttrsToList mkConfigLine cfg);
      in pkgs.writeText "aszlig.kconf" (mkConf (import ./mmrnmhrm-kconf.nix));

      allowImportFromDerivation = true;
    };
  in rec {
    kernelPackages = pkgs.linuxPackagesFor linuxAszlig kernelPackages;

    loader.grub.devices = singleton
      "/dev/disk/by-id/ata-WDC_WD6401AALS-00L3B2_WD-WMASY3263730";
  };

  networking.hostName = "mmrnmhrm";

  fileSystems = {
    "/" = {
      label = "root";
      fsType = "btrfs";
      options = concatStringsSep "," [
        "autodefrag"
        "space_cache"
        "inode_cache"
        "compress=lzo"
        "noatime"
      ];
    };
  };

  swapDevices = singleton {
    label = "swap";
  };

  services.xserver = {
    videoDrivers = [ "nouveau" ];

    xrandrHeads = [ "DVI-I-2" "DVI-I-1" ];

    displayManager.sessionCommands = ''
      ${pkgs.synergy}/bin/synergys -c "${../cfgfiles/synergy.conf}"
    '';
  };
}

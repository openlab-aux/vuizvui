{ config, pkgs, lib, ... }:

let
  cfg = config.vuizvui.hardware.t100ha;
  desc = "hardware support for the ASUS T100HA convertible";

in {
  options.vuizvui.hardware.t100ha.enable = lib.mkEnableOption desc;

  config = lib.mkIf cfg.enable {
    # It's a CherryTrail SoC, so we want to have the latest and greatest with a
    # few additional patches:
    boot.kernelPackages = let
      nixpkgs = import ../../../nixpkgs-path.nix;
      mkKernel = import "${nixpkgs}/pkgs/os-specific/linux/kernel/generic.nix";
      t100haKernel = mkKernel rec {
        version = "4.5-rc5";
        modDirVersion = "4.5.0-rc5";
        extraMeta.branch = "4.5";

        src = pkgs.fetchurl {
          url = "mirror://kernel/linux/kernel/v4.x/testing/"
              + "linux-${version}.tar.xz";
          sha256 = "06qlypnrlkckxhf3clq6l2d3kps7rwfw811sxapjbnhzjd75fcx8";
        };

        kernelPatches = lib.singleton {
          name = "drm-fixes.patch";
          patch = ./drm-fixes.patch;
        };

        extraConfig = ''
          MMC y
          MMC_BLOCK y
          MMC_SDHCI y
          MMC_SDHCI_ACPI y
          PINCTRL_CHERRYVIEW y
          INTEL_SOC_PMIC y
        '';

        features.iwlwifi = true;
        features.efiBootStub = true;
        features.needsCifsUtils = true;
        features.canDisableNetfilterConntrackHelpers = true;
        features.netfilterRPFilter = true;

        inherit (pkgs) stdenv perl buildLinux;
      };
      self = pkgs.linuxPackagesFor t100haKernel self;
    in self;

    # By default the console is rotated by 90 degrees to the right.
    boot.kernelParams = [ "fbcon=rotate:3" ];
    services.xserver.deviceSection = ''
      Option "monitor-DSI1" "Monitor[0]"
    '';
    services.xserver.monitorSection = ''
      Option "Rotate" "left"
    '';
    services.xserver.videoDriver = "intel";

    # The touch screen needs to be rotated as well:
    services.xserver.inputClassSections = lib.singleton ''
      Identifier "touchscreen"
      MatchProduct "SIS0457"
      Option "TransformationMatrix" "0 -1 1 1 0 0 0 0 1"
    '';
  };
}

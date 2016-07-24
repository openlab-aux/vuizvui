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
        version = "4.7-rc6";
        modDirVersion = "4.7.0-rc6";
        extraMeta.branch = "4.7";

        src = pkgs.fetchgit {
          url = "git://git.kernel.org/pub/scm/linux/kernel/git/"
              + "torvalds/linux.git";
          rev = "ee40fb2948fc99096836995d4f3ddcc0efbac790";
          sha256 = "02mdw2wcghsrncrab77pznqx150w2r5jswxb5s71zmzq88bxc6h8";
        };

        kernelPatches = [
          { name = "backlight";
            patch = ./backlight.patch;
          }
          { name = "meta-keys";
            patch = ./meta-keys.patch;
          }
          { name = "revert-fix-acpica-namespace-interpreter-lock";
            patch = ./revert-fix-acpica-namespace-interpreter-lock.patch;
          }
        ];

        # Missing device drivers:
        #
        #   808622B8 -> Intel(R) Imaging Signal Processor 2401
        #   808622D8 -> Intel(R) Integrated Sensor Solution
        #   HIMX2051 -> Camera Sensor Unicam hm2051
        #   IMPJ0003 -> Impinj RFID Device (MonzaX 8K)
        #   OVTI5670 -> Camera Sensor ov5670
        #
        extraConfig = ''
          # CPU
          MATOM y

          # MMC
          MMC y
          MMC_BLOCK y
          MMC_SDHCI y
          MMC_SDHCI_ACPI y

          # PMIC
          INTEL_PMC_IPC y
          INTEL_SOC_PMIC y
          MFD_AXP20X y
          MFD_AXP20X_I2C y

          # GPU
          AGP n
          DRM y
          DRM_I915 y

          # Thermal
          INT3406_THERMAL y
          INT340X_THERMAL y

          # GPIO
          PINCTRL_CHERRYVIEW y

          # I2C
          CONFIG_I2C_DESIGNWARE_BAYTRAIL y
          CONFIG_I2C_DESIGNWARE_PLATFORM y

          # HID
          INTEL_HID_EVENT y

          # MEI
          CONFIG_INTEL_MEI y
          CONFIG_INTEL_MEI_TXE y
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

    # Fix boot loader, otherwise it's hard (not impossible though if you press
    # the key combinations fast enough) to choose anything else than the default
    # entry.
    #
    # Upstream issue with workaround:
    #   https://github.com/systemd/systemd/issues/3632
    systemd.package = lib.overrideDerivation pkgs.systemd (drv: {
      patches = (drv.patches or []) ++ [ ./fix-bootloader.patch ];
    });

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

    # XXX: Workaround for a vblank issue that causes the display to stay blank
    # until the next subsequent vblank (usually on no activity for a while until
    # the monitor gets powered down).
    #
    # I know this is very ugly, but another mitigation would be to disable power
    # management entirely, which I think is even uglier.
    services.xserver.displayManager.sessionCommands = ''
      ${pkgs.xorg.xset}/bin/xset dpms force standby
      ${pkgs.xorg.xset}/bin/xset dpms force on
    '';
  };
}

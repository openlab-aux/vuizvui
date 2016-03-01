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
      linuxNextVersion = "20160226";
      mkKernel = import "${nixpkgs}/pkgs/os-specific/linux/kernel/generic.nix";
      t100haKernel = mkKernel rec {
        version = "4.5-rc5";
        modDirVersion = "4.5.0-rc5-next-${linuxNextVersion}";
        extraMeta.branch = "4.5";

        src = pkgs.fetchgit {
          url = "git://git.kernel.org/pub/scm/linux/kernel/git/next/"
              + "linux-next.git";
          rev = "refs/tags/next-${linuxNextVersion}";
          sha256 = "0q39mnjyi8jany03b4ral34hicdrgjpab53hg712jzhbcngj5kh3";
        };

        kernelPatches = [
          { name = "backlight";
            patch = ./backlight.patch;
          }
          { name = "meta-keys";
            patch = ./meta-keys.patch;
          }
        ];

        extraConfig = ''
          MMC y
          MMC_BLOCK y
          MMC_SDHCI y
          MMC_SDHCI_ACPI y
          PINCTRL_CHERRYVIEW y
          INTEL_SOC_PMIC y

          AGP n
          DRM y
          DRM_I915 y

          # These do not compile as of 4.5.0-rc5-next-20160226:
          VIDEO_EM28XX n
          RAPIDIO n
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

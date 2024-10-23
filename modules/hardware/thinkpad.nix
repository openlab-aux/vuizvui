{ lib, config, pkgs, ... }:
let
  cfg = config.vuizvui.hardware.thinkpad;

in
{
  options.vuizvui.hardware.thinkpad = {
    enable = lib.mkEnableOption "thinkpad support";

    cpuType = lib.mkOption {
      type = lib.types.enum [ "intel" "amd" ];
      default = "intel";
      description = "The CPU type of the ThinkPad";
    };

    powerManagement = lib.mkOption {
      type = lib.types.enum [ "tlp" "auto-cpufreq" ];
      default = "tlp";
      description = ''
        The power management tool to use for the ThinkPad.
        - tlp: TLP Linux Advanced Power Management
        - auto-cpufreq: https://github.com/AdnanHodzic/auto-cpufreq

        Only enables the respective services, you might have to adjust settings manually.
      '';
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    (lib.mkIf (cfg.cpuType == "intel") {
      # We need to update the Intel microcode on every update,
      # otherwise there can be problems with newer kernels.
      hardware.cpu.intel.updateMicrocode = lib.mkDefault true;

    })
    (lib.mkIf (cfg.cpuType == "amd") {
      # We need to update the AMD microcode on every update,
      # otherwise there can be problems with newer kernels.
      hardware.cpu.amd.updateMicrocode = lib.mkDefault true;
    })
    (lib.mkIf (cfg.powerManagement == "tlp") {
      # TLP Linux Advanced Power Management
      services.tlp.enable = lib.mkDefault true;
      # ensure auto-cpufreq is always disabled (some hardware configs set it)
      services.auto-cpufreq.enable = lib.mkForce false;
    })
    (lib.mkIf (cfg.powerManagement == "auto-cpufreq") {
      services.auto-cpufreq.enable = lib.mkDefault true;
      # ensure tlp is always disabled (some hardware configs set it)
      services.tlp.enable = lib.mkForce false;
    })
    {
      # read acpi stats (e.g. battery)
      environment.systemPackages = [ pkgs.acpi ];

      # for wifi & cpu microcode (amd)
      hardware.enableRedistributableFirmware = lib.mkDefault true;

      hardware.trackpoint = lib.mkDefault {
        enable = true;
        emulateWheel = true;
        speed = 250;
        sensitivity = 140;
      };

      boot = {
        # acpi_call is required for some tlp features, e.g. discharge/recalibrate
        kernelModules = [
          "acpi_call"
        ];

        extraModulePackages = [
          config.boot.kernelPackages.acpi_call
        ];
      };
    }
  ]);
}

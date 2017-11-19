{ config, pkgs, lib, ... }:

let
  cfg = config.vuizvui.hardware.t100ha;
  desc = "hardware support for the ASUS T100HA convertible";

in {
  options.vuizvui.hardware.t100ha.enable = lib.mkEnableOption desc;

  config = lib.mkIf cfg.enable {
    hardware.firmware = lib.singleton (pkgs.runCommand "t100ha-firmware" {
      params = ./brcmfmac43340-sdio.txt;
      fwpkg = pkgs.firmwareLinuxNonfree;
      install = "install -vD -m 0644";
    } ''
      for fw in brcm/brcmfmac43340-sdio intel/fw_sst_22a8; do
        $install "$fwpkg/lib/firmware/$fw.bin" "$out/lib/firmware/$fw.bin"
      done
      $install "$params" "$out/lib/firmware/brcm/brcmfmac43340-sdio.txt"
    '');

    boot.kernelPackages = let
      t100haKernel = pkgs.linux_4_14.override {
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

          # Backlight
          PWM y
          PWM_SYSFS y
          PWM_CRC y
          GPIO_CRYSTAL_COVE y

          # GPU
          AGP n
          DRM y
          DRM_I915 m

          # Thermal
          INT3406_THERMAL m
          INT340X_THERMAL m

          # GPIO
          PINCTRL_CHERRYVIEW y

          # I2C
          I2C_DESIGNWARE_BAYTRAIL y
          I2C_DESIGNWARE_PLATFORM y

          # HID
          INTEL_HID_EVENT y

          # MEI
          INTEL_MEI y
          INTEL_MEI_TXE y
        '';
      };
    in pkgs.linuxPackagesFor t100haKernel;

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

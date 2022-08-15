{ config, pkgs, unfreePkgs, unfreeAndNonDistributablePkgs, lib, ... }:

let
  mainDisk = "ata-WDC_WD5000LPVX-22V0TT0_WD-WXG1E2559AYH";
  rootUUID = "dbbd5a35-3ac0-4d5a-837d-914457de14a4";

in {
  boot = {
    initrd.availableKernelModules = [
      "xhci_pci" "ehci_pci" "ahci" "usb_storage" "sd_mod" "sr_mod"
      "rtsx_pci_sdmmc"
    ];
    kernelModules = [ "kvm-intel" "wl" ];
    kernelPackages = pkgs.linuxPackages_latest;
    extraModulePackages = [ config.boot.kernelPackages.broadcom_sta ];
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/${rootUUID}";
    fsType = "btrfs";
    options = [ "compress=zstd" "space_cache" "noatime" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/534F-980B";
    fsType = "vfat";
  };

  hardware.enableAllFirmware = true;
  hardware.cpu.intel.updateMicrocode = true;

  networking.hostName = "brawndo";

  nix.settings.max-jobs = 4;

  nixpkgs.config.allowUnfree = true; # XXX: More granularity!

  environment.systemPackages = with pkgs; [
    vuizvui.aszlig.axbo chromium firefox gpodder opentyrian unfreePkgs.steam
    python3 unfreeAndNonDistributablePkgs.vscode-with-extensions
  ];

  i18n.defaultLocale = "en_US.UTF-8";

  services = {
    deluge.enable = true;
    printing.drivers = [ pkgs.cups-bjnp pkgs.cnijfilter2 ];
  };

  swapDevices = lib.singleton { label = "swap"; };

  vuizvui.user.aszlig.profiles.managed.enable = true;
  vuizvui.user.aszlig.profiles.managed.mainUser = "dwenola";
}

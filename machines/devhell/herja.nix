{ config, pkgs, lib, ... }:

{
  vuizvui.user.devhell.profiles.base.enable = true;

  boot = {
#    binfmt.emulatedSystems = [ "aarch64-linux" ];
    loader = {
      timeout = 2;
      systemd-boot = {
        enable = true;
      };

      efi.canTouchEfiVariables = true;
    };

    initrd = {
      availableKernelModules = [ "nvme" "ehci_pci" "xhci_pci" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
      kernelModules = [ "fuse" "amdgpu" ];
    };

    kernelModules = [ "kvm-amd" "acpi_call" ];
    kernelParams = [ "acpi_backlight=native" "mem_sleep_default=deep" ];
    extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];
  };

  hardware = {
    cpu.amd.updateMicrocode = true;
    enableRedistributableFirmware = true;
    trackpoint = {
      enable = true;
      emulateWheel = true;
    };
    graphics = {
      enable = true;
      extraPackages = with pkgs; [
        libvdpau-va-gl
        libva-vdpau-driver
      ];
    };
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/34c21c91-6722-427d-882b-6da0e2f57f50";
    fsType = "btrfs";
    options = [
      "space_cache"
      "compress=zstd"
      "noatime"
      "autodefrag"
    ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/18B4-40B0";
    fsType = "vfat";
  };

  # FIXME Check if this is still necessary in the future
  systemd.services.systemd-networkd-wait-online.enable = lib.mkForce false;

  # XXX Make networkd ignore virtual network interfaces
  systemd.network.networks."99-libvirt" = {
    matchConfig.Name = "virbr* vnet*";
    linkConfig.Unmanaged = true;
  };

  networking = {
    hostName = "herja";
    wireless.iwd.enable = true;
    useNetworkd = true;
    interfaces = {
      enp6s0f3u1.useDHCP = true;
      enp2s0f0.useDHCP = true;
      wlan0.useDHCP = true;
    };
  };

  powerManagement = {
    powertop.enable = true;
    cpuFreqGovernor = "powersave";
  };

  nix = {
    settings.max-jobs = lib.mkDefault 16;
 #   settings.extra-platforms = config.boot.binfmt.emulatedSystems;
  };

  i18n = {
    defaultLocale = "en_GB.UTF-8";
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "uk";
  };

  #### Machine-specific service configuration ####

  vuizvui.user.devhell.profiles.services.enable = true;

  services = {
    gnome.gnome-keyring.enable = true;
    printing = {
      enable = false;
      drivers = [ pkgs.hplip ];
    };
    offlineimap = {
      enable = true;
      install = true;
      path = [ pkgs.notmuch ];
    };
    syncthing = {
      enable = false;
    };
  };

  services.tlp.enable = true;
  services.illum.enable = true;
  services.fwupd.enable = true;

  #### Machine-specific packages configuration ####

  vuizvui.user.devhell.profiles.packages.enable = true;

  nixpkgs.config.mpv.vaapiSupport = true;

  programs = {
    light.enable = false;
  };

  environment.systemPackages = with pkgs; [
    aircrack-ng
    fwupd
    horst
    ipmitool
    iw
    libva
    minicom
    acpica-tools
    pmutils
    libva-vdpau-driver
    vdpauinfo
  ];

  system.stateVersion = "24.05";
}

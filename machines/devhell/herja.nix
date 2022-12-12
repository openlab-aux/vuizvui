{ config, pkgs, lib, ... }:

{
  vuizvui.user.devhell.profiles.base.enable = true;

  boot = {
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
    kernelParams = [ "acpi_backlight=native" ];
    extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];
  };

  hardware = {
    cpu.amd.updateMicrocode = true;
    enableRedistributableFirmware = true;
    trackpoint = {
      enable = true;
      emulateWheel = true;
    };
    opengl = {
      enable = true;
      driSupport = true;
      extraPackages = with pkgs; [
        libvdpau-va-gl
        vaapiVdpau
        amdvlk
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
      enable = true;
      drivers = [ pkgs.hplip ];
    };
    offlineimap = {
      enable = true;
      install = true;
      path = [ pkgs.notmuch ];
    };
    syncthing = {
      enable = true;
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
    wavemon.enable = true;
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
    vaapiVdpau
    vdpauinfo
    xbindkeys
  ];
}

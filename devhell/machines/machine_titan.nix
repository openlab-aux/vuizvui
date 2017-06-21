{ config, pkgs, lib, ... }:

{
  imports = [ ./machine_common.nix ];

  boot = {
    loader = {
      timeout = 2;
      systemd-boot = {
        enable = true;
      };

      efi.canTouchEfiVariables = true;
    };

    initrd = {
      availableKernelModules = [ "xhci_hcd" "ehci_pci" "ahci" "usb_storage" ];
      kernelModules = [ "fuse" ];
    };

    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];

    kernelParams = [ "elevator=noop" ];
  };

  hardware = {
    opengl = {
      enable = true;
      extraPackages = [ pkgs.libvdpau-va-gl pkgs.vaapiVdpau pkgs.vaapiIntel ];
    };
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/09d1155f-e7dd-4754-ae01-44da2517d5f0";
    fsType = "btrfs";
    options = [
      "autodefrag"
      "space_cache"
      "compress=lzo"
      "noatime"
      "ssd"
    ];
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/fecde631-8661-4a0e-88e6-5ce5b551847a"; }
  ];

  nix = {
    maxJobs = 4;
    extraOptions = ''
      auto-optimise-store = true
    '';
  };

  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "uk";
    defaultLocale = "en_GB.UTF-8";
  };
}

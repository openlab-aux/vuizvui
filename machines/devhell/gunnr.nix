{ config, pkgs, lib, ... }:

{
  vuizvui.user.devhell.profiles.base.enable = true;

  system.stateVersion = "24.05";

  boot = {
    swraid.enable = true;
    loader = {
      grub  = {
        enable = true;
        copyKernels = true;
        devices = [ "/dev/sda" "/dev/sdb" ];
      };
    };

    zfs = {
      enableUnstable = true;
      requestEncryptionCredentials = true;
    };

    initrd = {
      availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
      kernelModules = [ "fuse" "amdgpu" ];
      postDeviceCommands = ''
        echo none > /sys/block/sda/queue/scheduler
        echo none > /sys/block/sdb/queue/scheduler
      '';
    };

    kernelParams = [ "pcie_aspm=off" ];
    kernelModules = [ "kvm-amd" ];
    extraModulePackages = with config.boot.kernelPackages; [ rtl88xxau-aircrack ];
    blacklistedKernelModules = [ ];
    kernelPackages = lib.mkForce
      config.boot.zfs.package.latestCompatibleLinuxPackages;
  };

  hardware = {
    cpu.amd.updateMicrocode = true;
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
    device = "zpool/root/nixos";
    fsType = "zfs";
  };

  fileSystems."/home" = {
    device = "zpool/home";
    fsType = "zfs";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/boot";
    fsType = "ext4";
  };

  # FIXME Check if this is still necessary in the future
  systemd.services.systemd-networkd-wait-online.enable = lib.mkForce false;

  networking = {
    hostName = "gunnr";
    hostId = "29e6affc";
    wireless.iwd.enable = true;
    useNetworkd = true;
    interfaces = {
      enp4s0.useDHCP = true;
      wlan0.useDHCP = true;
    };
  };

  nix = {
    settings.max-jobs = lib.mkDefault 16;
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "dvorak";
  };

  #### Machine-specific service configuration ####

  vuizvui.user.devhell.profiles.services.enable = true;

  services = {
    printing = {
      enable = true;
      drivers = [ pkgs.foo2zjs pkgs.hplip ];
    };
    syncthing = {
      enable = true;
    };
  };

  services.zfs.autoScrub.enable = true;

   services.timesyncd = {
     servers = [ "ntp.canterbury.ac.uk" ];
   };

   #### Machine-specific packages configuration ####

   vuizvui.user.devhell.profiles.packages.enable = true;

   nixpkgs.config.mpv.vaapiSupport = true;
   nixpkgs.config.mpv.bs2bSupport = true;

   environment.systemPackages = with pkgs; [
     glxinfo
     libva
     libvdpau-va-gl
     vaapiVdpau
     vdpauinfo
     xbindkeys
   ];
}

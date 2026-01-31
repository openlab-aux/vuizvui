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
      availableKernelModules = [ "ehci_pci" "ahci" "usb_storage" ];
      kernelModules = [ "fuse" ];
      postDeviceCommands = ''
        echo none > /sys/block/sda/queue/scheduler
      '';
    };

    kernelModules = [ "tp_smapi" ];
    kernelParams = [ "i915.enable_rc6=7" ];
    extraModulePackages = [ config.boot.kernelPackages.tp_smapi ];
  };

  hardware = {
    cpu.intel.updateMicrocode = true;
    enableRedistributableFirmware = true;
    bluetooth = {
      enable = true;
      powerOnBoot = false;
    };
    trackpoint = {
      enable = true;
      emulateWheel = true;
    };
    graphics = {
      enable = true;
      extraPackages = with pkgs; [
        libvdpau-va-gl
        libva-vdpau-driver
        intel-vaapi-driver
      ];
    };
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/4788e218-db0f-4fd6-916e-e0c484906eb0";
    fsType = "btrfs";
    options = [
      "space_cache"
      "compress=zstd"
      "noatime"
      "autodefrag"
    ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/BDBC-FC8B";
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
    hostName = "eir";
    wireless.iwd.enable = true;
    useNetworkd = true;
    interfaces = {
      enp0s25.useDHCP = true;
      wwp0s29u1u4.useDHCP = true;
      wlan0.useDHCP = true;
    };
  };

  powerManagement = {
    powertop.enable = true;
    cpuFreqGovernor = "powersave";
  };
  nix = {
    settings.max-jobs = lib.mkDefault 4;
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
    thermald.enable = true;
    tftpd.enable = false;
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

#  services.udev = {
#    extraRules = ''
#      SUBSYSTEM=="firmware", ACTION=="add", ATTR{loading}="-1"
#    '';
#  };
#
#  services.acpid = {
#    enable = true;
#    lidEventCommands = ''
#      LID="/proc/acpi/button/lid/LID/state"
#      state=`cat $LID | ${pkgs.gawk}/bin/awk '{print $2}'`
#      case "$state" in
#        *open*) ;;
#        *close*) systemctl suspend ;;
#        *) logger -t lid-handler "Failed to detect lid state ($state)" ;;
#      esac
#    '';
#  };

  services.tlp.enable = true;
  services.illum.enable = true;

  #### Machine-specific packages configuration ####

  vuizvui.user.devhell.profiles.packages.enable = true;

  nixpkgs.config.mpv.vaapiSupport = true;

  programs = {
    light.enable = false;
  };

  environment.systemPackages = with pkgs; [
    acpica-tools
    aircrack-ng
    bluetui
    bluez
    cdrtools
    dvdplusrwtools
    horst
    iw
    libva
    libva-vdpau-driver
    minicom
    pmutils
    vdpauinfo
  ];

  system.stateVersion = "24.05";
}

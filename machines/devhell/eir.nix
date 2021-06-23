{ config, pkgs, lib, ... }:

{
  vuizvui.user.devhell.profiles.base.enable = true;
  vuizvui.system.kernel.bfq.enable = true;

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
        echo noop > /sys/block/sda/queue/scheduler
      '';
    };

    kernelModules = [ "tp_smapi" ];
    extraModulePackages = [ config.boot.kernelPackages.tp_smapi ];
  };

  hardware = {
    cpu.intel.updateMicrocode = true;
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
        vaapiIntel
      ];
    };
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/4788e218-db0f-4fd6-916e-e0c484906eb0";
    fsType = "btrfs";
    options = [
      "autodefrag"
      "space_cache"
      "compress=zstd"
      "noatime"
    ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/BDBC-FC8B";
    fsType = "vfat";
  };

  swapDevices = [ ];

  # FIXME Check if this is still necessary in the future
  systemd.services.systemd-networkd-wait-online.enable = false;

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

  nix = {
    maxJobs = lib.mkDefault 4;
    extraOptions = ''
      auto-optimise-store = true
    '';
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

  services.udev = {
    extraRules = ''
      SUBSYSTEM=="firmware", ACTION=="add", ATTR{loading}="-1"
    '';
  };

  services.acpid = {
    enable = true;
    lidEventCommands = ''
      LID="/proc/acpi/button/lid/LID/state"
      state=`cat $LID | ${pkgs.gawk}/bin/awk '{print $2}'`
      case "$state" in
        *open*) ;;
        *close*) systemctl suspend ;;
        *) logger -t lid-handler "Failed to detect lid state ($state)" ;;
      esac
    '';
  };

  services.xserver = {
    enable = true;
    layout = "gb";
    videoDrivers = [ "modesetting" ];

    libinput = {
      enable = true;
      touchpad = {
        disableWhileTyping = true;
        middleEmulation = true;
      };
    };
#    synaptics = {
#      enable = true;
#      twoFingerScroll = true;
#      palmDetect = true;
#    };

    # XXX: Factor out and make DRY, because a lot of the stuff here is
    # duplicated in the other machine configurations.
    displayManager.sessionCommands = ''
      ${pkgs.nitrogen}/bin/nitrogen --restore &
      ${pkgs.rofi}/bin/rofi &
      ${pkgs.xorg.xrdb}/bin/xrdb "${pkgs.writeText "xrdb.conf" ''
        Xft.dpi:                     96
        Xft.antialias:               true
        Xft.hinting:                 full
        Xft.hintstyle:               hintslight
        Xft.rgba:                    rgb
        Xft.lcdfilter:               lcddefault
        Xft.autohint:                1
        Xcursor.theme:               Vanilla-DMZ-AA
        Xcursor.size:                22
        *.charClass:33:48,35:48,37:48,43:48,45-47:48,61:48,63:48,64:48,95:48,126:48,35:48,58:48
      ''}"
    '';
  };

  services.tlp = {
    enable = true;
    extraConfig = ''
      TLP_ENABLE = 1
      DISK_IDLE_SECS_ON_BAT=2
      MAX_LOST_WORK_SECS_ON_AC=15
      MAX_LOST_WORK_SECS_ON_BAT=60
      SCHED_POWERSAVE_ON_AC=0
      SCHED_POWERSAVE_ON_BAT=1
      NMI_WATCHDOG=0
      DISK_DEVICES="sda sdb"
      DISK_APM_LEVEL_ON_AC="254 254"
      DISK_APM_LEVEL_ON_BAT="254 127"
      DISK_IOSCHED="bfq bfq"
      SATA_LINKPWR_ON_AC=max_performance
      SATA_LINKPWR_ON_BAT=min_power
      PCIE_ASPM_ON_AC=performance
      PCIE_ASPM_ON_BAT=powersave
      WIFI_PWR_ON_AC=1
      WIFI_PWR_ON_BAT=5
      WOL_DISABLE=Y
      SOUND_POWER_SAVE_ON_AC=0
      SOUND_POWER_SAVE_ON_BAT=1
      SOUND_POWER_SAVE_CONTROLLER=Y
      RUNTIME_PM_ON_AC=on
      RUNTIME_PM_ON_BAT=auto
      RUNTIME_PM_ALL=1
      USB_AUTOSUSPEND=1
      USB_BLACKLIST_WWAN=1
      RESTORE_DEVICE_STATE_ON_STARTUP=0
      DEVICES_TO_DISABLE_ON_STARTUP="bluetooth wwan"
      DEVICES_TO_ENABLE_ON_STARTUP="wifi"
      DEVICES_TO_DISABLE_ON_SHUTDOWN="bluetooth wifi wwan"
      #DEVICES_TO_ENABLE_ON_SHUTDOWN=""
      START_CHARGE_THRESH_BAT0=70
      STOP_CHARGE_THRESH_BAT0=95
      #DEVICES_TO_DISABLE_ON_LAN_CONNECT="wifi wwan"
      #DEVICES_TO_DISABLE_ON_WIFI_CONNECT="wwan"
      #DEVICES_TO_DISABLE_ON_WWAN_CONNECT="wifi"
      #DEVICES_TO_ENABLE_ON_LAN_DISCONNECT="wifi wwan"
      #DEVICES_TO_ENABLE_ON_WIFI_DISCONNECT=""
      #DEVICES_TO_ENABLE_ON_WWAN_DISCONNECT=""
    '';
  };

  #### Machine-specific packages configuration ####

  vuizvui.user.devhell.profiles.packages.enable = true;

  nixpkgs.config.mpv.vaapiSupport = true;

  programs = {
    light.enable = true;
    wavemon.enable = true;
  };

  environment.systemPackages = with pkgs; [
    cdrtools
    dvdplusrwtools
    glxinfo
    horst
    iw
    libva
    libvdpau-va-gl
    minicom
    pmtools
    pmutils
    vaapiVdpau
    vdpauinfo
    xbindkeys
  ];
}

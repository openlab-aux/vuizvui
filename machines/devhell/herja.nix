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

  # FIXME Check if this is still necessary in the future
  systemd.services.systemd-networkd-wait-online.enable = false;

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

  nix.settings = {
    max-jobs = lib.mkDefault 16;
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

  services.xserver = {
    enable = true;
    layout = "gb";
    videoDrivers = [ "amdgpu" ];

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
      ${pkgs.xbindkeys}/bin/xbindkeys &
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

  #### Machine-specific packages configuration ####

  vuizvui.user.devhell.profiles.packages.enable = true;

  nixpkgs.config.mpv.vaapiSupport = true;

  programs = {
    light.enable = false;
    wavemon.enable = true;
  };

  environment.systemPackages = with pkgs; [
    aircrackng
    fwupd
    horst
    ipmitool
    iw
    libva
    minicom
    pmtools
    pmutils
    teams
    vaapiVdpau
    vdpauinfo
    xbindkeys
  ];
}

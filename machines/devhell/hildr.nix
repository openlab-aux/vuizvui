{ config, pkgs, lib, ... }:

{
  vuizvui.user.devhell.profiles.base.enable = true;
  vuizvui.user.devhell.programs.vim.enable = true;
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
      availableKernelModules = [ "xhci_hcd" "ahci" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
      kernelModules = [ "fuse" ];
    };

    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
  };

  hardware = {
    cpu.intel.updateMicrocode = true;
    opengl = {
      enable = true;
      extraPackages = [ pkgs.libvdpau-va-gl pkgs.vaapiVdpau pkgs.vaapiIntel ];
    };
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/3099f245-51cf-4ca8-b89c-269dbc0ad730";
    fsType = "btrfs";
    options = [
      "space_cache"
      "compress=zstd"
      "noatime"
      "autodefrag"
    ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/9344-E6FE";
    fsType = "vfat";
  };

  swapDevices = [ 
    { device = "/dev/disk/by-uuid/ff725995-b9a1-453f-9e6d-ba9bd6579db6"; }
  ];

  # FIXME Check if this is still necessary in the future
  systemd.services.systemd-networkd-wait-online.enable = false;

  # XXX Make networkd ignore virtual network interfaces
  systemd.network.networks."99-libvirt" = {
    matchConfig.Name = "virbr* vnet*";
    linkConfig.Unmanaged = true;
  };

  networking = {
    hostName = "hildr";
    wireless.enable = true;
    useNetworkd = true;
    interfaces = {
      enp0s31f6.useDHCP = true;
      wlp2s0.useDHCP = true;
    };
  };

  powerManagement = {
    powertop.enable = true;
    cpuFreqGovernor = "powersave";
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
    gnome3.gnome-keyring.enable = true;
    printing = {
      enable = true;
      drivers = [ pkgs.foo2zjs pkgs.cups-brother-hl1110 ];
    };
    offlineimap = {
      enable = true;
      install = true;
      path = [ pkgs.notmuch ];
    };
    syncthing = {
      enable = true;
      user = "dev";
      dataDir = "/home/dev/syncthing/";
    };
  };

  services.actkbd = {
    enable = true;
    bindings = [
      { keys = [ 224 ]; events = [ "key" ]; command = "${pkgs.light}/bin/light -U 5"; }
      { keys = [ 225 ]; events = [ "key" ]; command = "${pkgs.light}/bin/light -A 5"; }
    ];
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
      disableWhileTyping = true;
      middleEmulation = true;
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
        
        ! Copyright (c) 2016-present Arctic Ice Studio <development@arcticicestudio.com>
        ! Copyright (c) 2016-present Sven Greb <code@svengreb.de>
        
        ! Project:    Nord XResources
        ! Version:    0.1.0
        ! Repository: https://github.com/arcticicestudio/nord-xresources
        ! License:    MIT
        
        #define nord0 #2E3440
        #define nord1 #3B4252
        #define nord2 #434C5E
        #define nord3 #4C566A
        #define nord4 #D8DEE9
        #define nord5 #E5E9F0
        #define nord6 #ECEFF4
        #define nord7 #8FBCBB
        #define nord8 #88C0D0
        #define nord9 #81A1C1
        #define nord10 #5E81AC
        #define nord11 #BF616A
        #define nord12 #D08770
        #define nord13 #EBCB8B
        #define nord14 #A3BE8C
        #define nord15 #B48EAD
        
        *.foreground:   nord4
        *.background:   nord0
        *.cursorColor:  nord4
        *fading: 35
        *fadeColor: nord3
        
        *.color0: nord1
        *.color1: nord11
        *.color2: nord14
        *.color3: nord13
        *.color4: nord9
        *.color5: nord15
        *.color6: nord8
        *.color7: nord5
        *.color8: nord3
        *.color9: nord11
        *.color10: nord14
        *.color11: nord13
        *.color12: nord9
        *.color13: nord15
        *.color14: nord7
        *.color15: nord6
      ''}"
    '';
  };

  virtualisation.podman.enable = true;

  #### Machine-specific packages configuration ####

  vuizvui.user.devhell.profiles.packages.enable = true;

  nixpkgs.config.mpv.vaapiSupport = true;

  programs = {
    light.enable = true;
    wavemon.enable = true;
  };

  environment.systemPackages = with pkgs; [
    aircrackng
    cdrtools
    dvdplusrwtools
    horst
    ipmitool
    iw
    libva
    libvdpau-va-gl
    minicom
    pmtools
    pmutils
    podman
    podman-compose
    reaverwps
    snort
    teams
    vaapiVdpau
    vdpauinfo
    xbindkeys
  ];
}

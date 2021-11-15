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
      drivers = [ pkgs.foo2zjs pkgs.cups-brother-hl1110 ];
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
    reaverwps
    snort
    teams
    vaapiVdpau
    vdpauinfo
    xbindkeys
  ];
}

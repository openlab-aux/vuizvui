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

  # XXX Ensure that these are added in addition to the DHCP provided DNS servers
  systemd.network.networks."99-main".dns = [ "1.1.1.1" "1.0.0.1" ];

  networking = {
    hostName = "hildr";
    wireless.iwd.enable = true;
    useNetworkd = true;
    interfaces = {
      enp0s31f6.useDHCP = true;
      wlan0.useDHCP = true;
    };
  };

  powerManagement = {
    powertop.enable = true;
    cpuFreqGovernor = "powersave";
  };

  virtualisation.docker.enable = true;

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
    font = "lat9w-16";
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
      ${pkgs.xscreensaver}/bin/xscreensaver -no-splash &
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
        *background:                 #121212
        *foreground:                 #babdb6
        ${lib.concatMapStrings (xterm: ''
            ${xterm}.termName:       xterm-256color
            ${xterm}*bellIsUrgent:   true
            ${xterm}*utf8:           1
            ${xterm}*locale:             true
            ${xterm}*utf8Title:          true
            ${xterm}*utf8Fonts:          1
            ${xterm}*utf8Latin1:         true
            ${xterm}*dynamicColors:      true
            ${xterm}*eightBitInput:      true
            ${xterm}*faceName:           xft:DejaVu Sans Mono for Powerline:pixelsize=9:antialias=true:hinting=true
            ${xterm}*faceNameDoublesize: xft:Unifont:pixelsize=12:antialias=true:hinting=true
            ${xterm}*cursorColor:        #545f65
        '') [ "UXTerm" "XTerm" ]}
      ''}"
    '';
  };

  #### Machine-specific packages configuration ####

  vuizvui.user.devhell.profiles.packages.enable = true;

  nixpkgs.config.mpv.vaapiSupport = true;

  programs.light.enable = true;

  environment.systemPackages = with pkgs; [
    aircrackng
    cdrtools
    docker
    dvdplusrwtools
    glxinfo
    horst
    ipmitool
    iw
    kismet
    libva
    libvdpau-va-gl
    minicom
    pamixer
    pmtools
    pmutils
    reaverwps
    signal-desktop
    snort
    vaapiVdpau
    vdpauinfo
    wavemon
    xbindkeys
  ];
}

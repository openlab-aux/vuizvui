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

  networking.hostName = "titan";
  networking.wireless.enable = true;
  networking.connman.enable = true;
  networking.wicd.enable = false;
  networking.networkmanager.enable = false;

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

  #### Machine-specific service configuration ####

  vuizvui.user.devhell.profiles.services.enable = true;

  services = {
    tftpd.enable = false;
    gnome3.gnome-keyring.enable = true;
    printing.enable = false;
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
    videoDrivers = [ "intel" ];

    synaptics = {
      enable = true;
      twoFingerScroll = true;
      palmDetect = true;
    };

    # XXX: Factor out and make DRY, because a lot of the stuff here is
    # duplicated in the other machine configurations.
    displayManager.sessionCommands = ''
      ${pkgs.xbindkeys}/bin/xbindkeys &
      ${pkgs.nitrogen}/bin/nitrogen --restore &
      #${pkgs.networkmanagerapplet}/bin/nm-applet &
      ${pkgs.connmanui}/bin/connman-ui-gtk &
      ${pkgs.xscreensaver}/bin/xscreensaver -no-splash &
      ${pkgs.pasystray}/bin/pasystray &
      ${pkgs.compton}/bin/compton -f &
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
        ! ------------------------------------------------------------------------------
        ! ROFI Color theme & Settings
        ! ------------------------------------------------------------------------------
        rofi.modi: run
        rofi.opacity: 85
        rofi.width: 100
        rofi.lines: 3
        rofi.padding: 300
        rofi.bw: 0
        rofi.eh: 2
        rofi.color-enabled: true
        rofi.color-window: #393939, #393939, #268bd2
        rofi.color-normal: #393939, #ffffff, #393939, #268bd2, #ffffff
        rofi.color-active: #393939, #268bd2, #393939, #268bd2, #205171
        rofi.color-urgent: #393939, #f3843d, #393939, #268bd2, #ffc39c
      ''}"
    '';
  };
}

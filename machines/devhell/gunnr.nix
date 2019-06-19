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
      availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
      kernelModules = [ "fuse" ];
    };

    kernelParams = [ ];
    kernelMOdules = [ "kvm-amd" ];
    extraModulePackages = [ ];
    blacklistedKernelModules = [ ];
  };

  hardware = {
    opengl = {
      enable = true;
      extraPackages = [ pkgs.libvdpau-va-gl pkgs.vaapiVdpau ];
    };
  };

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "btrfs";
    options = [
      "space_cache"
      "compress=zstd"
      "noatime"
      "autodefrag"
      "discard"
      "ssd"
    ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/BOOT";
    fsType = "vfat";
  };

  zramSwap.enable = true;

  systemd.network.networks."99-main".dns = [ "1.1.1.1" "8.8.8.8" ];

  networking = {
    hostName = "gunnr";
    wireless.enable = false;
    useNetworkd = true;
    proxy = {
      default = "http://wproxy.canterbury.ac.uk:3128/";
      noProxy = "127.0.0.1,localhost,canterbury.ac.uk";
    };
  };

  powerManagement = {
    powertop.enable = true;
    cpuFreqGovernor = "ondemand";
  };

  nix = {
    maxJobs = lib.mkDefault = 16;
    extraOptions = ''
      auto-optimise-store = true
    '';
  };

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "uk";
    defaultLocale = "en_GB.UTF-8";
  };

  #### Machine-specific service configuration ####

  vuizvui.user.devhell.profiles.services.enable = true;

  services.compton = {
    enable = true;
    backend = "glx";
    vSync = "drm";
    extraOptions = ''
      inactive-dim = 0.2;
    '';
  };

  services.xserver = {
    enable = true;
    layout = "gb";
    videoDrivers = [ "modesetting" ];

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
   nixpkgs.config.mpv.bs2bSupport = true;

   environment.systemPackages = with pkgs; [
     cdrtools
     docker
     dvdplusrwtools
     glxinfo
     ipmitool
     libva
     libvdpau-va-gl
     pamixer
     vaapiVdpau
     vdpauinfo
   ];
}

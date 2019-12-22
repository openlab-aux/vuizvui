{ config, pkgs, lib, ... }:

{
  vuizvui.user.devhell.profiles.base.enable = true;
  vuizvui.system.kernel.bfq.enable = true;

  boot = {
    loader.grub = {
      enable = true;
      version = 2;
      devices = [
        "/dev/disk/by-id/ata-ST31500541AS_6XW0NK21"
        "/dev/disk/by-id/ata-ST31500541AS_6XW0P0CW"
        "/dev/disk/by-id/ata-Samsung_SSD_850_EVO_250GB_S21PNSAG848626F"
        "/dev/disk/by-id/ata-Samsung_SSD_850_EVO_250GB_S21PNSAG848674K"
      ];
    };

    initrd = {
      availableKernelModules = [ "ehci_pci" "ahci" "firewire_ohci" "usbhid" "usb_storage" ];
      kernelModules = [ "fuse" ];
    };

    kernelParams = [ "pci=noaer" ];
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
    blacklistedKernelModules = [ "pcspkr" ];
  };

  hardware = {
    cpu.intel.updateMicrocode = true;
    opengl = {
      extraPackages = [ pkgs.vaapiVdpau ];
    };
  };

  fileSystems."/" = {
    label = "nixos";
    fsType = "btrfs";
    options = [
      "autodefrag"
      "space_cache"
      "compress=lzo"
      "noatime"
      "ssd"
    ];
  };

  fileSystems."/home" = {
    label = "home";
    fsType = "btrfs";
    options = [
      "autodefrag"
      "space_cache"
      "compress=lzo"
      "noatime"
    ];
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/16bd9abd-6af5-4a24-8ea5-58adc51e9641"; }
    { device = "/dev/disk/by-uuid/279708cb-f9c3-4a37-a064-80ff85a66f88"; }
    { device = "/dev/disk/by-uuid/0c2409c3-e824-4759-a9ad-9bfcea1e73bb"; }
    { device = "/dev/disk/by-uuid/3f1835a8-5587-4963-9b6c-66ecb36059de"; }
  ];

  networking.hostName = "sigrun";
  networking.wireless.enable = false;
  networking.useNetworkd = true;

  nix.maxJobs = 8;

  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "dvorak";
  };

  powerManagement.powerUpCommands = ''
    ${pkgs.hdparm}/sbin/hdparm -B 255 \
  /dev/disk/by-id/ata-ST31500541AS_6XW0NK21
    ${pkgs.hdparm}/sbin/hdparm -B 255 \
  /dev/disk/by-id/ata-ST31500541AS_6XW0P0CW
  '';

  #### Machine-specific service configuration ####

  vuizvui.user.devhell.profiles.services.enable = true;

  services = {
    printing = {
      enable = true;
      drivers = [ pkgs.hplipWithPlugin ];
    };
    thermald.enable = true;
    timesyncd.enable = true;
    resolved.enable = true;
    canto-daemon.enable = true;
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

  services.xserver = {
    enable = true;
    layout = "dvorak";
    videoDrivers = [ "ati" ];

    serverLayoutSection = ''
      Screen "Center/Right"
      Screen "Left" LeftOf "Center/Right"
    '';

    config = ''
      Section "ServerLayout"
        Identifier          "Multihead layout"
        Screen              "Center/Right"
        Screen              "Left" LeftOf "Center/Right"
      EndSection

      Section "Device"
        Identifier          "Radeon HD 4650 PCIEx8"
        Driver              "radeon"
        BusId               "PCI:2:0:0"

        Option              "monitor-DVI-1" "Left monitor"
      EndSection

      Section "Device"
        Identifier          "Radeon HD 4650 PCIEx16"
        Driver              "radeon"
        BusID               "PCI:1:0:0"

        Option              "monitor-DVI-0" "Center monitor"
        Option              "monitor-HDMI-0" "Right monitor"
      EndSection

      Section "Screen"
        Identifier          "Center/Right"
        Monitor             "Left monitor"
        Device              "Radeon HD 4650 PCIEx16"
      EndSection

      Section "Screen"
        Identifier          "Left"
        Device              "Radeon HD 4650 PCIEx8"
      EndSection

      Section "Monitor"
        Identifier          "Left monitor"
      EndSection

      Section "Monitor"
        Identifier          "Center monitor"
        Option              "LeftOf" "Right monitor"
        Option              "Primary" "true"
      EndSection

      Section "Monitor"
        Identifier          "Right monitor"
      EndSection
    '';

    # XXX: Factor out and make DRY, because a lot of the stuff here is
    # duplicated in the other machine configurations.
    displayManager.sessionCommands = ''
      ${pkgs.xorg.xsetroot}/bin/xsetroot -solid black
      ${pkgs.xscreensaver}/bin/xscreensaver -no-splash &
      ${pkgs.rofi}/bin/rofi &
      ${pkgs.xorg.xrdb}/bin/xrdb "${pkgs.writeText "xrdb.conf" ''
        Xft.dpi:              96
        Xft.antialias:        true
        Xft.hinting:          full
        Xft.hintstyle:        hintslight
        Xft.rgba:             rgb
        Xft.lcdfilter:        lcddefault
        Xft.autohint:         1
        XTerm.termName:       xterm-256color
        XTerm*bellIsUrgent:   true
        XTerm*utf8:           1
        XTerm*locale:         true
        XTerm*utf8Title:      true
        XTerm*utf8Fonts:      true
        XTerm*utf8Latin1:     true
        XTerm*dynamicColors:  true
        XTerm*eightBitInput:  true
        Xcursor.theme:        Vanilla-DMZ-AA
        Xcursor.size:         22
        *.charClass:33:48,35:48,37:48,43:48,45-47:48,61:48,63:48,64:48,95:48,126:48,35:48,58:48
        XTerm*faceName:       xft:DejaVu Sans Mono for Powerline:pixelsize=12:antialias=true:hinting=true
        XTerm*faceNameDoublesize: xft:Unifont:pixelsize=12:antialias=true:hinting=true
        XTerm*cursorColor:    #545f65
        XTerm*saveLines:      10000
        ! Base16 Twilight
        ! Scheme: David Hart (http://hart-dev.com)
        #define base00 #1e1e1e
        #define base01 #323537
        #define base02 #464b50
        #define base03 #5f5a60
        #define base04 #838184
        #define base05 #a7a7a7
        #define base06 #c3c3c3
        #define base07 #ffffff
        #define base08 #cf6a4c
        #define base09 #cda869
        #define base0A #f9ee98
        #define base0B #8f9d6a
        #define base0C #afc4db
        #define base0D #7587a6
        #define base0E #9b859d
        #define base0F #9b703f
        *.foreground:   base05
        *.background:   base00
        *.cursorColor:  base05
        *.color0:       base00
        *.color1:       base08
        *.color2:       base0B
        *.color3:       base0A
        *.color4:       base0D
        *.color5:       base0E
        *.color6:       base0C
        *.color7:       base05
        *.color8:       base03
        *.color9:       base09
        *.color10:      base01
        *.color11:      base02
        *.color12:      base04
        *.color13:      base06
        *.color14:      base0F
        *.color15:      base07
        ! ------------------------------------------------------------------------------
        ! ROFI Color theme & Settings
        ! ------------------------------------------------------------------------------
        rofi.modi: run
        rofi.opacity: 85
        rofi.width: 100
        rofi.lines: 3
        rofi.padding: 450
        rofi.bw: 0
        rofi.eh: 2
        rofi.color-enabled: true
        rofi.color-window: #393939, #393939, #268bd2
        rofi.color-normal: #393939, #ffffff, #393939, #268bd2, #ffffff
        rofi.color-active: #393939, #268bd2, #393939, #268bd2, #205171
        rofi.color-urgent: #393939, #f3843d, #393939, #268bd2, #ffc39c
      ''}"

      DISPLAY=:0.1 ${pkgs.windowmaker}/bin/wmaker &
    '';
  };

  #### Machine-specific packages configuration ####

  vuizvui.user.devhell.profiles.packages.enable = true;

  nixpkgs.config.chromium.enablePepperFlash = true;
  nixpkgs.config.mpv.bs2bSupport = true;

  environment.systemPackages = with pkgs; [
    #ipfs
    #scummvm
    abook
    canto-curses
    cli-visualizer
    cmus
    handbrake
    hplip
    nzbget
    slrn
  ];
}

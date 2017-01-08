{ config, pkgs, lib, ... }:

{
  imports = [ ./services_common.nix ];

  services = {
    tftpd.enable = true;
    gnome3.gnome-keyring.enable = true;
    printing.enable = false;
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
    videoDrivers = [ "intel" ];

    desktopManager.kde5 = {
      enable = true;
    };

    synaptics = {
      enable = true;
      twoFingerScroll = true;
      palmDetect = true;
    };

    displayManager.sessionCommands = ''
      #${pkgs.xbindkeys}/bin/xbindkeys &
      #${pkgs.nitrogen}/bin/nitrogen --restore &
      #${pkgs.networkmanagerapplet}/bin/nm-applet &
      #${pkgs.connmanui}/bin/connman-ui-gtk &
      #${pkgs.xscreensaver}/bin/xscreensaver -no-splash &
      #${pkgs.pasystray}/bin/pasystray &
      #${pkgs.compton}/bin/compton -f &
      #${pkgs.rofi}/bin/rofi &
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
        XTerm*utf8Fonts:      1
        XTerm*utf8Latin1:     true
        XTerm*dynamicColors:  true
        XTerm*eightBitInput:  true
        Xcursor.theme:        Vanilla-DMZ-AA
        Xcursor.size:         22
        *.charClass:33:48,35:48,37:48,43:48,45-47:48,61:48,63:48,64:48,95:48,126:48,35:48,58:48
        XTerm*faceName:       xft:DejaVu Sans Mono for Powerline:pixelsize=11:antialias=true:hinting=true
        XTerm*faceNameDoublesize: xft:Unifont:pixelsize=12:antialias=true:hinting=true
        XTerm*cursorColor:    #545f65
        *background:          #121212
        *foreground:          #babdb6
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
      DISK_IOSCHED="noop cfq"
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
      START_CHARGE_THRESH_BAT0=75
      STOP_CHARGE_THRESH_BAT0=80
      #DEVICES_TO_DISABLE_ON_LAN_CONNECT="wifi wwan"
      #DEVICES_TO_DISABLE_ON_WIFI_CONNECT="wwan"
      #DEVICES_TO_DISABLE_ON_WWAN_CONNECT="wifi"
      #DEVICES_TO_ENABLE_ON_LAN_DISCONNECT="wifi wwan"
      #DEVICES_TO_ENABLE_ON_WIFI_DISCONNECT=""
      #DEVICES_TO_ENABLE_ON_WWAN_DISCONNECT=""
    '';
  };
}

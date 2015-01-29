{ config, pkgs, lib, ... }:

{
  services.tftpd.enable = true;
  services.acpid.enable = true;
  services.gnome3.gnome-keyring.enable = true;
  services.printing.enable = false;

  services.tlp.enable = true;
  services.tlp.extraConfig = ''
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

  services.xserver = {
    enable = true;
    layout = "gb";
    videoDrivers = [ "intel" ];
    vaapiDrivers = [ pkgs.vaapiIntel ];

    synaptics = {
      enable = true;
      twoFingerScroll = true;
      palmDetect = true;
    };
  };

  services.xserver.displayManager.sessionCommands = ''
    ${pkgs.xorg.xsetroot}/bin/xsetroot -solid black
    ${pkgs.networkmanagerapplet}/bin/nm-applet &
    ${pkgs.pasystray}/bin/pasystray &
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
       XTerm*faceName:       xft:DejaVu Sans Mono for Powerline:pixelsize=11:antialia=true:hinting=true
       XTerm*cursorColor:    #545f65
       *background:          #121212
       *foreground:          #babdb6
    ''}"
  '';
}

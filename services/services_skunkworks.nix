{ config, pkgs, lib, ... }:

{
  services.printing.enable = true;
  services.timesyncd.enable = true;
  services.resolved.enable = true;
  services.haveged.enable = true;

  services.redshift = {
    enable = true;
    latitude = "51.2750";
    longitude = "1.0870";
  };

  services.xserver = {
    enable = true;
    layout = "dvorak";
    videoDrivers = [ "ati" ];
    vaapiDrivers = [ pkgs.vaapiVdpau ];

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

    displayManager.sessionCommands = ''
      ${pkgs.xorg.xsetroot}/bin/xsetroot -solid black
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
        *background:          #121212
        *foreground:          #babdb6
      ''}"

      DISPLAY=:0.1 ${pkgs.windowmaker}/bin/wmaker & 
    '';
  };
}

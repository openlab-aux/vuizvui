# A base configuration for my workstations.
{ config, pkgs, lib, ... }:
let
  myPkgs = import ./pkgs.nix { inherit pkgs lib myLib; };
  myLib  = import ./lib.nix  { inherit pkgs lib; };
  micro = myPkgs.micro;

  philip = myLib.philip;

in {

  imports = [
    ./base.nix
  ];

  config = {

    time.timeZone = "Europe/Berlin";

    vuizvui.services.profpatsch.hardware.externalMonitorControl.enable = true;

    networking = {
      # it will be ready when it’s ready,,
      enableIPv6 = false;
      # better for untrusted networks
      firewall = {
        enable = true;
        # for manual/temporary stuff
        allowedTCPPortRanges =
          [
          { from = 5037; to = 5037; }
          # test.profpatsch.de forwarding
          { from = 9999; to = 9999; }
          ];
        allowedUDPPortRanges =
          [
          { from = 5037; to = 5037; }
          ];
      };
    };

    vuizvui.services.upower = {
      enable = true;
      settings = {
        UPower = {
          UsePercentageForPolicy = true;
          CriticalPowerAction = "Suspend";

          PercentageLow = 15;
          PercentageCritical = 8;
          PercentageAction = 5;
        };
      };
    };

    services.fwupd.enable = true;

    ###################
    # Graphical System

    vuizvui.user.profpatsch.xserver.windowManager.xmonad = {
      enable = true;
      package = pkgs.vuizvui.profpatsch.tvl.users.Profpatsch.my-xmonad;
      updateSessionEnvironment = true;
    };

    xdg.portal = {
      enable = true;
      extraPortals = [
        pkgs.xdg-desktop-portal-gtk
      ];
      configPackages = [
        pkgs.xdg-desktop-portal-gtk
      ];
      config.common = {
        default = [
          "gtk"
        ];
        # "org.freedesktop.impl.portal.Settings" = [ "gtk" ];
      };
    };

    # TODO: libinput?
    services.libinput.enable = false;
    services.xserver = {
      enable = true;

      # otherwise xterm is enabled, creating an xterm that spawns the window manager.
      desktopManager.xterm.enable = false;

      xkb.layout = "de";
      xkb.variant = "neo";
      xkb.options = "altwin:swap_alt_win";
      serverFlagsSection = ''
        Option "StandbyTime" "10"
        Option "SuspendTime" "20"
        Option "OffTime" "30"
      '';

      displayManager = {
        sessionCommands = ''
            #TODO add as nixpkg
            export PATH+=":$HOME/scripts" #add utility scripts
            export PATH+=":$HOME/bin" #add user-specific binaries (filled by nix-home)
            export EDITOR=micro
            export TERMINAL=terminal-emulator

            ${pkgs.xorg.xset}/bin/xset r rate 250 35

            set-background &
            # TODO xbindkeys user service file
            ${lib.getBin pkgs.xbindkeys}/bin/xbindkeys
            # synchronize clipboards
            ${lib.getBin pkgs.autocutsel}/bin/autocutsel -s PRIMARY &
          '';
      };
      synaptics = {
        enable = true;
        minSpeed = "0.6";
        maxSpeed = "1.5";
        accelFactor = "0.015";
        twoFingerScroll = true;
        vertEdgeScroll = false;
      };

    };

    services.gnome.gnome-settings-daemon.enable = true;

    fonts.fontconfig = {
      enable = true;
      defaultFonts = {
        monospace = [ "Source Code Pro" "DejaVu Sans Mono" ]; # TODO does not work
        sansSerif = [ "Liberation Sans" ];
      };
    };


    programs.ssh.startAgent = false;

    ###########
    # Packages

    environment.sessionVariables = {
      EDITOR = "micro";

      # TODO: required? old msg: This is important so that the xdg-desktop-portal-gtk will use the gtk.portal config arghhh
      XDG_CURRENT_DESKTOP = "gnome";

    };

    environment.systemPackages = with pkgs;
    let
      # of utmost necessity for me to function
      basePkgs = [
        smartmontools     # check disk state
        stow              # dotfile management
        wirelesstools     # iwlist (wifi scan)
        gitFull           # git with send-email
      ];
      # minimal set of gui applications
      guiPkgs = [
        dmenu             # minimal launcher
        (pkgs.vuizvui.profpatsch.binify { exe = pkgs.vuizvui.profpatsch.xdg-open; name = "xdg-open"; }) # override the crap freedesktop crap
      ];
    in basePkgs ++ guiPkgs;

    # friendly user shell
   programs.fish.enable = true;

    ###########
    # Services

    # services.openssh.enable = true;

    vuizvui.programs.fish.fasd.enable = true;

    ########
    # Users

    users.users = { inherit philip; };

  };
}

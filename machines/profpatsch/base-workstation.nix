# A base configuration for my workstations.
{ pkgs, lib, ... }:
let
  myPkgs = import ./pkgs.nix { inherit pkgs lib myLib; };
  myLib  = import ./lib.nix  { inherit pkgs lib; };

  philip = myLib.philip;

in {

  imports = [
    ./base.nix
  ];

  config = {

    time.timeZone = "Europe/Berlin";

    networking = {
      # it will be ready when it’s ready,,
      enableIPv6 = false;
      # better for untrusted networks
      firewall = {
        enable = true;
        # for manual/temporary stuff
        allowedTCPPortRanges =
          [{ from = 9990; to = 9999; }];
        allowedUDPPortRanges =
          [{ from = 9990; to = 9999; }];
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
    };
    services.xserver = {
      enable = true;

      # otherwise xterm is enabled, creating an xterm that spawns the window manager.
      desktopManager.xterm.enable = false;

      layout = "de";
      xkbVariant = "neo";
      xkbOptions = "altwin:swap_alt_win";
      serverFlagsSection = ''
        Option "StandbyTime" "10"
        Option "SuspendTime" "20"
        Option "OffTime" "30"
      '';

      displayManager = {
        sessionCommands = with pkgs; ''
            #TODO add as nixpkg
            export PATH+=":$HOME/scripts" #add utility scripts
            export PATH+=":$HOME/bin" #add user-specific binaries (filled by nix-home)
            export EDITOR="emacsclient --create-frame"
            export TERMINAL=terminal-emulator

            ${xorg.xset}/bin/xset r rate 250 35

            set-background &
            # TODO xbindkeys user service file
            ${lib.getBin xbindkeys}/bin/xbindkeys
            # synchronize clipboards
            ${lib.getBin autocutsel}/bin/autocutsel -s PRIMARY &
          '';
      };

      # TODO: libinput?
      libinput.enable = false;
      synaptics = {
        enable = true;
        minSpeed = "0.6";
        maxSpeed = "1.5";
        accelFactor = "0.015";
        twoFingerScroll = true;
        vertEdgeScroll = false;
      };

    };

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

    environment.sessionVariables = { EDITOR = "${myPkgs.vim}/bin/vim"; };

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

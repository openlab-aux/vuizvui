# A base configuration for Thinkpads.
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

    ###########
    # Hardware

    boot.loader = {
      grub.enable = true;
      grub.version = 2;
    };

    hardware.cpu.intel.updateMicrocode = true;

    networking = {
      # better for untrusted networks
      firewall = {
        enable = true;
        # for manual/temporary stuff
        allowedTCPPortRanges =
          [{ from = 9990; to = 9999; }];
      };
    };

    i18n = {
      consoleFont = "lat9w-16";
      consoleKeyMap = "neo";
    };

    # Enables drivers, acpi, power management
    vuizvui.hardware.thinkpad.enable = true;

    ###################
    # Graphical System

    services.xserver = {

      enable = true;
      layout = "de";
      xkbVariant = "neo";
      xkbOptions = "altwin:swap_alt_win";
      serverFlagsSection = ''
        Option "StandbyTime" "10"
        Option "SuspendTime" "20"
        Option "OffTime" "30"
      '';

      # otherwise xterm is enabled, creating an xterm that spawns the window manager.
      desktopManager.xterm.enable = false;

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };

      displayManager = {
        sessionCommands = with pkgs; ''
            #TODO add as nixpkg
            export PATH+=":$HOME/scripts" #add utility scripts
            export EDITOR=emacsclient
            export TERMINAL=${lilyterm-git}/bin/lilyterm

            ${xorg.xset}/bin/xset r rate 250 35

            set-background &
            # TODO xbindkeys user service file
            ${lib.getBin xbindkeys}/bin/xbindkeys
            # synchronize clipboards
            ${lib.getBin autocutsel}/bin/autocutsel -s PRIMARY &
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

    fonts.fontconfig = {
      enable = true;
      defaultFonts = {
        monospace = [ "Source Code Pro" "DejaVu Sans Mono" ]; # TODO does not work
        sansSerif = [ "Liberation Sans" ];
      };
      ultimate = {
        enable = true;
        substitutions = "combi";
        preset = "ultimate4";
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
        silver-searcher   # file content searcher, > ack > grep
        dos2unix          # text file conversion
        manpages          # system manpages (not included by default)
        mkpasswd          # UNIX password creator
        ncdu              # disk size checker
        smartmontools     # check disk state
        stow              # dotfile management
        traceroute        # trace ip routes
        wirelesstools     # iwlist (wifi scan)
      ];
      # minimal set of gui applications
      guiPkgs = [
        lilyterm-git      # terminal emulator, best one around
        dmenu             # minimal launcher
      ];
    in basePkgs ++ guiPkgs;

    # friendly user shell
   programs.fish.enable = true;

    ###########
    # Services

    # services.openssh.enable = true;

    time.timeZone = "Europe/Paris";

    # bounded journal size
    services.journald.extraConfig = "SystemMaxUse=50M";

    vuizvui.programs.fish.fasd.enable = true;

    ########
    # Users

    users.users = { inherit philip; };

  };
}

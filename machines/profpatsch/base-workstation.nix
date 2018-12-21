# A base configuration that still assumes a workstation
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
      # TODO: kinda broken?
      # inputMethod = {
      #   enabled = "fcitx";
      #   fcitx.engines = with pkgs.fcitx-engines; [ mozc ];
      # };
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
    in basePkgs;

    # friendly user shell
   programs.fish.enable = true;

    ###########
    # Services

    services.openssh.enable = true;

    time.timeZone = "Europe/Paris";

    # bounded journal size
    services.journald.extraConfig = "SystemMaxUse=50M";

    services.xserver = {
      # otherwise xterm is enabled, creating an xterm that spawns the window manager.
      desktopManager.xterm.enable = false;

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };


    # services.xserver = {
      # libinput = {
      #   enable = true;
      #   naturalScrolling = true;
      #   accelSpeed = "0.01";
      # };

      # # TODO: modify libinput module so that you can do libinput."trackpoint".scrollMethod = "button";
      # # and maybe a convenience option for thinkpads (maybe in the hardware repo?).
      # config = ''
      #   Section "InputClass"
      #     Identifier     "Enable libinput for TrackPoint"
      #     MatchIsPointer "on"
      #     Driver         "libinput"
      #     Option         "ScrollMethod" "button"
      #     Option         "ScrollButton" "8"
      #   EndSection
      # '';
    # };

    ########
    # Users

    users.users = { inherit philip; };

  };
}

# A base configuration that still assumes a workstation
{ pkgs, lib, ... }:
let
  myPkgs = import ./pkgs.nix { inherit pkgs lib myLib; };
  myLib  = import ./lib.nix  { inherit pkgs lib; };

  philip = myLib.philip;

in {

  config = {

    boot.loader = {
      grub.enable = true;
      grub.version = 2;
    };

    networking = {
      wireless.enable = false;
      firewall.enable = false;
      networkmanager.enable = true;
    };

    i18n = {
      consoleFont = "lat9w-16";
      consoleKeyMap = "neo";
      defaultLocale = "en_US.UTF-8";
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
        curl              # transfer data to/from a URL
        dos2unix          # text file conversion
        file              # file information
        git               # version control system
        htop              # top replacement
        manpages          # system manpages (not included by default)
        mkpasswd          # UNIX password creator
        nmap              # stats about clients in the network
        rsync             # file syncing tool
        smartmontools     # check disk state
        stow              # dotfile management
        tmux              # detachable terminal multiplexer
        traceroute        # trace ip routes
        wget              # the other URL file fetcher
        wirelesstools     # iwlist (wifi scan)
        myPkgs.vim        # slight improvement over vi
      ];
    in basePkgs;

    # friendly user shell
    programs.fish.enable = true;

    ###########
    # Services

    services.openssh.enable = true;

    time.timeZone = "Europe/Berlin";

    # bounded journal size
    services.journald.extraConfig = "SystemMaxUse=50M";

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

    # Nobody wants mutable state. :)
    users.mutableUsers = false;
    users.users = { inherit philip; };

  };
}

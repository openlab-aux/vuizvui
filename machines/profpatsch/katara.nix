{ config, pkgs, lib, ... }:
{

  #########
  # Kernel
  
  boot.initrd.availableKernelModules = [ "uhci_hcd" "ehci_pci" "ahci" "firewire_ohci" ];
  boot.kernelModules = [ "kvm-intel" ];
  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";
  boot.initrd.luks.devices = [ { device = "/dev/sda2"; name = "cryptroot"; } ];


  ###########
  # Hardware
  
  # Use this if you want the T400 wifi to work …
  hardware.enableAllFirmware = true;

  hardware.trackpoint = {
    enable = true;
    emulateWheel = true;
    speed = 250;
    sensitivity = 140;
  };

  fileSystems."/" = {
    device = "/dev/dm-0";
    fsType = "btrfs";
  };

  fileSystems."/boot" = {
    device = "/dev/sda1";
    fsType = "ext3";
  };

  hardware.pulseaudio.enable = true;


  ######
  # Nix
  
  nix.maxJobs = 2;
  nix.binaryCaches = [ "https://hydra.nixos.org/" ];

  networking.hostName = "katara";
  networking.networkmanager.enable = true;

  networking.firewall = {
    enable = true;
    # Programmer’s dilemma
    allowedTCPPortRanges = [
      { from = 8000; to = 8005; }
      { from = 8080; to = 8085; }
    ];
    allowedUDPPorts = [ 60001 ];
  };

  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };


  ###########
  # Packages

  environment.systemPackages = with pkgs;
  let
    systemPkgs = [
      ack
      curl
      file
      fish
      git
      mkpasswd
      mosh
      nix-repl
      nmap
      stow
      tmux
      vim
      wget
      zsh
    ];
    xPkgs = [
      dmenu
      i3lock
      xbindkeys
      haskellPackages.xmobar
    ];
    guiPkgs = [
      gnome3.adwaita-icon-theme
      gnome3.gnome_themes_standard
      kde4.oxygen-icons
    ];
    userPrograms = [
      chromium
      emacs
      gajim
      keepassx
      lilyterm
    ];
    mailPkgs = [
      offlineimap
      mutt-kz
      msmtp
      notmuch
    ];
    haskellPkgs = with pkgs.haskellngPackages; [
      cabal2nix
    ];
  in systemPkgs ++ xPkgs ++ userPrograms ++ mailPkgs ++ haskellPkgs;


  ###########
  # Services

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  time.timeZone = "Europe/Berlin";

  # locate
  services.locate = {
    enable = true;
  };

  
  ###################
  # Graphical System
  
  # Enable the X11 windowing system.
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
    synaptics.enable = true;
    synaptics.minSpeed = "0.5";
    synaptics.accelFactor = "0.01";
    videoDrivers = [ "intel" "vesa" ];

    # otherwise xterm is enabled, creating an xterm that spawns the window manager.
    desktopManager.xterm.enable = false;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
    displayManager.sessionCommands =
      ''
      xset r rate 250 35
      '';

    startGnuPGAgent = true;
    
  };

  fonts.enableCoreFonts = true;
  fonts.fontconfig = {
    defaultFonts = {
      monospace = [ "Source Code Pro" "DejaVu Sans Mono" ];
      sansSerif = [ "Liberation Sans" ];
    };
    # use overkill infinality settings from old Arch installation
    ultimate = {
      rendering = {
        INFINALITY_FT_FILTER_PARAMS = "08 24 36 24 08";
        INFINALITY_FT_FRINGE_FILTER_STRENGTH = "25";
        INFINALITY_FT_USE_VARIOUS_TWEAKS = "true";
        INFINALITY_FT_WINDOWS_STYLE_SHARPENING_STRENGTH = "25";
        INFINALITY_FT_STEM_ALIGNMENT_STRENGTH = "15";
        INFINALITY_FT_STEM_FITTING_STRENGTH = "15";
      };
      # substitutions = "combi";
    };
  };
  
  # redshift
  services.redshift = {
    enable = true;
    latitude = "48";
    longitude = "10";
  };


  ########
  # Users

  # Nobody wants mutable state. :)
  users.mutableUsers = false;
  users.extraUsers = 
    let authKeys = ["ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDJhthfk38lzDvoI7lPqRneI0yBpZEhLDGRBpcXzpPSu+V0YlgrDix5fHhBl+EKfw4aeQNvQNuAky3pDtX+BDK1b7idbz9ZMCExy2a1kBKDVJz/onLSQxiiZMuHlAljVj9iU4uoTOxX3vB85Ok9aZtMP1rByRIWR9e81/km4HdfZTCjFVRLWfvo0s29H7l0fnbG9bb2E6kydlvjnXJnZFXX+KUM16X11lK53ilPdPJdm87VtxeSKZ7GOiBz6q7FHzEd2Zc3CnzgupQiXGSblXrlN22IY3IWfm5S/8RTeQbMLVoH0TncgCeenXH7FU/sXD79ypqQV/WaVVDYMOirsnh/ philip@nyx"];
    in {
      philip = rec {
	name = "philip";
	group = "users";
        extraGroups = [ "wheel" "networkmanager" ];
	uid = 1000;
	createHome = true;
	home = "/home/philip";
        passwordFile = "${home}/.config/passwd";
        # password = "test"; # in case of emergency, break glass
	shell = "/run/current-system/sw/bin/bash";
        openssh.authorizedKeys.keys = authKeys;
    };
  };

  ###########
  # Programs

  programs.ssh = {
    startAgent = false; # see services.xserver.startGnuPGAgent
    agentTimeout = "1h";
  };

  ########
  # Fixes
  
  # fix for emacs
  programs.bash.promptInit = "PS1=\"# \"";
}

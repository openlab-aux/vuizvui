# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let 
   mytexlive = with pkgs.texlive; combine { inherit scheme-medium minted units collection-bibtexextra; };
in {
  nixpkgs.config.allowUnfree = true;

  # hardware
  boot.blacklistedKernelModules = [ "nouveau" ];
  boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usb_storage" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.initrd.luks.devices = [ { device = "/dev/sda2"; name = "crypted"; } ];

  fileSystems."/" = {
    device = "/dev/dm-0";
    fsType = "btrfs";
  };
  fileSystems."/boot/" = {
    device = "/dev/sda1";
    fsType = "vfat";
  };

  swapDevices = [ ];
  nix.maxJobs = 8;

  boot.loader.gummiboot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  hardware.pulseaudio.enable = true;

  hardware.enableAllFirmware = true;

  # graphics
  hardware.bumblebee.enable = true;
  hardware.bumblebee.driver = "nvidia";

  hardware.trackpoint = {
    enable = true;
    emulateWheel = true;
    speed = 250;
    sensitivity = 140;
  };

  networking.hostName = "fliewatuet"; # Define your hostname.
  networking.networkmanager.enable = true;

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "de-latin1";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  environment.systemPackages = with pkgs; [
    ## tools
    rdiff-backup
    pass
    wget
    curl
    stow
    scrot
    dmenu
    mosh
    gnupg
    gpgme
    sudo
    silver-searcher
    imagemagick
    mkpasswd
    nmap
    traceroute
    file
    progress
    zip
    unzip
    atool
    manpages

    ## dev
    zsh
    git
    vim
    # c
    gnumake 
    clang
    gcc
    gnum4
    automake
    # haskell
    ghc
    cabal-install
    haskellPackages.cabal2nix
    haskellPackages.stylish-haskell
    # ocaml
    opam
    ocaml
    ocamlPackages.findlib
    # lisp
    clisp
    sbcl
    # go
    go

    ## applications
    tmux
    htop
    mutt
    tor
    torbrowser
    mupdf
    zathura
    pythonPackages.alot
    msmtp
    offlineimap
    notmuch
    # office
    libreoffice
    mytexlive

    ## GUI
    # wm etc.
    taffybar
    xbindkeys
    alock
    dunst
    libnotify
    redshift
    #gnome_themes_standard
    hicolor_icon_theme
    # applications
    lxappearance
    firefox
    termite
    feh
    pavucontrol
    cbatticon
    filezilla
    twister

    ## audio / video
    mpv
    abcde
    audacity
    beets
    lame
    ffmpeg

    ## services
    gutenprint  
    acpi

    ## libraries
    gmp

    ## games
    steam
    glxinfo
  ];

  # Proudly stolen from Profpatsch
  fonts.fontconfig = {
    defaultFonts = {
      monospace = [ "Inconsolata" "Source Code Pro" "DejaVu Sans Mono" ];
      sansSerif = [ "Liberation Sans" ];
    };
    ultimate = {
      rendering = {
        INFINALITY_FT_FILTER_PARAMS = "08 24 36 24 08";
        INFINALITY_FT_FRINGE_FILTER_STRENGTH = "25";
        INFINALITY_FT_USE_VARIOUS_TWEAKS = "true";
        INFINALITY_FT_WINDOWS_STYLE_SHARPENING_STRENGTH = "25";
        INFINALITY_FT_STEM_ALIGNMENT_STRENGTH = "15";
        INFINALITY_FT_STEM_FITTING_STRENGTH = "15";
      };
    };
  };
  fonts.fonts = with pkgs; [
    corefonts
    source-han-sans-japanese
    source-han-sans-korean
    source-han-sans-simplified-chinese
    source-code-pro
    dejavu_fonts
    ubuntu_font_family
    inconsolata
    tewi-font
  ];

  # to make Ctrl-Shift-t work in termite
  environment.etc."vte.sh" = { source = "${pkgs.gnome3.vte}/etc/profile.d/vte.sh"; };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # for taffybar
  services.upower.enable = true;

  # Enable CUPS to print documents.
  services.printing = {
    enable = true;
    drivers = [ pkgs.gutenprint ];
  };

  services.tlp.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "de";
    xkbVariant = "nodeadkeys";

    desktopManager.xterm.enable = false;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
    displayManager = {
      desktopManagerHandlesLidAndPower = false;
      sessionCommands =
        ''
        export BROWSER=firefox
        redshift -c .redshift &
        xbindkeys
        '';
    };

    synaptics.enable = true;
    synaptics.tapButtons = false;
    synaptics.twoFingerScroll = false;

    videoDrivers = [ "intel" ];

    startGnuPGAgent = true;
  };

  users.mutableUsers = false;
  users.extraUsers.lukas = {
    isNormalUser = true;
    uid = 1000;
    home = "/home/lukas";
    shell = "/run/current-system/sw/bin/zsh";
    group = "users";
    passwordFile = "/home/lukas/.config/passwd";
    extraGroups = [ "wheel" "networkmanager" ];
  };

  system.stateVersion = "unstable";

  programs.ssh.startAgent = false;
}

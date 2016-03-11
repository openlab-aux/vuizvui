{ config, lib, pkgs, ... }:

let
   mytexlive = with pkgs.texlive; combine { inherit scheme-medium minted units collection-bibtexextra ifplatform xstring doublestroke; };

in {
  nixpkgs.config.allowUnfree = true;
  
  boot.initrd.availableKernelModules = [ "uhci_hcd" "ehci_pci" "ata_piix" "usb_storage" "floppy" "usblp" "pcspkr" ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/98d6b322-25d9-4eff-a64a-684b3aad3734";
      fsType = "ext4";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/e5e7e8ad-af02-4b51-8a5b-f79f143c63da"; }
    ];

  nix.maxJobs = 1;
  networking.enableIntel2200BGFirmware = true;

  hardware.pulseaudio.enable = true;

  hardware.trackpoint = {
    enable = true;
    emulateWheel = true;
    speed = 250;
    sensitivity = 140;
  };

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  networking.hostName = "schnurrkadse";
  networking.networkmanager.enable = true;

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "de neo";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Berlin";

  environment.systemPackages = with pkgs; [
    unzip
    zip
    bzip2
    wget
    vim
    git
    stow
    acpi
    termite
    redshift
    networkmanagerapplet
    sudo
    mosh
    dmenu
    taffybar
    alock
    graphicsmagick
    silver-searcher
    pavucontrol
    hostapd

    # texlive, minted deps
    mytexlive
    which
    pythonPackages.pygments
    python

    w3m
    chromium
    mpv
    htop
    feh
    mupdf
    screen-message
    zathura
    youtube-dl
    pass

    ghc
    cabal-install
    haskellPackages.stylish-haskell
    haskellPackages.cabal2nix

    mutt
    notmuch
    offlineimap
    msmtp
    gnupg
    gpgme
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
    libertine
  ];

  services.openssh.enable = true;

  services.printing = {
    enable = true;
    drivers = [ pkgs.gutenprint pkgs.hplip ];
  };

  # for taffybar
  services.upower.enable = true;

  services.tlp.enable = true;

  services.syncthing.enable = true;
  services.syncthing.user = "lukas";

  services.xserver = {
    enable = true;
    layout = "de";
    xkbVariant = "neo";

    desktopManager.xterm.enable = false;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };

    displayManager = {
      sessionCommands =
        ''
        redshift -c .redshift &
        xsetroot -solid '#0fffb0'
        '';
    };

    synaptics.enable = true;
    synaptics.tapButtons = false;
    synaptics.twoFingerScroll = false;

    videoDrivers = [ "intel" ];

    startGnuPGAgent = true;
  };

  programs.fish.enable = true;

  users.users.lukas = {
    isNormalUser = true;
    uid = 1000;
    shell = "/run/current-system/sw/bin/fish";
    group = "users";
    extraGroups = [ "audio" "wheel" "networkmanager" ];
  };

  environment.etc."vte.sh" = { source = "${pkgs.gnome3.vte}/etc/profile.d/vte.sh"; };
  programs.ssh.startAgent = false;

  system.stateVersion = "unstable";
}

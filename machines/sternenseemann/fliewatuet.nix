{ config, pkgs, ... }:

let
   mytexlive = with pkgs.texlive; combine { inherit scheme-medium minted units collection-bibtexextra; };
in {
  nixpkgs.config.allowUnfree = true;

  # hardware
  boot.blacklistedKernelModules = [ "nouveau" "nvidia" ];
  boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usb_storage" ];
  boot.kernelModules = [ "kvm-intel" "virtio" ];
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
  nix.binaryCaches = [
    "https://headcounter.org/hydra/"
  ];
  nix.binaryCachePublicKeys = [
    "headcounter.org:/7YANMvnQnyvcVB6rgFTdb8p5LG1OTXaO+21CaOSBzg="
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.timeout = 5;
  boot.loader.efi.canTouchEfiVariables = true;

  # sound
  # fix sound
  boot.extraModprobeConfig = ''
  options snd-hda-intel index=1,0 enable_msi=1
  '';

  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;
    zeroconf.discovery.enable = true;
  };

  hardware.opengl.driSupport32Bit = true;
  hardware.bumblebee.enable = false;
  hardware.bumblebee.driver = "nvidia";

  hardware.enableAllFirmware = true;

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
    rdup
    pass
    wget
    curl
    stow
    scrot
    dmenu
    i3status
    mosh
    gnupg
    pinentry
    gpgme
    sudo
    silver-searcher
    graphicsmagick
    dcraw
    mkpasswd
    nmap
    traceroute
    file
    progress
    zip
    unzip
    atool
    manpages
    man_db
    #sshuttle
    speedtest-cli
    youtube-dl
    yafc
    psmisc
    telnet
    unison

    ## dev
    git
    vim
    neovim
    gnumake
    clang
    gcc
    gnum4
    automake
    valgrind
    ghc
    cabal-install
    haskellPackages.cabal2nix
    haskellPackages.stylish-haskell
    clisp
    go

    ## applications
    tmux
    htop
    mutt
    tor
    torbrowser
    mupdf
    zathura
    w3m
    pythonPackages.alot
    msmtp
    offlineimap
    notmuch
    irssi
    mytexlive

    ## GUI
    # wm etc.
    taffybar
    xbindkeys
    alock
    dunst
    libnotify
    redshift
    xorg.xbacklight
    xorg.xmodmap
    hicolor_icon_theme
    networkmanagerapplet
    xclip
    xsel
    # applications
    lxappearance
    firefox
    qutebrowser
    gstreamer
    termite
    feh
    gimp
    darktable
    rawtherapee
    pavucontrol
    cbatticon
    filezilla
    screen-message
    mumble
    libreoffice
    qemu
    xmpp-client
    cutegram
    ## GUI

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

  services.tor.enable = true;

  # Enable CUPS to print documents.
  services.printing = {
    enable = true;
    drivers = [ pkgs.gutenprint pkgs.hplip ];
  };

  services.tlp.enable = true;

  # Enable the X11 windowing system.
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
        export BROWSER=firefox
        redshift -c .redshift &
        xmodmap -e "pointer = 1 25 3 4 5 6 7 8 9"
        xbindkeys
        cbatticon &
        set-bg
        '';
    };

    synaptics.enable = true;
    synaptics.tapButtons = false;
    synaptics.twoFingerScroll = true;

    videoDrivers = [ "intel" ];
  };

  programs.fish.enable = true;

  users.mutableUsers = false;
  users.users.lukas = {
    isNormalUser = true;
    uid = 1000;
    home = "/home/lukas";
    shell = "/run/current-system/sw/bin/fish";
    group = "users";
    passwordFile = "/home/lukas/.config/passwd";
    extraGroups = [ "audio" "wheel" "networkmanager" "hugetlbfs"];
  };

  system.stateVersion = "unstable";

  programs.ssh.startAgent = false;
}

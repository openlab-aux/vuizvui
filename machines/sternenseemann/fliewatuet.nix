{ config, lib, pkgs, ... }:

let
  myPkgs = import ./pkgs.nix { inherit pkgs lib; };

in {
  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      bluez = pkgs.bluez5;
    };
  };

  # hardware
  boot.blacklistedKernelModules = [ "nouveau" "nvidia" ];
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
  nix.binaryCaches = [
    "https://headcounter.org/hydra/"
    "https://cache.nixos.org/"
  ];
  nix.binaryCachePublicKeys = [
    "headcounter.org:/7YANMvnQnyvcVB6rgFTdb8p5LG1OTXaO+21CaOSBzg="
  ];
  nix.useSandbox = true;
  nix.extraOptions = "gc-keep-derivations = false";

  boot.loader.systemd-boot.enable = true;
  boot.loader.timeout = 5;
  boot.loader.efi.canTouchEfiVariables = true;

  # limit journal size
  services.journald.extraConfig = "SystemMaxUse=100M";

  # sound
  # fix sound
  boot.extraModprobeConfig = ''
  options snd-hda-intel index=1,0 enable_msi=1
  '';

  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;
    package = pkgs.pulseaudioFull;
    zeroconf.discovery.enable = true;
    daemon.config.flat-volumes = "no";
  };

  hardware.bluetooth.enable = true;

  hardware.opengl.driSupport32Bit = true;
  hardware.enableAllFirmware = true;

  hardware.trackpoint = {
    enable = true;
    emulateWheel = true;
    speed = 250;
    sensitivity = 140;
  };

  networking.hostName = "fliewatuet";
  networking.firewall.enable = false;

  networking.supplicant = {
    wlp4s0 = {
      configFile.path = "/etc/wpa_supplicant.conf";
      userControlled.enable = true;
      userControlled.group = "users";
      driver = "wext";
      extraConf = ''
        ap_scan=1
      '';
    };
  };


  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "de-latin1";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Berlin";

  environment.systemPackages = with pkgs; [
    ## tools
    remind
    attic
    pass
    wget
    curl
    stow
    scrot
    dmenu
    mosh
    gnupg
    signing-party
    pinentry
    gpgme
    sudo
    silver-searcher
    graphicsmagick
    mkpasswd
    nmap
    file
    zip
    unzip
    atool
    manpages
    man_db
    sshuttle
    youtube-dl
    psmisc
    bar-xft
    unison
    ddate
    # aspell
    aspell
    aspellDicts.en
    aspellDicts.de

    ## dev
    git
    neovim
    ghc
    cabal-install
    haskellPackages.cabal2nix

    ## applications
    tmux
    htop
    mutt
    tor
    torbrowser
    zathura
    msmtp
    isync
    notmuch
    irssi
    myPkgs.texlive
    firefox
    chromium
    elinks
    termite
    imv
    gimp
    rawtherapee
    pavucontrol
    cbatticon
    filezilla
    screen-message
    jackline
    w3m

    ## GUI
    # wm etc.
    xdotool
    xbindkeys
    alock
    dunst
    libnotify
    xorg.xbacklight
    hicolor_icon_theme
    xsel

    ## audio / video
    myPkgs.mpv
    audacity
    lame
    ffmpeg
    beets

    ## services
    acpi

    ## games
    steam
  ];

  fonts.fontconfig = {
    defaultFonts = {
      monospace = [ "Inconsolata" ];
      sansSerif = [ "Open Sans" ];
      serif     = [ "Linux Libertine" ];
    };
    ultimate = {
      enable = true;
      substitutions = "combi";
    };
  };

  fonts.fonts = with pkgs; [
    corefonts
    opensans-ttf
    dejavu_fonts
    inconsolata
    tewi-font
    libertine
    google-fonts
    shrikhand # because not in google fonts :(
    xorg.fontbitstream100dpi
    xorg.fontbitstreamtype1
    freefont_ttf
    unifont
    unifont_upper
    poly
    junicode
  ];

  # to make Ctrl-Shift-t work in termite
  environment.etc."vte.sh" = { source = "${pkgs.gnome3.vte}/etc/profile.d/vte.sh"; };

  services.openssh.enable = true;

  services.tor = {
    enable = true;
    controlPort = 9051;
  };

  services.printing = {
    enable = true;
    drivers = [ pkgs.gutenprint ];
  };

  services.tlp.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "de";
    xkbVariant = "neo";

    desktopManager.xterm.enable = false;
    windowManager.herbstluftwm.enable = true;

    displayManager = {
      sessionCommands =
        ''
          ${pkgs.redshift}/bin/redshift -c .redshift &
          ${pkgs.xorg.xmodmap}/bin/xmodmap -e "pointer = 1 25 3 4 5 6 7 8 9"
          ${pkgs.xbindkeys}/bin/xbindkeys
          ${pkgs.cbatticon}/bin/cbatticon &
        '';
    };

    synaptics.enable = true;
    synaptics.tapButtons = false;
    synaptics.twoFingerScroll = false;

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
    extraGroups = [ "audio" "wheel" "networkmanager" ];
  };

  system.stateVersion = "unstable";

  programs.ssh.startAgent = false;
}

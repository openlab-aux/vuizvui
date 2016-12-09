{ config, lib, pkgs, ... }:

let
   mytexlive = with pkgs.texlive; combine { inherit scheme-medium minted units collection-bibtexextra wrapfig; };
   mympv = pkgs.mpv.override { scripts = [ pkgs.mpvScripts.convert ]; };
in {
  nixpkgs.config = {
    allowUnfree = true;
    packageOverides = pkgs: {
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
  ];
  nix.binaryCachePublicKeys = [
    "headcounter.org:/7YANMvnQnyvcVB6rgFTdb8p5LG1OTXaO+21CaOSBzg="
  ];
  nix.useSandbox = true;

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
    mytexlive
    (chromium.override { enablePepperFlash = true; })
    termite
    imv
    gimp
    rawtherapee
    pavucontrol
    cbatticon
    filezilla
    screen-message
    jackline

    ## GUI
    # wm etc.
    xbindkeys
    alock
    dunst
    libnotify
    xorg.xbacklight
    hicolor_icon_theme
    networkmanagerapplet
    xclip
    xsel

    ## audio / video
    mympv
    audacity
    lame
    ffmpeg

    ## services
    gutenprint
    acpi

    ## games
    steam
    sauerbraten
  ];

  fonts.fontconfig = {
    defaultFonts = {
      monospace = [ "Inconsolata" ];
      sansSerif = [ "Open Sans" ];
      serif     = [ "Linux Libertine" ];
    };
    ultimate = {
      preset = "ultimate1";
    };
  };

  fonts.fonts = with pkgs; [
    corefonts
    opensans-ttf
    dejavu_fonts
    inconsolata
    tewi-font
    libertine
    go-font
  ];

  # to make Ctrl-Shift-t work in termite
  environment.etc."vte.sh" = { source = "${pkgs.gnome3.vte}/etc/profile.d/vte.sh"; };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.tor = {
    enable = true;
    controlPort = 9051;
  };

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
    windowManager.herbstluftwm.enable = true;

    displayManager = {
      sessionCommands =
        ''
        redshift -c .redshift &
        xmodmap -e "pointer = 1 25 3 4 5 6 7 8 9"
        xbindkeys
        cbatticon &
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
    extraGroups = [ "audio" "wheel" "networkmanager" "hugetlbfs"];
  };

  system.stateVersion = "unstable";

  programs.ssh.startAgent = false;
}

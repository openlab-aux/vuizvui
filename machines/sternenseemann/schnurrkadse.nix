{ config, lib, pkgs, ... }:

let
   mytexlive = with pkgs.texlive; combine { inherit scheme-medium minted units collection-bibtexextra ifplatform xstring doublestroke csquotes; };
   urxvt = pkgs.rxvt_unicode-with-plugins.override { plugins = [ pkgs.urxvt_perls ]; };
in {
  nixpkgs.config.allowUnfree = true;
  nixpkgs.system = "i686-linux";

  boot.initrd.availableKernelModules = [ "uhci_hcd" "ehci_pci" "ata_piix" "usb_storage" "floppy" "usblp" "pcspkr" "btusb" ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  boot.initrd.luks.devices =
    [ { name = "schnurrkadse";
        device = "/dev/disk/by-uuid/544529b8-81cb-4e8e-9b6b-44f828ea2a7b";
        preLVM = true;
    } ];

  fileSystems."/" =
    { device = "/dev/mapper/schnurrkadse-root";
      fsType = "btrfs";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/e42bd75d-627d-4469-90cb-282dca7fdd4f";
      fsType = "ext4";
    };

  swapDevices =
    [ { device = "/dev/mapper/schnurrkadse-swap"; }
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
    consoleKeyMap = "de";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Berlin";

  environment.systemPackages = with pkgs; [
    unzip
    zip
    bzip2
    wget
    neovim
    git
    stow
    acpi
    urxvt
    xsel
    sudo
    mosh
    dmenu
    bar-xft
    alock
    silver-searcher
    pavucontrol
    unison

    mytexlive
    pythonPackages.pygments
    python

    w3m
    elinks
    torbrowser
    mpv
    htop
    imv
    screen-message
    zathura
    youtube-dl
    pass

    mutt
    notmuch
    msmtp
    isync
    gnupg
    gpgme
  ];

  fonts.fontconfig = {
    defaultFonts = {
      monospace = [ "Inconsolata" "Source Code Pro" "DejaVu Sans Mono" ];
      sansSerif = [ "DejaVu Sans" ];
      serif = [ "Vollkorn" ];
    };
  };
  fonts.fonts = with pkgs; [
    corefonts
    dejavu_fonts
    inconsolata
    libertine
    unifont
    google-fonts
  ];

  services.openssh.enable = true;

  services.tor.enable = true;
  services.tor.controlPort = 9051;

  services.printing = {
    enable = true;
    drivers = [ pkgs.gutenprint pkgs.hplip ];
  };

  services.tlp.enable = true;

  services.xserver = {
    enable = true;
    layout = "de";
    xkbVariant = "neo";

    desktopManager.xterm.enable = false;

    windowManager.herbstluftwm.enable = true;

    displayManager = {
      sessionCommands =
        ''
            ${urxvt}/bin/urxvtd -q -f -o
        '';
    };

    synaptics.enable = true;
    synaptics.tapButtons = false;
    synaptics.twoFingerScroll = false;

    videoDrivers = [ "intel" ];
  };

  programs.fish.enable = true;

  users.users.lukas = {
    isNormalUser = true;
    uid = 1000;
    shell = "${pkgs.fish}/bin/fish";
    group = "users";
    extraGroups = [ "audio" "wheel" "networkmanager" "uucp" ];
  };

  environment.etc."vte.sh" = { source = "${pkgs.gnome3.vte}/etc/profile.d/vte.sh"; };
  programs.ssh.startAgent = false;

  system.stateVersion = "unstable";
}

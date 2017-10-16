{ config, lib, pkgs, ... }:

let
  myPkgs = import ./pkgs.nix { inherit pkgs lib; };

in {
  nixpkgs.config.allowUnfree = true;
  nixpkgs.system = "i686-linux";

  boot.initrd.availableKernelModules = [ "uhci_hcd" "ehci_pci" "ata_piix" "usb_storage" "floppy" "usblp" "pcspkr" "btusb" ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  boot.initrd.luks.devices =
    [ { name = "schnurrkadse";
        device = "/dev/disk/by-uuid/544529b8-81cb-4e8e-9b6b-44f828ea2a7b";
        preLVM = true; } ];

  fileSystems."/" =
    { device = "/dev/mapper/schnurrkadse-root";
      fsType = "btrfs"; };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/e42bd75d-627d-4469-90cb-282dca7fdd4f";
      fsType = "ext4"; };

  swapDevices = [ { device = "/dev/mapper/schnurrkadse-swap"; } ];

  nix.maxJobs = 1;

  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;
  hardware.pulseaudio.zeroconf.discovery.enable = true;

  hardware.enableRedistributableFirmware = true;

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
  networking.supplicant = {
    wlp4s2 = {
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
    myPkgs.urxvt
    xsel
    sudo
    mosh
    dmenu
    bar-xft
    alock
    silver-searcher
    pavucontrol
    unison

    myPkgs.texlive
    pythonPackages.pygments
    python

    elinks
    torbrowser
    chromium
    myPkgs.mpv
    htop
    imv
    screen-message
    zathura
    youtube-dl
    pass
    aspell
    aspellDicts.de
    aspellDicts.en

    mutt
    notmuch
    msmtp
    isync
    gnupg
    gpgme
    w3m
  ];

  fonts.fontconfig = {
    defaultFonts = {
      monospace = [ "Inconsolata" "Source Code Pro" "DejaVu Sans Mono" ];
      sansSerif = [ "DejaVu Sans" ];
      serif = [ "Vollkorn" ];
    };
    ultimate.enable = true;
    ultimate.substitutions = "combi";
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
            ${myPkgs.urxvt}/bin/urxvtd -q -f -o
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

  programs.ssh.startAgent = false;

  system.stateVersion = "unstable";
}

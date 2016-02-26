{ config, lib, pkgs, ... }:

{

  boot.initrd.availableKernelModules = [ "uhci_hcd" "ehci_pci" "ata_piix" "usb_storage" "floppy" ];
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
  hardware.firmware = let myfirmware = pkgs.firmwareLinuxNonfree.overrideDerivation
    (old: {
      name = "myfirmware";
      src = pkgs.fetchFromGitHub {
        owner = "wkennington";
        repo = "linux-firmware";
        rev = "2016-01-26";
        sha256="07hv4kgbsxndhm1va6k6scy083886aap3naq1l4jdz7dnph4ir02";
      };
    }); in [ myfirmware ];

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
    consoleKeyMap = "de-latin1";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Berlin";

  environment.systemPackages = with pkgs; [
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

    chromium
    mpv
    htop
  ];

  services.openssh.enable = true;
  services.printing = {
    enable = true;
    drivers = [ pkgs.gutenprint pkgs.hplip ];
  };

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
      desktopManagerHandlesLidAndPower = false;
      sessionCommands =
        ''
        redshift -c .redshift &
        '';
    };

    synaptics.enable = true;
    synaptics.tapButtons = false;
    synaptics.twoFingerScroll = true;

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

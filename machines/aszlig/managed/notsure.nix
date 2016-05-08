{ pkgs, lib, ... }:

let
  rootUUID = "ata-WDC_WD7500BPVT-22HXZT3_WD-WX71A9124879";

in {
  boot = {
    initrd.availableKernelModules = [ "ehci_pci" "ahci" ];
    kernelPackages = pkgs.linuxPackages_latest;
    loader.grub.enable = true;
    loader.grub.version = 2;
    loader.grub.device = "/dev/disk/by-id/${rootUUID}";
  };

  fileSystems."/" = {
    label = "root";
    fsType = "btrfs";
    options = [ "space_cache" "relatime" ];
  };

  hardware = {
    enableAllFirmware = true;
    opengl.s3tcSupport = true;
    pulseaudio.enable = true;
  };

  networking = {
    firewall.enable = false;
    hostName = "notsure";
    networkmanager.enable = true;
  };

  nix.maxJobs = 2;

  nixpkgs.config = {
    allowUnfree = true;
    chromium.enablePepperFlash = true;
    pulseaudio = true;
  };

  environment.systemPackages = with pkgs; [
    chromium
    file
    gajim
    gimp
    git
    htop
    kde5.gwenview
    libreoffice
    miro
    mpv
    opentyrian
    pavucontrol
    pulseaudioFull
    samba
    skype
    thunderbird
    vuizvui.tomahawk
    wine
    xpdf
    youtubeDL
  ];

  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "de";
    defaultLocale = "en_US.UTF-8";
  };

  services = {
    deluge.enable = true;
    printing.enable = true;
    printing.drivers = [ pkgs.gutenprint pkgs.hplip ];
    tlp.enable = true;

    xserver = {
      enable = true;
      layout = "de";
      xkbOptions = "eurosign:e";

      displayManager.kdm.enable = true;
      desktopManager.kde5.enable = true;
    };
  };

  swapDevices = lib.singleton { label = "swap"; };

  time.timeZone = "Europe/Berlin";

  vuizvui.user.aszlig.programs.vim.enable = true;
  vuizvui.enableGlobalNixpkgsConfig = true;
}

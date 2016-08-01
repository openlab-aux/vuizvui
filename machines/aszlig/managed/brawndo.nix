{ config, pkgs, unfreeAndNonDistributablePkgs, lib, ... }:

let
  mainDisk = "ata-WDC_WD5000LPVX-22V0TT0_WD-WXG1E2559AYH";
  rootUUID = "dbbd5a35-3ac0-4d5a-837d-914457de14a4";

in {
  boot = {
    initrd.availableKernelModules = [
      "xhci_pci" "ehci_pci" "ahci" "usb_storage" "sd_mod" "sr_mod"
      "rtsx_pci_sdmmc"
    ];
    kernelModules = [ "kvm-intel" "wl" ];
    kernelPackages = pkgs.linuxPackages_latest;
    extraModulePackages = [ config.boot.kernelPackages.broadcom_sta ];
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/${rootUUID}";
    fsType = "btrfs";
    options = [ "compress=zlib" "space_cache" "noatime" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/534F-980B";
    fsType = "vfat";
  };

  hardware = {
    cpu.intel.updateMicrocode = true;
    enableAllFirmware = true;
    opengl.s3tcSupport = true;
    pulseaudio.enable = true;
  };

  networking = {
    firewall.enable = false;
    hostName = "brawndo";
    networkmanager.enable = true;
  };

  nix.maxJobs = 4;

  nixpkgs.config = {
    allowUnfree = true; # XXX: More granularity!
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
    kde5.okular
    libreoffice
    miro
    mpv
    opentyrian
    pavucontrol
    pulseaudioFull
    samba
    unfreeAndNonDistributablePkgs.skype
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
    printing.drivers = [ pkgs.gutenprint pkgs.hplip pkgs.cups-bjnp ];
    tlp.enable = true;

    xserver = {
      enable = true;
      layout = "de";
      xkbOptions = "eurosign:e";

      synaptics.enable = true;
      synaptics.twoFingerScroll = true;

      displayManager.kdm.enable = true;
      desktopManager.kde5.enable = true;
    };
  };

  swapDevices = lib.singleton { label = "swap"; };

  time.timeZone = "Europe/Berlin";

  users.users.dwenola = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "video" "wheel" "networkmanager" ];
  };

  vuizvui.user.aszlig.programs.vim.enable = true;
  vuizvui.enableGlobalNixpkgsConfig = true;
}

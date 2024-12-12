{ config, pkgs, lib, ... }:

let
  browser = rec {
    pkg = pkgs.firefox-wayland;
    bin = "${pkg}/bin/firefox";
  };

in {
  imports = [
    ((import ../../nixos-hardware-path.nix) + "/lenovo/thinkpad/x270")
    ./base-laptop.nix
    ./wireguard.nix
    ./user-lukas.nix
  ] ++ lib.optionals (builtins.pathExists ./local.nix) [
    ./local.nix
  ];

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "nvme"
    "usb_storage"
    "sd_mod"
    "rtsx_pci_sdmmc"
  ];
  boot.kernelModules = [
    "kvm-intel"
    "snd-seq"
    "snd-rawmidi"
  ];

  fileSystems."/" = {
    device = "/dev/mapper/main";
    fsType = "btrfs";
    options = [ "compress=zstd" ];
  };

  fileSystems."/boot" = {
    device = "/dev/nvme0n1p1";
    fsType = "vfat";
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/49f15787-a773-448f-bc9e-452104a80fb5"; }
  ];

  nix.settings.max-jobs = 4;

  boot.initrd.luks.devices = {
    "main".device = "/dev/nvme0n1p2";
    "swap".device = "/dev/nvme0n1p3";
  };

  vuizvui.hardware.thinkpad = {
    enable = true;
    powerManagement = "auto-cpufreq";
  };
  hardware.graphics.extraPackages = [
    pkgs.intel-media-sdk # KBL GT2 gpu
  ];

  boot.loader.systemd-boot = {
    enable = true;
    memtest86.enable = true;
  };
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "wolfgang";

  vuizvui.user.sternenseemann.profiles.desktop.enable = true;

  environment.systemPackages = with pkgs; [
    vuizvui.sternenseemann.pass
    vuizvui.sternenseemann.plan9port
    vuizvui.sternenseemann.scripts.borg-wrapper
    exfat borgbackup
    gnupg pinentry-gnome3 signing-party gpgme
    thunderbird
    vuizvui.sternenseemann.texlive
    # jabref # depends on insecure JDK version
    yt-dlp mpv vlc spotify
    ghc cabal-install cabal2nix
    sbcl rlwrap
    cbqn ngn-k
    valgrind gdb
    scribus gimp inkscape libreoffice
    audacity
    signal-desktop discord
    vuizvui.profpatsch.nman
    vuizvui.sternenseemann.nix-env-diff
    vuizvui.sternenseemann.tep.wayland
    xdg-utils                  # xdg-open etc.
    networkmanagerapplet       # for nm-connection-ediotr
    imv zathura
    pcmanfm
    browser.pkg
    gpxsee
    msr-tools
    quasselClient
    anki
  ];

  environment.variables = {
    BROWSER = browser.bin;
  };

  services.xserver = {
    videoDrivers = [ "intel" ];
  };

  vuizvui.profiles.tvl = {
    enable = true;
  };

  system.stateVersion = "unstable";
}

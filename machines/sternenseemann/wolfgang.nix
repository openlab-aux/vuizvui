{ config, pkgs, lib, ... }:

let
  browser = rec {
    pkg = pkgs.firefox-wayland;
    bin = "${pkg}/bin/firefox";
  };

in {
  imports = [
    ./base-laptop.nix
    ./desktop-sway.nix
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

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "wolfgang";

  # useful for debugging native compilation of packages on another platform
  boot.binfmt.emulatedSystems = [ /* "aarch64-linux" */ ];

  environment.systemPackages = with pkgs; [
    vuizvui.sternenseemann.pass
    vuizvui.sternenseemann.acme
    exfat borgbackup
    gnupg pinentry-gnome signing-party gpgme
    thunderbird-wayland
    jackline gajim fractal
    vuizvui.sternenseemann.texlive jabref
    yt-dlp mpv spotify
    ghc cabal-install cabal2nix
    sbcl rlwrap
    valgrind gdb
    docker-compose
    scribus gimp inkscape libreoffice
    audacity
    signal-desktop discord
    vuizvui.profpatsch.nman
    vuizvui.sternenseemann.nix-env-diff
    vuizvui.sternenseemann.tep
    xdg-utils                  # xdg-open etc.
    networkmanagerapplet       # for nm-connection-ediotr
    imv zathura
    gnome.nautilus
    gnome.gedit
    browser.pkg
    castor
    nix-output-monitor
    vuizvui.tvl.users.sterni.clhs-lookup
    hunspell
  ] ++ (with hunspellDicts; [ de-de en-gb-large en-us ]);

  environment.variables = {
    BROWSER = browser.bin;
  };

  services.lorri.enable = true;

  services.tor = {
    enable = true;

    torsocks = {
      enable = true;
    };

    client = {
      enable = true;
    };
  };

  services.xserver = {
    videoDrivers = [ "intel" ];
  };

  vuizvui.profiles.tvl = {
    enable = true;
  };

  system.stateVersion = "unstable";
}

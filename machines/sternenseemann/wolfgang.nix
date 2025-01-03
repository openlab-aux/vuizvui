{ config, pkgs, lib, ... }:

let
  browser = rec {
    pkg = pkgs.firefox-wayland;
    bin = "${pkg}/bin/firefox";
  };

in {
  imports = builtins.map (p: (import ../../nixos-hardware-path.nix) + p) [
    "/common/pc/laptop/ssd"
  ] ++ [
    ./base-laptop.nix
    ./wireguard.nix
    ./user-lukas.nix
  ] ++ lib.optionals (builtins.pathExists ./local.nix) [
    ./local.nix
  ];

  boot.kernelParams = [
    # Try to work around random freezes
    "i915.enable_psr=0"
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
    vuizvui.sternenseemann.scripts.disable-bd-prochot
    vuizvui.sternenseemann.scripts.fdate
    exfat borgbackup
    gnupg pinentry-gnome3 # signing-party
    thunderbird
    vuizvui.sternenseemann.texlive
    yt-dlp mpv
    ghc cabal-install cabal2nix
    rlwrap
    cbqn ngn-k
    valgrind gdb
    libreoffice
    vuizvui.profpatsch.nman
    vuizvui.sternenseemann.nix-env-diff
    vuizvui.sternenseemann.tep
    xdg-utils                  # TODO(sterni): fix xdg stuff
    networkmanagerapplet       # for nm-connection-ediotr
    imv                        # TODO(sterni): is slow and clunky
    zathura                    # TODO(sterni): annotations would be nice
    pcmanfm
    browser.pkg
    quasselClient
    ricochet-refresh
    anki
    signal-desktop # no signal web client
    # scribus gimp inkscape audacity # rarely need them
    # gpxsee # rarely need it nowadays
  ];

  services.tor.enable = true;

  environment.variables = {
    BROWSER = browser.bin;
  };

  services.xserver = {
    videoDrivers = [ "modesetting" ];
  };

  vuizvui.profiles.tvl = {
    enable = true;
  };

  system.stateVersion = "unstable";
}

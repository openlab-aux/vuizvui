{ config, pkgs, lib, ... }:

let
  browser = rec {
    pkg = pkgs.firefox-wayland;
    bin = "${pkg}/bin/firefox";
  };

in {
  imports = builtins.map (p: (import ../../nixos-hardware-path.nix) + p) [
    "/common/pc/ssd"
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
    # TODO(sterni): is there an alternative for my GPU?
    # EOL / multiple CVEs: pkgs.intel-media-sdk # KBL GT2 gpu
  ];

  # Manage Receiver of Logitech M570 with ltunify
  hardware.logitech.wireless.enable = true;

  boot.loader.systemd-boot = {
    enable = true;
    memtest86.enable = true;
  };
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "wolfgang";

  vuizvui.user.sternenseemann.profiles = {
    desktop.enable = true;

    editors = {
      enable = true;
      editor = "emacs";
      acme.enable = true;
      emacs.enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
    vuizvui.sternenseemann.pass
    vuizvui.sternenseemann.scripts.borg-wrapper
    vuizvui.sternenseemann.scripts.disable-bd-prochot
    vuizvui.sternenseemann.scripts.idate
    vuizvui.sternenseemann.scripts.fdate
    exfat borgbackup
    gnupg pinentry-gnome3 # signing-party
    thunderbird
    vuizvui.sternenseemann.texlive
    yt-dlp mpv
    ghc cabal-install cabal2nix
    rlwrap
    watchexec
    jq
    cbqn ngn-k
    gdb
    libreoffice
    vuizvui.profpatsch.nman
    vuizvui.sternenseemann.nix-env-diff
    nix-derivation
    treefmt nixfmt
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

  # Needs https://github.com/NixOS/nixpkgs/pull/378620
  environment.etc.zathurarc.text = ''
    set database sqlite
    set selection-clipboard clipboard
    # double g is unnecessary
    map g goto top
  '';

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

  system.stateVersion = "25.05";
}

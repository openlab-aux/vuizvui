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
    { device = "/dev/disk/by-uuid/198329ed-5038-4ad8-b8a6-e52921f2673d"; }
  ];

  nix.maxJobs = 4;

  boot.initrd.luks.devices = {
    "main".device = "/dev/nvme0n1p2";
    "swap".device = "/dev/nvme0n1p3";
  };

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking = {
    hostName = "wolfgang";
    firewall = {
      enable = true;
      allowedTCPPortRanges = [
        { from = 9990; to = 9999; }
      ];
    };
    # nat networking for virtual machines / containers
    # TODO(sterni): remove when I don't have to deal
    #               with such stuff @ work anymore
    nat = {
      enable = true;
      internalInterfaces = [ "ve-+" ];
      externalInterface = "wlp3s0";
    };
    networkmanager = {
      enable = true;
      unmanaged = [ "interface-name:ve-*" ];
    };
  };


  virtualisation.docker.enable = true;

  time.timeZone = "Europe/Berlin";

  environment.systemPackages = with pkgs; [
    vuizvui.sternenseemann.pass
    exfat borgbackup
    gnupg pinentry-gtk2 signing-party gpgme
    thunderbird
    jackline
    vuizvui.sternenseemann.texlive jabref
    youtube-dl mpv spotify
    newsboat
    ghc cabal-install cabal2nix
    sbcl rlwrap
    valgrind gdb
    github-cli
    scribus gimp inkscape libreoffice
    audacity
    signal-desktop discord
    multimc
    vuizvui.profpatsch.nman
    vuizvui.sternenseemann.nix-env-diff
    vuizvui.sternenseemann.tep
    vuizvui.sternenseemann.t
    xdg_utils                  # xdg-open etc.
    networkmanagerapplet       # for nm-connection-ediotr
    imv zathura
    gnome3.nautilus
    browser.pkg
    nix-output-monitor
    # TODO(sterni) depot.users.sterni.clhs-lookup
    hunspell
  ] ++ (with hunspellDicts; [ de-de en-gb-large en-us ]);

  environment.variables = {
    BROWSER = browser.bin;
  };

  services.tor = {
    enable = true;

    torsocks = {
      enable = true;
    };

    client = {
      enable = true;
    };
  };

  services.printing = {
    enable = true;
    drivers = [ pkgs.gutenprint pkgs.hplip ];
  };

  services.xserver = {
    videoDrivers = [ "intel" ];
  };

  vuizvui.profiles.tvl = {
    enable = true;
  };

  nix.trustedUsers = [ "lukas" ];
  users.users.lukas = {
    isNormalUser = true;
    uid = 1000;
    home = "/home/lukas";
    group = "users";
    extraGroups = [ "wheel" "networkmanager" "audio" "docker" ];
    shell = "${pkgs.fish}/bin/fish";
  };

  system.stateVersion = "unstable";
}

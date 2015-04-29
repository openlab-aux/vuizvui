{ pkgs, lib, ... }:

let
  greybird = pkgs.stdenv.mkDerivation {
    name = "greybird-xfce-theme";

    src = pkgs.fetchFromGitHub {
      repo = "Greybird";
      owner = "shimmerproject";
      rev = "61ec18d22780aa87998381599c941e0cf4f7bfb5";
      sha256 = "03h8hba4lfp337a4drylcplrbggry9gz8dq1f3gjy25fhqkgvq05";
    };

    phases = [ "unpackPhase" "installPhase" ];

    installPhase = ''
      mkdir -p "$out/share/themes/Greybird" \
               "$out/share/themes/Greybird-compact/xfwm4"
      cp -vrt "$out/share/themes/Greybird" \
        gtk-* metacity-1 unity xfce-notify-4.0 xfwm4
      cp -vrt "$out/share/themes/Greybird-compact/xfwm4" \
        xfwm4_compact/*
    '';
  };

  modulesPath = "${import ../../nixpkgs-path.nix}/nixos/modules";

in {
  imports = [ "${modulesPath}/installer/scan/not-detected.nix" ];

  boot.loader.grub.device = "/dev/disk/by-id/ata-HITACHI_HTS722010K9SA00_080711DP0270DPGLVMPC";

  boot.kernelModules = [ "kvm-intel" ];
  boot.initrd.availableKernelModules = [
    "uhci_hcd" "ehci_pci" "ata_piix" "firewire_ohci" "usb_storage"
  ];

  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "us";
    defaultLocale = "de_DE.UTF-8";
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/754fd3e3-2e04-4028-9363-0c6bb4c54367";
    fsType = "ext4";
  };

  environment.systemPackages = with pkgs; [
    greybird
    #repetierhost <- TODO
    firefox
    fish
    gimp
    git
    freecad
    openscad
    printrun
    blender
    slic3r
    tmux
    libreoffice
    inkscape
    filezilla
    gmpc
    vlc
    vim
    wget
  ];

  services.xserver = {
    enable = true;
    layout = "us";
    xkbOptions = "eurosign:e";

    displayManager.auto.enable = true;
    displayManager.auto.user = "openlab";
    desktopManager.xfce.enable = true;
 
    synaptics.enable = true;
    synaptics.minSpeed = "0.5";
    synaptics.accelFactor = "0.01";
  };


  hardware.trackpoint = {
    enable = true;
    emulateWheel = true;
    sensitivity = 130;
    speed = 350;
  };


  services.openssh.enable = true;

  networking.networkmanager.enable = true;
  networking.enableIntel3945ABGFirmware = true;

  nix.maxJobs = 2;

  users.mutableUsers = false;
  users.extraUsers.openlab = {
    uid = 1000;
    isNormalUser = true;
    password = "openlab";
    extraGroups = [ "wheel" "networkmanager" "dialout"];
    openssh.authorizedKeys.keys = lib.singleton (lib.concatStrings [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDJhthfk38lzDvoI7lPqRneI0yBpZEhLD"
      "GRBpcXzpPSu+V0YlgrDix5fHhBl+EKfw4aeQNvQNuAky3pDtX+BDK1b7idbz9ZMCExy2a1"
      "kBKDVJz/onLSQxiiZMuHlAljVj9iU4uoTOxX3vB85Ok9aZtMP1rByRIWR9e81/km4HdfZT"
      "CjFVRLWfvo0s29H7l0fnbG9bb2E6kydlvjnXJnZFXX+KUM16X11lK53ilPdPJdm87VtxeS"
      "KZ7GOiBz6q7FHzEd2Zc3CnzgupQiXGSblXrlN22IY3IWfm5S/8RTeQbMLVoH0TncgCeenX"
      "H7FU/sXD79ypqQV/WaVVDYMOirsnh/ philip@nyx"
    ]);
  };

  # fix for emacs
  programs.bash.promptInit = "PS=\"# \"";
}

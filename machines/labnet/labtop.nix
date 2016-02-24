{ pkgs, lib, ... }:

let
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

  vuizvui.hardware.thinkpad.enable = true;

  hardware.trackpoint.enable = false;

  environment.systemPackages = with pkgs; [
    #repetierhost <- TODO
    ack
    antimony
    blender
    filezilla
    firefox
    fish
    freecad
    gcc
    gnome3.gedit
    gimp
    git
    gmpc
    inkscape
    ino
    (libreoffice.overrideDerivation (lib.const { doCheck = false; }))
    netcat-openbsd
    openscad
    printrun
    python3
    screen
    slic3r
    tmux
    vim
    vlc
    wget
  ];

  services.xserver = {
    enable = true;
    layout = "us";
    xkbOptions = "eurosign:e";

    displayManager.auto.enable = true;
    displayManager.auto.user = "openlab";
    desktopManager.gnome3.enable = true;
    synaptics.enable = true;
    synaptics.minSpeed = "0.5";
    synaptics.accelFactor = "0.01";
  };

  services.openssh.enable = true;

  networking.networkmanager.enable = true;
  networking.enableIntel3945ABGFirmware = true;
  networking.hostName = "labtop";
  networking.firewall = {
    allowedTCPPorts = [ 1337 2342 ];
    allowedTCPPortRanges = [ { from = 8000; to = 8005; } ];
    allowPing = true;
  };

  nix.maxJobs = 2;

  users.mutableUsers = false;
  users.users.openlab = {
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

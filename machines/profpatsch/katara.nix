{ config, pkgs, ... }:

{
  # Kernel
  boot.initrd.availableKernelModules = [ "uhci_hcd" "ehci_pci" "ahci" "firewire_ohci" ];
  boot.kernelModules = [ "kvm-intel" ];
  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";
  boot.initrd.luks.devices = [ { device = "/dev/sda2"; name = "cryptroot"; } ];

  hardware.trackpoint = {
    enable = true;
    emulateWheel = true;
    speed = 250;
    sensitivity = 140;
  };

  fileSystems."/" = {
    device = "/dev/dm-0";
    fsType = "btrfs";
  };

  fileSystems."/boot" = {
    device = "/dev/sda1";
    fsType = "ext3";
  };

  nix.maxJobs = 2;

  networking.hostName = "katara";
  networking.networkmanager.enable = true;

  networking.firewall.enable = true;
  # Programmer’s dilemma
  networking.firewall.allowedTCPPortRanges = [
    { from = 8000; to = 8005; }
    { from = 8080; to = 8085; }
  ];

  # Select internationalisation properties.
  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # List packages installed in system profile. To search by name, run:
  # -env -qaP | grep wget
  environment.systemPackages =
  let
    haskellPkgs = with pkgs.haskellngPackages; [
      ghc
      cabal-install
      cabal2nix
    ];
  in with pkgs; [
    ack
    curl
    emacs
    nix-repl
    fish
    mkpasswd
    git
    tmux
    vim
    wget
    zsh
  ] ++ haskellPkgs;


  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "de";
  services.xserver.xkbOptions = "eurosign:e";

  # services.xserver.windowManager.xmonad.enable = true;
  services.xserver.desktopManager.gnome3.enable = true;

  # redshift
  services.redshift = {
    enable = true;
    latitude = "48";
    longitude = "10";
  };

  time.timeZone = "Europe/Berlin";

  # locate
  services.locate = {
    enable = true;
    extraFlags = ["--add-prunepaths /nix/store"];
  };

  # Nobody wants mutable state. :)
  users.mutableUsers = false;
  users.extraUsers = 
    let authKeys = ["ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDJhthfk38lzDvoI7lPqRneI0yBpZEhLDGRBpcXzpPSu+V0YlgrDix5fHhBl+EKfw4aeQNvQNuAky3pDtX+BDK1b7idbz9ZMCExy2a1kBKDVJz/onLSQxiiZMuHlAljVj9iU4uoTOxX3vB85Ok9aZtMP1rByRIWR9e81/km4HdfZTCjFVRLWfvo0s29H7l0fnbG9bb2E6kydlvjnXJnZFXX+KUM16X11lK53ilPdPJdm87VtxeSKZ7GOiBz6q7FHzEd2Zc3CnzgupQiXGSblXrlN22IY3IWfm5S/8RTeQbMLVoH0TncgCeenXH7FU/sXD79ypqQV/WaVVDYMOirsnh/ philip@nyx"];
    in {
      philip = rec {
	name = "philip";
	group = "users";
        extraGroups = [ "wheel" "networkmanager" ];
	uid = 1000;
	createHome = true;
	home = "/home/philip";
        passwordFile = "${home}/.config/passwd";
        # password = "test"; # in case of emergency, break glass
	shell = "/run/current-system/sw/bin/bash";
        openssh.authorizedKeys.keys = authKeys;
    };
  };

  # fix for emacs
  programs.bash.promptInit = "PS1=\"# \"";
}

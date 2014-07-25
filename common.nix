{ config, pkgs, ... }:

{
  nix = {
    package = pkgs.nixUnstable;
    useChroot = true;
    readOnlyStore = true;
    extraOptions = ''
      build-cores = 0
    '';
  };

  boot.loader.grub = {
    enable = true;
    version = 2;
  };

  hardware.cpu.intel.updateMicrocode = true;

  users.defaultUserShell = "/var/run/current-system/sw/bin/zsh";

  networking.wireless.enable = false;
  networking.firewall.enable = false;

  i18n.consoleKeyMap = "dvorak";
  programs.ssh.startAgent = false;

  services.nixosManual.showManual = false;

  environment.systemPackages = with pkgs; [
    binutils
    cacert
    file
    htop
    iotop
    psmisc
    unrar
    unzip
    vim_configurable
    vlock
    wget
    xz
    zsh
  ];

  nixpkgs.config = import ./nixpkgs/config.nix;
  system.fsPackages = with pkgs; [ sshfsFuse ];
  time.timeZone = "Europe/Berlin";
}

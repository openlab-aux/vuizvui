{ config, pkgs, lib, ... }:
{
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;

  services.nixosManual.enable = false;

  nix.binaryCaches = [ 
    "https://headcounter.org/hydra/"
    "http://nixos-arm.dezgeg.me/channel"
  ];

  nix.binaryCachePublicKeys = [
    "headcounter.org:/7YANMvnQnyvcVB6rgFTdb8p5LG1OTXaO+21CaOSBzg="
    "nixos-arm.dezgeg.me-1:xBaUKS3n17BZPKeyxL4JfbTqECsT+ysbDJz29kLFRW0=%"
  ];

  nix.maxJobs = 3;
  nix.extraOptions = ''
    gc-keep-derivations = false
  '';

  nixpkgs.system = "armv7l-linux";
  hardware.opengl.enable = false;
  powerManagement.enable = false;

  networking.hostName = "schaf";

  time.timeZone = "Europe/Berlin";


  fileSystems = {
    "/boot" = {
      device = "/dev/disk/by-label/NIXOS_BOOT";
      fsType = "vfat";
    };
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
    };
    "/home" = {
      device = "/dev/disk/by-label/SCHAF_HOME";
      fsType = "ext4";
    };
    "/nix" = {
      device = "/dev/disk/by-label/NIX_SCHAF";
      fsType = "ext4";
    };
  };

  swapDevices = [ { device = "/swapfile"; size = 1024; } ];

  services.openssh.enable = true;
  services.openssh.permitRootLogin = "no";
  networking.firewall.enable = false;

  services.journald.extraConfig = "SystemMaxUse=10M";

  environment.systemPackages = with pkgs; [
    (unison.override { enableX11 = false; })
    vim
    sudo
    git
    dtach
  ];

  services.tor.enable = true;
  services.tor.extraConfig = ''
    HiddenServiceDir /var/lib/tor/hs
    HiddenServicePort 22 127.0.0.1:22
  '';

  users.extraUsers.lukas = {
    isNormalUser = true;
    uid = 1000;
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDdsggyI+fin8c9FySJ1QtiwrExUD6UW5HCNQ5gniz1iUPkdxK9Xb1GcVIbQ2l7h9W7hz5w5bksPa6/ngrXoBRgztok1qzNT1eNA3GGhqVNqO7gQdag+vOReIxyHtcFUc/W7wIERC2zLCwfwCC+jAqsjonjZvnJsX0B0Ds6uGJ69B2U7U57/GcLoewEVjimrPl4aFWmnEMlCmse/vYAQLnTSBMskA2PEYMzs+YIlJtzhw3qv5Xuz3+AgRGarPM4mxSK/oIu4bIX8pwVbiX/GtDiqp8wz+hcQXIt4lnY3dvS5KklctB6VJPmXzrlRz9ujcrHmdOnuE4RSGim92QRMZiZVO7ET9G3wmxV/FiXAy9QBIn8xySr41rDdO5PVSwsKFBNOtLxubfeTlQRJ2eblp2ZJsATo1AdvMx+7PI7ZxrtaEuRyaPLnVn21YFq7slRzNeXx4UNxmRcMk/nOsVoMhFRUui+Vxv55aTZ03rThsV5fF02x5hT4zjCv3DjAQlhbNpBO8K1Dx09lubDa8aChqtamD9NQUgG8bhafeHtwUPoPK9yghEzlYKNh/I0Xv/V51lFkZLcfevMnVuKNEaVchBcx6Oh93kJhMpaASQ0UYvICYDU9hrSue42aR6riEx3XUZeBtJcvRKoBQSw0Crs0nzSPz1NOud2LcVGncp5pX/ICw== git@lukasepple.de"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCb+uVp/SRQnff7JxUIp+VomFrJBpo+ZIU7hyoaln9tAyVx3RW0B5XZlyZJSDXB4G4mn2fc0qpmY7AlEC2be4fzQSC8US5mKOgaoUz0nItdHg8MxDrBCxc8gR6s7/sbupEr0l48M+7GVQOhZV5yKjEF0XN3XnfDpL67tqjPSCxi9KYXLr8zEJCMaE0dKrAWMBUq3P/Q+pdciV1AOvjkrfiFWw1lM+CefOehEp3hwuCkUKOazKIGskx2MymtkFYdIjTeL/WJkT0mpzlUS3uJ3KCCsCgwDBs/hc7Fad4seDEWCAR7sP6OTXcM3Xd23Ygas9ogxLkinIVQzkfOM3eWoQ8JhjZXG2/tnf1JYHappjiBwm3uTxkCy+qRPwiF8+c6J/qHGKC4EPthaZWejpc9ZbZc6xPZEAtPr4MPdC7AtC12uNsJmWfQQVKUuBKAMrkh5LVjIRAfa3pDy1Vzf1wxohH+CVjCp/lpNr9nzhoY1ahAxS+r22zLdmM70R0R1B8PGRRFIIDj7r+0dRG4Oneg1Y9WvuIscrBaqcH9HGS2zfy+r1EvDoXZBQ4jdfQdMp8OHlqOLLV3F/BkMk8NN6rEqZ+flcK++E98ZodIGE4Ekis3eWuyk496d4Tzc5L/tEITl1d6V1GOBbdVNMWJAvL5T3WZVxlrywOcxLjIop9pgcdhnw== git@lukasepple.de"
      ];
    shell = "${pkgs.fish}/bin/fish";
    group = "users";
    extraGroups = [ "wheel" ];
  };

  programs.fish.enable = true;

  system.stateVersion = "unstable";
}

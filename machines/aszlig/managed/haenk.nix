{ config, pkgs, lib, ... }:

{
  boot.initrd.availableKernelModules = [
    "pata_sis" "ohci_pci" "ehci_pci" "firewire_ohci" "sd_mod" "sr_mod"
  ];
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/disk/by-id/ata-FUJITSU_MHV2080AH_NT61T782VR71";
  };

  environment.systemPackages = with pkgs; [
    chromium figlet gajim gimp htop inkscape kde5.oxygen-icons5 libreoffice mosh
    mpv pciutils skype vim_configurable vlc vuizvui.greybird-xfce-theme
    vuizvui.tomahawk wget youtubeDL
  ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/df1cab2d-cbca-4fc5-af6a-c0580c4db1b7";
    fsType = "btrfs";
  };

  swapDevices = lib.singleton {
    device = "/dev/disk/by-uuid/b5ea0ae8-20c6-43dd-ad97-6d8c783dac02";
  };

  hardware.cpu.amd.updateMicrocode = true;

  hardware.firmware = lib.singleton (pkgs.runCommand "ipw2x00-firmware" {} ''
    mkdir -p "$out/lib/firmware"
    cp "${pkgs.fetchsvn rec {
      name = "ipw2x00-${toString rev}";
      url = "svn://svn.debian.org/kernel/dists/trunk/firmware-nonfree/ipw2x00/";
      rev = 22942;
      sha256 = "16jrzqnb1r4aavygp11mh76iqml3xq9xm8j8b18c14dpv08drb55";
    }}"/*.fw "$out/lib/firmware/"
  '');

  i18n.consoleKeyMap = "de";
  i18n.defaultLocale = "en_US.UTF-8";

  networking.hostName = "haenk";
  networking.firewall.enable = false;
  networking.wireless.enable = true;
  networking.useNetworkd = true;
  networking.enableIntel2200BGFirmware = true;

  nix.maxJobs = 1;
  nix.useChroot = true;
  nix.extraOptions = ''
    auto-optimise-store = true
  '';

  nixpkgs.config.allowUnfree = true;

  services.openssh.enable = true;
  services.tlp.enable = true;
  services.ntp.extraFlags = [ "-G" ];

  services.xserver.enable = true;
  services.xserver.layout = "de";
  services.xserver.xkbOptions = "eurosign:e";
  services.xserver.displayManager.auto.enable = true;
  services.xserver.displayManager.auto.user = "bla";
  services.xserver.desktopManager.xfce.enable = true;
  services.xserver.synaptics.enable = true;
  services.xserver.wacom.enable = true;

  time.timeZone = "Europe/Berlin";

  users.extraUsers.bla = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "video" "wheel" ];
  };
}

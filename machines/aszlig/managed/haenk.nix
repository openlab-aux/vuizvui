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

  hardware = {
    cpu.amd.updateMicrocode = true;

    firmware = lib.singleton (pkgs.runCommand "ipw2x00-firmware" {} ''
      mkdir -p "$out/lib/firmware"
      cp "${pkgs.fetchgit rec {
        name = "ipw2x00-20151227";
        url = "git://anonscm.debian.org/kernel/firmware-nonfree.git";
        rev = "e4147b94a856dfe7d4dac11b5da7d9e96b3c2e95";
        sha256 = "18kymqzhlppj520n6vkq5666qgryz3prym1pxn3sqv34yvav7agi";
      }}"/debian/config/ipw2x00/*.fw "$out/lib/firmware/"
    '');

    pulseaudio.enable = true;
  };

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

  nixpkgs.config = {
    allowUnfree = true;
    pulseaudio = true;
    chromium.enablePepperFlash = true;

    packageOverrides = opkgs: {
      # This is because the driver for the NV44M GPU doesn't like LLVM 3.7
      mesa_noglu = opkgs.mesa_noglu.override {
        llvmPackages = opkgs.llvmPackages_36;
      };
    };
  };

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

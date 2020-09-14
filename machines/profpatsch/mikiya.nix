{ config, lib, pkgs, ... }:

let
  myLib  = import ./lib.nix  { inherit pkgs lib; };
  myPkgs = import ./pkgs.nix { inherit pkgs lib myLib; };

  mkDevice = category: num: uuid: {
    name = "mikiya-${category}-crypt-${toString num}";
    device = "/dev/disk/by-uuid/${uuid}";
    keyFile = "/root/raid.key";
  };

  systemDevice = "/dev/disk/by-id/ata-MKNSSDCR60GB-DX_MKN1140A0000025162";
  systemPartition = "/dev/disk/by-uuid/56910867-ed83-438a-b67c-c057e662c89e";
  rootDevice = "/dev/mapper/mikiya-root";

  raidDevices = lib.imap (mkDevice "raid") [
    "f0069e04-d058-40b3-8f13-92f11c4c2546"
  ];



in {
  imports = [ ./base-server.nix ];

  config = {

    boot = {
      loader.grub.device = systemDevice;
      kernelModules = [ "kvm-intel" ];
      kernelParams = [ "ip=192.168.0.5" ];

      initrd = {
        network = {
          enable = true;
          ssh.enable = true;
          ssh.authorizedKeys = myLib.authKeys;
          # we wait until the root device is unlocked (by ssh)
          postCommands = ''
            echo "Waiting for ssh unlock of ${rootDevice} (infinitely)"
            while [ ! -e ${rootDevice} ]; do sleep 1; done
          '';
        };
          availableKernelModules = [
            "ahci" "xhci_pci" "usb_storage" "usbhid" "sd_mod"
          # used for ethernet device(s)
          "r8169"
          ];

        # decrypt root device
        luks.devices.mikiya-root.device = systemPartition;
      };

    };

    fileSystems."/" = {
      device = rootDevice;
      fsType = "ext4";
      options = [ "ssd" ];
    };
    fileSystems."/boot" = {
      device = "/dev/disk/by-uuid/9aa38aa7-652f-4762-a0c2-b70332b93f4d";
      fsType = "ext3";
    };

    nix.maxJobs = 4;

    vuizvui.user.profpatsch.server.sshPort = 22;

    /*
    # decrypt RAID with key from root
    environment.etc.crypttab.text =
      let luksDevice = dev: "${dev.name} ${dev.device} ${dev.keyFile} luks";
      in concatMapStringsSep "\n" luksDevice raidDevices;

    powerManagement = {
      # spin down raid drives after 30 minutes
      powerUpCommand =
        let driveStandby = drive: "${pkgs.hdparm}/sbin/hdparm -S 241 ${drive.device}";
        in concatMapStringsSep "\n" driveStandby raidDevices;
    */

    users.users = { inherit (myLib) philip; };

  };

}

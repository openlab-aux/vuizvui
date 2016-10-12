{ config, pkgs, lib, ... }:

with lib;

let
  rootUUID = "e33a3dda-a87d-473b-b113-37783aa35667";
  swapUUID = "e9f59283-143c-4c36-978c-c730c6ca27c7";
  storeUUID = "ce1db87b-d717-450d-a212-3685a224f626";
  diskID = "ata-Hitachi_HTS543232A7A384_E2P31243FGB6PJ";
in {
  vuizvui.user.aszlig.profiles.workstation.enable = true;
  vuizvui.user.aszlig.programs.taalo-build.enable = true;

  boot = rec {
    kernelPatches = singleton {
      name = "bfqsched";
      patch = pkgs.fetchpatch {
        name = "cfq-replacement.patch";
        url = "https://github.com/linusw/linux-bfq/compare/"
            + "29b4817d4018df78086157ea3a55c1d9424a7cfc"
            + "...cfq_replacement-logical.patch";
        sha256 = "1b3n287r31g0sn85f88dmd00wlsccnm90mr5sr8lj4g1fvnfswqv";
      };
    };

    kernelPackages = with pkgs; let
      trimVer = ver: take 2 (splitString "." (replaceChars ["-"] ["."] ver));
      tooOld = trimVer linux_latest.version == trimVer linux_testing.version;
      kernel = if tooOld then linux_latest else linux_testing;
    in linuxPackagesFor kernel;

    initrd.kernelModules = [ "fbcon" "usb_storage" ];
    loader.grub.device = "/dev/disk/by-id/${diskID}";
    loader.timeout = 1;
  };

  networking.hostName = "tishtushi";
  networking.wireless.enable = mkForce true;

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/${rootUUID}";
    fsType = "btrfs";
    options = [ "space_cache" "compress=zlib" "noatime" ];
  };

  fileSystems."/nix/store" = {
    device = "/dev/disk/by-uuid/${storeUUID}";
    fsType = "btrfs";
    options = [ "ssd" "compress-force=zlib" "noatime" ];
  };

  swapDevices = singleton {
    device = "/dev/disk/by-uuid/${swapUUID}";
  };

  services.synergy.client.enable = true;
  services.synergy.client.serverAddress = "mmrnmhrm";

  services.tlp.enable = true;

  services.xserver.videoDrivers = [ "intel" ];
  services.xserver.synaptics.enable = true;
  services.xserver.synaptics.tapButtons = true;
  services.xserver.synaptics.twoFingerScroll = true;
  services.xserver.synaptics.vertEdgeScroll = false;
  services.xserver.synaptics.accelFactor = "0.1";

  nix.maxJobs = 4;
}

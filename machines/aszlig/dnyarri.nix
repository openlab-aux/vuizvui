{ pkgs, lib, ... }:

with lib;

{
  vuizvui.user.aszlig.profiles.workstation.enable = true;

  nix.maxJobs = 8;

  boot = {
    kernelParams = [ "snd-hda-intel.patch=patch51.fw" ];

    initrd = {
      mdadmConf = ''
        ARRAY /dev/md0 metadata=1.2 UUID=f5e9de04:89efc509:4e184fcc:166b0b67
        ARRAY /dev/md1 metadata=0.90 UUID=b85aa8be:cea0faf2:7abcbee8:eeae037b
      '';
      luks.devices = [
        { name = "system_crypt";
          device = "/dev/md1";
          preLVM = true;
        }
      ];
    };

    loader.grub.devices = [
      "/dev/disk/by-id/ata-ST31500541AS_5XW0AMNH"
      "/dev/disk/by-id/ata-ST31500541AS_6XW0M217"
    ];
  };

  nixpkgs.config.virtualbox.enableExtensionPack = true;

  vuizvui.user.aszlig.system.kernel.enable = true;
  vuizvui.user.aszlig.system.kernel.config = let
    radeonFw = [
      "radeon/R600_rlc.bin"
      "radeon/R700_rlc.bin"
      "radeon/RV710_uvd.bin"
      "radeon/RV710_smc.bin"
      "radeon/RV730_smc.bin"
    ];

    extraFw = radeonFw ++ [ "patch51.fw" ];

    patch51 = pkgs.writeText "patch51.fw" ''
      [codec]
      0x10ec0889 0x80860033 2

      [pincfg]
      0x11 0x01442130
      0x12 0x411111f0
      0x14 0x01014410
      0x15 0x0321403f
      0x16 0x40f000f0
      0x17 0x40f000f0
      0x18 0x03a19020
      0x19 0x40f000f0
      0x1a 0x01014412
      0x1b 0x01014411
      0x1c 0x411111f0
      0x1d 0x411111f0
      0x1e 0x01451140
      0x1f 0x01c51170

      [model]
      auto
    '';

  in import ./dnyarri-kconf.nix // {
    CONFIG_EXTRA_FIRMWARE = concatStringsSep " " extraFw;
    CONFIG_EXTRA_FIRMWARE_DIR = pkgs.stdenv.mkDerivation {
      name = "builtin-firmware";
      buildCommand = let
        firmwareBasePath = "${pkgs.firmwareLinuxNonfree}/lib/firmware";
      in ''
        mkdir -p "$out/radeon"
        ${concatMapStrings (x: ''
          cp -Lv -t "$out/radeon" "${firmwareBasePath}/${x}"
        '') radeonFw}

        cp "${patch51}" "$out/patch51.fw"
      '';
    };
  };

  networking.hostName = "dnyarri";

  fileSystems = {
    "/boot" = {
      label = "boot";
      fsType = "ext2";
    };
    "/" = {
      device = "/dev/shofixti/root";
      fsType = "xfs";
    };
  };

  powerManagement.powerUpCommands = ''
    ${pkgs.hdparm}/sbin/hdparm -B 255 /dev/disk/by-id/ata-ST31500541AS_5XW0AMNH
    ${pkgs.hdparm}/sbin/hdparm -B 255 /dev/disk/by-id/ata-ST31500541AS_6XW0M217
  '';

  swapDevices = singleton {
    device = "/dev/shofixti/swap";
  };

  # TODO: Try to avoid this, but as there is only a single user using audio on
  # this machine, it's okay for now. But remember that this will break heavily,
  # should there be another user accessing the audio devices.
  users.extraUsers.aszlig.extraGroups = [ "audio" "vboxusers" ];

  services.synergy.client.enable = true;
  services.synergy.client.serverAddress = "mmrnmhrm";

  systemd.services."synergy-client".serviceConfig.CPUSchedulingPolicy = "rr";
  systemd.services."synergy-client".serviceConfig.CPUSchedulingPriority = 50;

  services.xserver.videoDrivers = [ "ati" ];
  services.xserver.xrandrHeads = [ /* "HDMI-0" */ "DVI-0" ];

  vuizvui.user.aszlig.services.i3.reverseHeads = true;
  vuizvui.user.aszlig.services.i3.workspaces."6" = {
    label = "Chromium";
    assign = singleton { class = "^Chromium(?:-browser)?\$"; };
  };
}

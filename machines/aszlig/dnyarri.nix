{ pkgs, ... }:

with pkgs.lib;
with import ../../lib;

{
  imports = singleton ../../common-workstation.nix;

  nix.maxJobs = 8;

  boot = let
    patch51Name = "patch51.fw";
    kernelParams = [ "snd-hda-intel.patch=${patch51Name}" ];

    patch51 = pkgs.writeText patch51Name ''
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

    radeonFW = [
      "radeon/R600_rlc.bin"
      "radeon/R700_rlc.bin"
      "radeon/RV710_uvd.bin"
      "radeon/RV710_smc.bin"
      "radeon/RV730_smc.bin"
    ];

    linuxVuizvui = pkgs.buildLinux {
      inherit (pkgs.kernelSourceVuizvui) version src;

      kernelPatches = singleton pkgs.vuizvuiKernelPatches.bfqsched;
      configfile = pkgs.substituteAll {
        name = "vuizvui-with-firmware.kconf";
        src = generateKConf (import ./dnyarri-kconf.nix);

        extra_firmware = concatStringsSep " " (radeonFW ++ [
          "patch51.fw"
        ]);

        builtin_firmware = pkgs.stdenv.mkDerivation {
          name = "builtin-firmware";
          buildCommand = let
            firmwareBasePath = "${pkgs.firmwareLinuxNonfree}/lib/firmware";
          in ''
            mkdir -p "$out/radeon"
            ${concatMapStrings (x: ''
              cp -Lv -t "$out/radeon" "${firmwareBasePath}/${x}";
            '') radeonFW}

            cp "${patch51}" "$out/${patch51Name}"
          '';
        };
      };
      allowImportFromDerivation = true; # XXX
    };
  in rec {
    kernelPackages = let
      kpkgs = pkgs.recurseIntoAttrs
        (pkgs.linuxPackagesFor linuxVuizvui kernelPackages);
      virtualbox = kpkgs.virtualbox.override {
        enableExtensionPack = true;
      };
    in pkgs.recurseIntoAttrs (kpkgs // { inherit virtualbox; });
    inherit kernelParams;

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
  services.xserver.xrandrHeads = [ "HDMI-0" "DVI-0" ];

  vuizvui.user.aszlig.services.i3.reverseHeads = true;
  vuizvui.user.aszlig.services.i3.workspaces."6" = {
    label = "Chromium";
    assign = singleton { class = "^Chromium(?:-browser)?\$"; };
  };
}

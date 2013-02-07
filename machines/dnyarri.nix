{config, pkgs, ...}:

with pkgs.lib;
with import ../lib;

{
  require = singleton ../common.nix;

  boot = let
    patch51Name = "patch51.fw";
    extraKernelParams = [ "snd-hda-intel.patch=${patch51Name}" ];

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

    builtinFW = [
      "${pkgs.radeonR600}/radeon/R600_rlc.bin"
      "${pkgs.radeonR700}/radeon/R700_rlc.bin"
    ];

    linuxAszlig = pkgs.linuxManualConfig {
      version = pkgs.kernelSourceAszlig.version;
      src = pkgs.kernelSourceAszlig.src;
      configfile = pkgs.substituteAll {
        name = "aszlig-with-firmware.kconf";
        src = generateKConf (import ./dnyarri-kconf.nix);

        builtin_firmware = pkgs.stdenv.mkDerivation {
          name = "builtin-firmware";
          buildCommand = ''
            mkdir -p "$out/radeon"
            ${concatMapStrings (x: "cp -Lv -t \"$out/radeon\" \"${x}\";") builtinFW}

            cp "${patch51}" "$out/${patch51Name}"
          '';
        };
      };
      allowImportFromDerivation = true; # XXX
    };
  in rec {
    kernelPackages = pkgs.linuxPackagesFor linuxAszlig kernelPackages;
    inherit extraKernelParams;

    initrd = {
      mdadmConf = ''
        ARRAY /dev/md0 metadata=1.2 UUID=f5e9de04:89efc509:4e184fcc:166b0b67
        ARRAY /dev/md1 metadata=0.90 UUID=b85aa8be:cea0faf2:7abcbee8:eeae037b
      '';
      luks.enable = true;
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

  services.xserver = {
    videoDrivers = [ "ati" ];

    xrandrHeads = [ "HDMI-0" "DVI-0" ];

    displayManager.sessionCommands = ''
      ${pkgs.synergy}/bin/synergyc mmrnmhrm
    '';
  };
}

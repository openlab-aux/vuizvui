{config, pkgs, ...}:
{
  require = [
    <nixos/modules/programs/virtualbox.nix>
  ];

  nix = {
    maxJobs = 8;
    useChroot = true;
    readOnlyStore = true;
    extraOptions = ''
      build-cores = 0
    '';
  };

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
      version = "3.7.0-rc6";
      src = pkgs.fetchgit {
        url = /home/aszlig/linux;
        rev = "f60c7ab5c56292820f96d8fcb21124c53ae02d0e";
        sha256 = "0zy6p14qjnk3dl3hy725m9mlavdklq2zjk64jk75ajmfygbz2q56";
      };
      configfile = pkgs.substituteAll {
        name = "aszlig-with-firmware.kconf";

        src = pkgs.fetchurl {
          name = "aszlig.kconf";
          url = "file:///home/aszlig/linux/.config";
          md5 = "318762752f2831d26a315d040437f42a";
        };

        builtin_firmware = pkgs.stdenv.mkDerivation {
          name = "builtin-firmware";
          buildCommand = ''
            mkdir -p "$out/radeon"
            ${pkgs.lib.concatMapStrings (x: "cp -Lv -t \"$out/radeon\" \"${x}\";") builtinFW}

            cp "${patch51}" "$out/${patch51Name}"
          '';
        };
      };
      allowImportFromDerivation = true; # XXX
    };
  in rec {
    kernelPackages = pkgs.linuxPackagesFor linuxAszlig kernelPackages;
    inherit extraKernelParams;

    cleanTmpDir = true;

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

    loader.grub = {
      enable = true;
      version = 2;

      memtest86 = true;

      devices = [
        "/dev/disk/by-id/ata-ST31500541AS_5XW0AMNH"
        "/dev/disk/by-id/ata-ST31500541AS_6XW0M217"
      ];
    };
  };

  hardware = {
    cpu.intel.updateMicrocode = true;
    pulseaudio.enable = true;
    pulseaudio.package = pkgs.pulseaudio.override {
      useSystemd = true;
    };
  };

  users.defaultUserShell = "/var/run/current-system/sw/bin/zsh";

  networking = {
    hostName = "dnyarri";
    wireless.enable = false;
  };

  fileSystems = {
    "/boot" = {
      label = "boot";
      fsType = "ext2";
    };
    "/" = {
      device = "/dev/shofixti/root";
      fsType = "xfs";
    };
    /*
    "/run/nix/remote-stores/mmrnmhrm/nix" = {
      device = "root@mmrnmhrm:/nix";
      fsType = "sshfs";
      noCheck = true;
      options = pkgs.lib.concatStringsSep "," [
        "comment=x-systemd.automount"
        "compression=yes"
        "ssh_command=${pkgs.openssh}/bin/ssh"
        "Ciphers=arcfour"
        "IdentityFile=/root/.ssh/id_buildfarm"
      ];
    };
    */
  };

  fonts = {
    enableCoreFonts = true;
    enableFontDir = true;
    enableGhostscriptFonts = true;
    extraFonts = [
      pkgs.dosemu_fonts
      pkgs.liberation_ttf
    ];
  };

  powerManagement.powerUpCommands = ''
    ${pkgs.hdparm}/sbin/hdparm -B 255 /dev/disk/by-id/ata-ST31500541AS_5XW0AMNH
    ${pkgs.hdparm}/sbin/hdparm -B 255 /dev/disk/by-id/ata-ST31500541AS_6XW0M217
  '';

  swapDevices = [
    { device = "/dev/shofixti/swap"; }
  ];

  i18n = {
    consoleKeyMap = "dvorak";
  };

  services = {
    openssh = {
      enable = true;
      permitRootLogin = "without-password";
    };

    /*
    nfs.server = {
      enable = true;
      exports = ''
        /nix mmrnmhrm.redmoon(ro,no_root_squash)
      '';
    };
    */

    /* mingetty.ttys = [
      "tty1" "tty2" "tty3" "tty4" "tty5" "tty6"
      "tty8" "tty9" "tty10" "tty11" "tty12"
    ]; */

    syslogd.tty = "tty13";

    xfs.enable = false;

    gpm = {
      enable = true;
      protocol = "exps2";
    };

    nixosManual.showManual = false;

    pulseaudio.enable = false;

    printing = {
      enable = true;
      drivers = [ pkgs.foo2zjs pkgs.foomatic_filters ];
    };

    xserver = {
      enable = true;
      layout = "dvorak";
      videoDrivers = [ "ati" ];

      windowManager = {
        i3.enable = true;
        default = "i3";
      };

      desktopManager.default = "none";

      displayManager.sessionCommands = ''
        ${pkgs.synergy}/bin/synergyc mmrnmhrm
        # work around synergy bug:
        ${pkgs.xorg.setxkbmap}/bin/setxkbmap dvorak
      '';

      displayManager.slim.theme = pkgs.fetchurl {
        url = "mirror://sourceforge/slim.berlios/slim-fingerprint.tar.gz";
        sha256 = "0i1igl4iciml3d46n5hl3bbmqsdzzv56akw2l36i9f779kw07ds8";
      };
    };
  };

  /*
  jobs.vlock_all = {
    name = "vlock-all";
    startOn = "keyboard-request";
    path = [ pkgs.vlock ];
    script = "vlock -asn";
    task = true;
    restartIfChanged = false;
  };
  */

  environment.nix = pkgs.nixUnstable;
  environment.systemPackages = with pkgs; [
    zsh
    wget
    vim_configurable
    cacert
  ];

  nixpkgs = {
    config = {
      git = {
        svnSupport = true;
        guiSupport = true;
      };
    };
  };

  system.fsPackages = with pkgs; [
    sshfsFuse
  ];

  # broken -> chroot build -> FIXME
  #system.copySystemConfiguration = true;

  time.timeZone = "Europe/Berlin";
}

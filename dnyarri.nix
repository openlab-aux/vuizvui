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
    builtinFW = [
      "${pkgs.radeonR600}/radeon/R600_rlc.bin"
      "${pkgs.radeonR700}/radeon/R700_rlc.bin"
    ];

    linuxAszlig = pkgs.linuxManualConfig {
      version = "3.7.0-rc6";
      src = pkgs.fetchgit {
        url = /home/aszlig/linux;
        rev = "f4a75d2eb7b1e2206094b901be09adb31ba63681";
        sha256 = "1r3y3z79yw8pcxrm6nc8iacdy0w13favgd53ra0w6hn1186vmg21";
      };
      configfile = pkgs.substituteAll {
        src = ./kernel.config;

        builtin_firmware = pkgs.stdenv.mkDerivation {
          name = "builtin-firmware";
          buildCommand = ''
            mkdir -p "$out/radeon"
            ${pkgs.lib.concatMapStrings (x: "cp -Lv -t \"$out/radeon\" \"${x}\";") builtinFW}
          '';
        };
      };
      allowImportFromDerivation = true; # XXX
    };
  in rec {
    kernelPackages = pkgs.linuxPackagesFor linuxAszlig kernelPackages;

    cleanTmpDir = true;

    supportedFilesystems = [ "xfs" ];

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

      inherit supportedFilesystems;
    };

    loader.grub = {
      enable = true;
      version = 2;

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

  fileSystems."/boot".label = "boot";
  fileSystems."/".device = "/dev/shofixti/root";

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
      permitRootLogin = "no";
    };

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

  # broken -> chroot build -> FIXME
  #system.copySystemConfiguration = true;

  time.timeZone = "Europe/Berlin";
}

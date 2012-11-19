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
    linuxAszlig = pkgs.linuxManualConfig {
      version = "3.7.0-rc6";
      src = pkgs.fetchgit {
        url = /home/aszlig/linux;
        rev = "f4a75d2eb7b1e2206094b901be09adb31ba63681";
        sha256 = "1r3y3z79yw8pcxrm6nc8iacdy0w13favgd53ra0w6hn1186vmg21";
      };
      configfile = pkgs.fetchurl {
        name = "aszlig.kconf";
        url = "file:///home/aszlig/linux/.config";
        md5 = "6e533e2e9b6ce0632678b83da093d22e";
      };
      allowImportFromDerivation = true; # XXX
    };
  in rec {
    kernelPackages = pkgs.linuxPackagesFor linuxAszlig kernelPackages;

    cleanTmpDir = true;

    supportedFilesystems = [ "btrfs" ];

    initrd = {
      luks.enable = true;
      luks.devices = [
        { name = "system_crypt";
          device = "/dev/disk/by-uuid/91fbc437-b191-4c32-add4-6f96b2c953dd";
          preLVM = true;
        }
      ];

      inherit supportedFilesystems;
    };

    loader.grub = {
      enable = true;
      version = 2;

      devices = [
        "/dev/disk/by-id/ata-WDC_WD10EAVS-00D7B1_WD-WCAU48931237"
      ];
    };
  };

  hardware = {
    cpu.intel.updateMicrocode = true;
    pulseaudio.enable = false;
  };

  users.defaultUserShell = "/var/run/current-system/sw/bin/zsh";

  networking = {
    hostName = "mmrnmhrm";
    wireless.enable = false;
  };

  fileSystems."/boot".label = "boot";
  fileSystems."/" = {
    device = "/dev/system/root";
    options = pkgs.lib.concatStringsSep "," [
      "autodefrag"
      "space_cache"
      "inode_cache"
      "compress=lzo"
      "noatime"
    ];
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

  swapDevices = [
    { device = "/dev/system/swap"; }
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
      videoDrivers = [ "nouveau" ];

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

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
    /*
    buildMachines = [
      { hostName = "
    ];
    */
  };

  boot = let
    linuxAszlig = pkgs.linuxManualConfig {
      version = "3.7.0-rc6";
      src = pkgs.fetchgit {
        url = /home/aszlig/linux;
        rev = "c56dcc86b9a0a140ae0b35abb4b2ecd1b45e8bda";
        sha256 = "1km8dgfgdcgngcdnj5jzy98zyn7mrfryygnrp2wvzk5vi53wksmx";
      };
      configfile = pkgs.fetchurl {
        name = "aszlig.kconf";
        url = "file:///home/aszlig/linux/.config";
        md5 = "0c632194689797846127b47fa135c516";
      };
      allowImportFromDerivation = true; # XXX
    };
  in rec {
    kernelPackages = pkgs.linuxPackagesFor linuxAszlig kernelPackages;

    cleanTmpDir = true;

    initrd = {
      luks.enable = true;
      luks.devices = [
        { name = "system_crypt";
          device = "/dev/disk/by-uuid/91fbc437-b191-4c32-add4-6f96b2c953dd";
          preLVM = true;
        }
      ];
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
    pulseaudio.enable = true;
    pulseaudio.package = pkgs.pulseaudio.override {
      useSystemd = true;
    };
  };

  users.defaultUserShell = "/var/run/current-system/sw/bin/zsh";

  networking = {
    hostName = "mmrnmhrm";
    wireless.enable = false;
  };

  fileSystems = {
    "/boot" = {
      label = "boot";
      fsType = "ext2";
    };
    "/" = {
      device = "/dev/system/root";
      fsType = "btrfs";
      options = pkgs.lib.concatStringsSep "," [
        "autodefrag"
        "space_cache"
        "inode_cache"
        "compress=lzo"
        "noatime"
      ];
    };
    /*
    "/run/nix/remote-stores/dnyarri/nix" = {
      device = "root@dnyarri:/nix";
      fsType = "sshfs";
      options = pkgs.lib.concatStringsSep "," [
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

  swapDevices = [
    { device = "/dev/system/swap"; }
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
        /nix dnyarri.redmoon(ro,no_root_squash)
        /nix/var/nix/db dnyarri.redmoon(rw,no_root_squash)
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

  system.fsPackages = with pkgs; [
    sshfsFuse
  ];

  # broken -> chroot build -> FIXME
  #system.copySystemConfiguration = true;

  time.timeZone = "Europe/Berlin";
}

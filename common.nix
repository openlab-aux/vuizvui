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

  boot = {
    cleanTmpDir = true;

    loader.grub = {
      enable = true;
      version = 2;
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
    wireless.enable = false;
  };

  fileSystems = {
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

  i18n = {
    consoleKeyMap = "dvorak";
  };

  services = {
    openssh = {
      enable = true;
      permitRootLogin = "without-password";
    };

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

      startGnuPGAgent = true;
      startOpenSSHAgent = false;

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

  users.extraUsers.aszlig = {
    description = "aszlig";
    group = "users";
    extraGroups = [ "vboxusers" "wheel" ];
    home = "/home/aszlig";
    isSystemUser = false;
    useDefaultShell = true;
    createHome = true;
    createUser = true;
  };

  environment.nix = pkgs.nixUnstable;
  environment.systemPackages = with pkgs; [
    cacert
    file
    htop
    psmisc
    binutils
    unrar
    unzip
    vim_configurable
    vlock
    wget
    xz
    zsh
  ];

  nixpkgs.config = import ./nixpkgs/config.nix;

  system.fsPackages = with pkgs; [
    sshfsFuse
  ];

  # broken -> chroot build -> FIXME
  #system.copySystemConfiguration = true;

  time.timeZone = "Europe/Berlin";
}

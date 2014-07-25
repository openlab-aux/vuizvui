{config, pkgs, ...}:
{
  imports = [
    <nixpkgs/nixos/modules/programs/virtualbox.nix>
    ./modules/i3
  ];

  nix = {
    package = pkgs.nixUnstable;
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

    kernelParams = [ "zswap.enabled=1" ];
  };

  hardware = {
    cpu.intel.updateMicrocode = true;
    pulseaudio.enable = true;
    pulseaudio.package = pkgs.pulseaudio.override {
      useSystemd = true;
    };
    opengl = {
      driSupport32Bit = true;
      s3tcSupport = true;
    };
  };

  users.defaultUserShell = "/var/run/current-system/sw/bin/zsh";

  networking = {
    wireless.enable = false;
    firewall.enable = false;
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
    fonts = [
      pkgs.dosemu_fonts
      pkgs.liberation_ttf
    ];
  };

  i18n = {
    consoleKeyMap = "dvorak";
  };

  programs.ssh.startAgent = false;

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

    printing = {
      enable = true;
      drivers = [ pkgs.foo2zjs pkgs.foomatic_filters ];
    };

    udev.extraRules = ''
      SUBSYSTEM=="tty", ATTRS{idVendor}=="10c4", ATTRS{idProduct}=="ea60", \
        ATTRS{serial}=="0001", OWNER="aszlig", SYMLINK+="axbo"
      SUBSYSTEM=="usb*|tty", ACTION=="add|change", ATTRS{idVendor}=="0403", \
        ATTRS{idProduct}=="6001", OWNER="aszlig" # Enttec
    '';

    xserver = {
      enable = true;
      layout = "dvorak";

      startGnuPGAgent = true;

      displayManager.sessionCommands = ''
        ${pkgs.redshift}/bin/redshift -l 48.428404:10.866007 &
      '';

      desktopManager.default = "none";
      desktopManager.xterm.enable = false;

      displayManager.lightdm.enable = true;
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
    uid = 1000;
    description = "aszlig";
    group = "users";
    extraGroups = [ "vboxusers" "wheel" "video" ];
    home = "/home/aszlig";
    useDefaultShell = true;
    createHome = true;
    createUser = true;
  };

  environment.systemPackages = with pkgs; [
    binutils
    cacert
    file
    htop
    iotop
    psmisc
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

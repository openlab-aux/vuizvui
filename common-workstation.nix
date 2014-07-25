{ pkgs, ... }:

{
  imports = [
    ./common.nix
    <nixpkgs/nixos/modules/programs/virtualbox.nix>
    ./modules/i3
  ];

  boot.kernelParams = [ "zswap.enabled=1" ];
  boot.cleanTmpDir = true;

  hardware = {
    pulseaudio.enable = true;
    pulseaudio.package = pkgs.pulseaudio.override {
      useSystemd = true;
    };
    opengl = {
      driSupport32Bit = true;
      s3tcSupport = true;
    };
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
}

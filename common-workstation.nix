{ pkgs, config, lib, ... }:

let
  randrHeads = config.services.xserver.xrandrHeads;
in {
  imports = [
    ./common.nix
    ./packages.nix
    ./zsh.nix
    <nixpkgs/nixos/modules/programs/virtualbox.nix>
  ];

  boot.kernelParams = [ "zswap.enabled=1" ];
  boot.cleanTmpDir = true;

  environment.systemPackages = with lib; let
    mkRandrConf = acc: name: acc ++ singleton {
      inherit name;
      value = "--output '${name}' --preferred"
            + optionalString (acc != []) " --right-of '${(head acc).name}'";
    };
    randrConf = map (getAttr "value") (foldl mkRandrConf [] randrHeads);
  in singleton (pkgs.writeScriptBin "xreset" ''
    #!${pkgs.stdenv.shell}
    ${pkgs.xorg.xrandr}/bin/xrandr ${concatStringsSep " " randrConf}
  '');

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

  vuizvui.i3.enable = true;

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
        ${pkgs.xorg.xrdb}/bin/xrdb "${pkgs.writeText "xrdb.config" ''
          Rxvt*font:                 vga
          Rxvt*background:           black
          Rxvt*foreground:           grey
          Rxvt*scrollBar:            false
          Rxvt*saveLines:            2000

          Rxvt*keysym.Home:          \033[1~
          Rxvt*keysym.End:           \033[4~

          Rxvt*urgentOnBell:         true

          XTerm*font:                vga
          XTerm*saveLines:           10000
          XTerm*bellIsUrgent:        true
          XTerm*background:          black
          XTerm*foreground:          grey

          XTerm*backarrowKeyIsErase: true
          XTerm*ptyInitialErase:     true
        ''}"
      '';

      desktopManager.default = "none";
      desktopManager.xterm.enable = false;
      displayManager.slim.enable = true;
      displayManager.slim.defaultUser = "aszlig";
      displayManager.slim.theme = pkgs.stdenv.mkDerivation {
        name = "nixos-theme-vuizvui";
        src = pkgs.slimThemes.nixosSlim;
        phases = [ "unpackPhase" "patchPhase" "installPhase" ];
        patchPhase = let
          headFactor = if randrHeads == [] then 1 else lib.length randrHeads;
          centerLeft = 100 / (headFactor * 2);
        in ''
          ${pkgs.imagemagick}/bin/mogrify \
            -fill '#080010' -draw 'color 0,0 reset' \
            share/slim/themes/nixos-slim-testing/background.png
          ${pkgs.imagemagick}/bin/mogrify \
            -negate -region 100x110+0+0 -negate -fill white -colorize 20% \
            share/slim/themes/nixos-slim-testing/panel.png
          sed -i \
            -e 's/^\([a-z_]\+_x[^0-9]*\)[0-9]\+%/\1${toString centerLeft}%/' \
            share/slim/themes/nixos-slim-testing/slim.theme
          cat >> share/slim/themes/nixos-slim-testing/slim.theme <<EOF
          session_x      ${toString centerLeft}%
          msg_color      #ffffff
          username_color #ffffff
          password_color #ffffff
          input_color    #ffffff
          EOF
        '';
        installPhase = ''
          cp -R share/slim/themes/nixos-slim-testing "$out"
        '';
      };
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
  };
}

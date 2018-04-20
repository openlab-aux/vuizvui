{ pkgs, config, lib, ... }:

let
  cfg = config.vuizvui.user.aszlig.profiles.workstation;
  inherit (config.services.xserver) xrandrHeads;

in {
  options.vuizvui.user.aszlig.profiles.workstation = {
    enable = lib.mkEnableOption "Workstation profile for aszlig";
  };

  config = lib.mkIf cfg.enable {
    vuizvui.user.aszlig.profiles.base.enable = true;

    boot.kernelParams = [ "zswap.enabled=1" "panic=1800" ];
    boot.cleanTmpDir = true;

    environment.systemPackages = with lib; let
      mkRandrConf = acc: rcfg: acc ++ singleton {
        name = rcfg.output;
        value = "--output ${lib.escapeShellArg rcfg.output} --preferred"
              + optionalString rcfg.primary " --primary"
              + optionalString (acc != []) " --right-of '${(head acc).name}'";
      };
      randrConf = map (getAttr "value") (foldl mkRandrConf [] xrandrHeads);
    in singleton (pkgs.writeScriptBin "xreset" ''
      #!${pkgs.stdenv.shell}
      ${pkgs.xorg.xrandr}/bin/xrandr ${concatStringsSep " " randrConf}
    '') ++ import ./packages.nix pkgs;

    environment.pathsToLink = lib.singleton "/share/chromium/extensions";

    vuizvui.lazyPackages = import ./lazy-packages.nix pkgs;

    sound.enable = true;

    hardware = {
      pulseaudio.enable = true;
      pulseaudio.package = pkgs.pulseaudioFull;
      opengl = {
        driSupport32Bit = true;
        s3tcSupport = true;
      };
    };

    fonts = {
      enableFontDir = true;
      enableGhostscriptFonts = true;
      fonts = [
        pkgs.dosemu_fonts
        pkgs.liberation_ttf
      ];
    };

    vuizvui.user.aszlig.services.i3.enable = true;
    vuizvui.user.aszlig.services.slim.enable = true;
    vuizvui.user.aszlig.services.vlock.enable = true;

    vuizvui.user.aszlig.programs.mpv.enable = true;
    vuizvui.user.aszlig.programs.taskwarrior.enable = true;

    vuizvui.user.aszlig.programs.git.enable = true;
    vuizvui.user.aszlig.programs.git.config = {
      color.ui = "auto";
      merge.tool = "vimdiff3";
      user.email = "aszlig@nix.build";
      user.name = "aszlig";
      user.signingkey = "DD526BC7767DBA2816C095E5684089CE67EBB691";
      gpg.program = "${pkgs.gnupg}/bin/gpg";
      push.default = "current";
      tar."tar.xz".command = "${pkgs.xz}/bin/xz -c";
      rebase.autosquash = true;
      rerere.enabled = true;
      rerere.autoupdate = true;
      commit.gpgsign = true;

      alias.backport = let
        release = "14.04";
        message = "Merge release ${release} into backports.";
      in "!git fetch upstream release-${release} &&"
       + " git merge -m \"${message}\" --log FETCH_HEAD";
    };

    vuizvui.hardware.gameController."03000000ff1100004133000010010000" = {
      name = "PS2 Controller";
      mapping = {
        a = "b2";
        b = "b1";
        x = "b3";
        y = "b0";
        back = "b8";
        start = "b9";
        leftshoulder = "b6";
        rightshoulder = "b7";
        leftstick = "b10";
        rightstick = "b11";
        leftx = "a0";
        lefty = "a1";
        rightx = "a3";
        righty = "a2";
        lefttrigger = "b4";
        righttrigger = "b5";
        dpup = "h0.1";
        dpleft = "h0.8";
        dpdown = "h0.4";
        dpright = "h0.2";
      };
    };

    vuizvui.programs.gnupg.enable = true;
    vuizvui.programs.gnupg.agent.enable = true;
    vuizvui.programs.gnupg.agent.sshSupport = true;
    vuizvui.programs.gnupg.agent.scdaemon.enable = true;

    services = {
      openssh.enable = true;

      xfs.enable = false;

      gpm = {
        enable = true;
        protocol = "exps2";
      };

      printing.enable = true;
      printing.drivers = [ pkgs.gutenprint pkgs.hplip ];

      pcscd.enable = true;
      pcscd.plugins = [ pkgs.ccid pkgs.pcsc-cyberjack ];

      udev.extraRules = ''
        # aXbo S.P.A.C.
        SUBSYSTEM=="tty", ATTRS{idVendor}=="10c4", ATTRS{idProduct}=="ea60", \
          ATTRS{serial}=="0001", OWNER="aszlig", SYMLINK+="axbo"
        # Enttec DMX device
        SUBSYSTEM=="usb*|tty", ACTION=="add|change", ATTRS{idVendor}=="0403", \
          ATTRS{idProduct}=="6001", OWNER="aszlig"
      '';

      redshift = {
        enable = true;
        latitude = "48.428404";
        longitude = "10.866007";
        temperature.day = 5500;
        temperature.night = 3500;
      };

      xserver = {
        enable = true;
        layout = "dvorak";

        displayManager.sessionCommands = ''
          ${pkgs.xorg.xrdb}/bin/xrdb "${pkgs.writeText "xrdb.config" ''
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
      };
    };

    users.users.aszlig = {
      uid = 1000;
      isNormalUser = true;
      description = "aszlig";
      extraGroups = [ "wheel" "video" ];
    };
  };
}

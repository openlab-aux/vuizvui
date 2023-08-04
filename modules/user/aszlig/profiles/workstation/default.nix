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

    boot.kernelParams = [ "panic=1800" ];
    boot.tmp.cleanOnBoot = true;

    # Allow sync, sak and unraw
    boot.kernel.sysctl."kernel.sysrq" = 20;

    environment.systemPackages = with lib; let
      mkRandrConf = acc: rcfg: acc ++ singleton {
        name = rcfg.output;
        value = "--output ${lib.escapeShellArg rcfg.output} --preferred"
              + optionalString rcfg.primary " --primary"
              + optionalString (acc != []) " --right-of '${(head acc).name}'";
      };
      randrConf = map (getAttr "value") (foldl mkRandrConf [] xrandrHeads);
      xreset = pkgs.writeScriptBin "xreset" ''
        #!${pkgs.stdenv.shell}
        ${pkgs.xorg.xrandr}/bin/xrandr ${concatStringsSep " " randrConf}
      '';
    in [
      xreset
      config.boot.kernelPackages.perf
      (pkgs.vuizvui.aszlig.psi.override {
        jid = "aszlig@aszlig.net";
        resource = config.networking.hostName;
      })
    ] ++ import ./packages.nix pkgs;

    vuizvui.requiresTests = [
      ["vuizvui" "aszlig" "programs" "psi"]
    ];

    environment.pathsToLink = lib.singleton "/share/chromium/extensions";

    # The default theme hurts my eyes.
    environment.variables.GTK_THEME = "Adwaita:dark";

    vuizvui.lazyPackages = import ./lazy-packages.nix pkgs;

    sound.enable = true;

    hardware = {
      pulseaudio.enable = true;
      pulseaudio.package = pkgs.pulseaudioFull;
      opengl = {
        driSupport32Bit = true;
      };
    };

    fonts = {
      fontDir.enable = true;
      enableGhostscriptFonts = true;
      fontconfig.useEmbeddedBitmaps = true;
      fonts = lib.singleton pkgs.ultimate-oldschool-pc-font-pack;
    };

    vuizvui.user.aszlig.services.i3.enable = true;
    vuizvui.user.aszlig.services.vlock.enable = true;
    vuizvui.user.aszlig.services.vlock.user = "aszlig";

    vuizvui.user.aszlig.programs.flameshot.enable = true;
    vuizvui.user.aszlig.programs.mpv.enable = true;

    vuizvui.user.aszlig.programs.git.enable = true;
    vuizvui.user.aszlig.programs.git.delta.enable = true;
    vuizvui.user.aszlig.programs.git.settings = {
      color.ui = "auto";
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
      pull.rebase = false;
      branch.sort = "-committerdate";

      merge.tool = "vimdiff3";
      merge.conflictstyle = "diff3";

      delta = {
        commit-decoration-style = "bold yellow box";
        file-decoration-style = "blue ul";
        file-style = "bold blue ul";
        hunk-header-style = "line-number dim italic syntax";
        hunk-header-decoration-style = "blue";
        hunk-header-line-number-style = "cyan";
        plus-style = "syntax auto";
        minus-style = "syntax auto";
        line-numbers = true;
      };
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

    vuizvui.system.kernel.zswap.enable = true;

    location.latitude = 48.4284;
    location.longitude = 10.866;

    # I occasionally run wg setups and I don't really care whether they're
    # online or not.
    systemd.network.wait-online.anyInterface = true;

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
        temperature.day = 5500;
        temperature.night = 3500;
        brightness.night = "0.5";
      };

      xserver = {
        enable = true;

        displayManager = {
          lightdm.enable = true;
          defaultSession = "none+i3";
          sessionCommands = ''
            ${pkgs.xorg.xrdb}/bin/xrdb "${pkgs.writeText "xrdb.config" ''
              XTerm*termName:            xterm-direct
              XTerm*directColor:         true
              XTerm*faceName:            MxPlus IBM VGA 8x16
              XTerm*faceSize:            12
              XTerm*renderFont:          true
              XTerm*saveLines:           10000
              XTerm*bellIsUrgent:        true
              XTerm*background:          black
              XTerm*foreground:          grey

              XTerm*backarrowKeyIsErase: true
              XTerm*ptyInitialErase:     true
            ''}"
          '';
        } // lib.optionalAttrs (config.boot.initrd.luks.devices != {}) {
          # All of my workstations are single-user machines with encrypted root
          # and swap, so there is no need to prompt another time for a password
          # or passphrase.
          autoLogin.enable = true;
          autoLogin.user = "aszlig";
        };
      };
    };

    nixpkgs.overlays = lib.singleton (lib.const (super: {
      xterm = super.vuizvui.aszlig.xterm;
    }));

    users.users.aszlig = {
      uid = 1000;
      isNormalUser = true;
      description = "aszlig";
      extraGroups = [ "wheel" "video" ];
    };
  };
}

# TODO(sterni) icons, GTK
# TODO(sterni) X11 cursor
{ config, pkgs, lib, ... }:

let
  cfg = config.vuizvui.user.sternenseemann.profiles.desktop-sway;

  inherit (pkgs.vuizvui.profpatsch)
    getBins
    ;

  inherit (pkgs.vuizvui.sternenseemann)
    tep
    ;

  bins = (getBins pkgs.bemenu [ "bemenu" "bemenu-run" ])
      // (getBins tep.wayland [ "tep" ])
      // (getBins pkgs.grim [ "grim" ])
      // (getBins pkgs.slurp [ "slurp" ])
      // (getBins config.vuizvui.user.sternenseemann.services.mako.package [ "makoctl" ])
      // (getBins screenshot [ "screenshot" ])
      // (getBins pkgs.foot [ "foot" ])
      // (getBins config.vuizvui.user.sternenseemann.programs.saneterm.package [ "saneterm" ])
      // (getBins pkgs.bash [ "sh" ])
      ;

  screenshot = pkgs.writers.writeDashBin "screenshot" ''
    if [ "$1" != "full" ]; then
      additionalOpts="-g '$(${bins.slurp})'"
    else
      shift
    fi

    $SHELL -c "${bins.grim} $additionalOpts $@"
  '';

  defaultFont = builtins.head config.fonts.fontconfig.defaultFonts.monospace;
  defaultEmojiFont = builtins.head config.fonts.fontconfig.defaultFonts.emoji;

  mkDefEnableOption = text: lib.mkEnableOption text // {
    default = true;
    example = false;
  };

in

{
  options = {
    vuizvui.user.sternenseemann.profiles.desktop-sway = {
      enable = lib.mkEnableOption "Sway-based Desktop Profile";
      saneterm.enable = mkDefEnableOption "Keyboard shortcuts for saneterm";
      tep.enable = mkDefEnableOption "Keyboard shortcuts for the tep emoji picker";
      nextcloud.enable = mkDefEnableOption "Automatically launching Nextcloud-Client";
      screenshot.enable = mkDefEnableOption "Keyboard shortcuts for taking screenshots";
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    # Core of the module, always active
    {
      # General prerequisites and general wayland hacks
      hardware.graphics.enable = true;
      # TODO(sterni): no longer valid setting, investigate qt.style
      qt.platformTheme = "gtk";
      # TODO(sterni): reduce this list if possible
      environment.sessionVariables = {
        # firefox screencapture
        MOZ_ENABLE_WAYLAND = "1";
        MOZ_USE_XINPUT2 = "1";
        # SDL
        SDL_VIDEODRIVER = "wayland";
        # QT
        QT_QPA_PLATFORM = "wayland";
        QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
      };
      programs.xwayland.enable = true;

      environment.systemPackages = with pkgs; [
        bemenu                     # better dmenu
        qt5.qtwayland
        wl-clipboard               # instead of xsel
        adwaita-icon-theme
        wdisplays                  # display layout GUI
      ];

      vuizvui.user.sternenseemann.services.mako = {
        enable = true;
        settings =
          let
            inherit (config.vuizvui.user.sternenseemann.services.sway) colors;
          in
          {
            anchor = "bottom-right";
            text-color = colors.activeText;
            background-color = colors.active;
            border-color = colors.inactive;
            outer-margin = "0,0,10";
          };
      };

      vuizvui.user.sternenseemann.services.sway = {
        enable = true;
        additionalBinds = {
          "$mod+c" = "${bins.makoctl} dismiss -a";
        };
        term = bins.foot;
        menu = "${bins.bemenu-run} -l 10 -i";
        lockArgs = [
          "-c" "FFC0CB" "-k" "-l"
        ];
        font = {
          name = defaultFont;
          size = 10;
        };
        colors = {
          background = "#ffffea";
          statusBackground = "#ffffea";
          statusText = "#000000";

          active = "#9EEEEE";
          activeText = "#000000";

          inactive = "#eaffff";
          inactiveText ="#000000";

          urgent = "#900000";
          urgentText = "#ffffff";

          indicate = "#d0d0d0";
        };
      };

      # Start sway via gdm
      services = {
        displayManager = {
          sessionPackages = [
            config.vuizvui.user.sternenseemann.services.sway.package
          ];
        };
        xserver = {
          # otherwise display-manager.service is broken
          enable = true;
          xkb = {
            layout = "de";
            variant = "neo";
          };
          displayManager.gdm = {
            wayland = true;
            enable = true;
          };
        };
      };

      programs.fish.shellAliases = {
        "hdmi2-above" = ''
          sway output HDMI-A-2 position 0 0 && sway output eDP-1 position 0 1440
        '';
        "workspace-move" = "sway move workspace output";
      };

      vuizvui.programs.foot = {
        enable = true;
        settings = {
          main = {
            include = "${config.vuizvui.programs.foot.package.themes}/share/foot/themes/selenized-white";

            dpi-aware = true;
            font = [
              {
                font = defaultFont;
                options = { size = 8; };
              }
              {
                font = defaultEmojiFont;
                options = { size = 8; };
              }
            ];
          };

          tweak = {
            grapheme-shaping = "yes";
          };

          key-bindings = {
            scrollback-up-page = "Control+Shift+Page_Up";
            scrollback-down-page = "Control+Shift+Page_Down";
            search-start = "Control+Shift+F";
            font-increase = "Control+Shift+b";
            font-decrease = "Control+Shift+t";
            font-reset = "Control+Shift+0";
            unicode-input = "none";
            show-urls-launch = "Control+Shift+u";
          };

          mouse-bindings = {
            primary-paste = "none";
          };

          mouse = {
            alternate-scroll-mode = "yes";
          };

          scrollback = {
            lines = 10000;
            indicator-position = "none";
          };
        };
      };
    }

    (lib.mkIf cfg.saneterm.enable {
      vuizvui.user.sternenseemann = {
        programs.saneterm.enable = true;
        services.sway.additionalBinds = {
          "$mod+Shift+Return" = "${bins.saneterm} -- ${bins.sh} -l";
        };
      };
    })

    (lib.mkIf cfg.nextcloud.enable {
      vuizvui.user.sternenseemann.services.sway.extraConfig = ''
        exec ${pkgs.gnome-keyring}/bin/gnome-keyring-daemon --start --components=secrets
        exec ${pkgs.nextcloud-client}/bin/nextcloud
      '';

      environment.systemPackages = [
        pkgs.nextcloud-client
      ];

      # so libsecret works
      services.gnome.gnome-keyring.enable = true;
      programs.dconf.enable = true;

      # for trash:// support in pcmanfm
      services.gvfs.enable = true;
    })

    (lib.mkIf cfg.tep.enable {
      vuizvui.user.sternenseemann.services.sway.additionalBinds = {
        "$mod+g" = "${bins.tep} copy -l 25 -p 'tep>' -i";
      };
    })

    (lib.mkIf cfg.screenshot.enable {
      vuizvui.user.sternenseemann.services.sway.additionalBinds = {
        "$mod+x" = bins.screenshot;
      };
      environment.systemPackages = [
        pkgs.grim pkgs.slurp screenshot      # screenshots
      ];
    })
  ]);
}

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
      // (getBins tep [ "tep" ])
      // (getBins pkgs.grim [ "grim" ])
      // (getBins pkgs.slurp [ "slurp" ])
      // (getBins pkgs.mako [ "makoctl" ])
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
      };

      # notifications
      services.dbus.packages = [ pkgs.mako ];

      environment.systemPackages = with pkgs; [
        bemenu                     # better dmenu
        xwayland qt5.qtwayland
        wl-clipboard               # instead of xsel
        mako                       # notifications
        gnome.adwaita-icon-theme
      ];

      programs.fish.shellAliases = {
        "hdmi2-above" = ''
          sway output HDMI-A-2 position 0 0 && sway output eDP-1 position 0 1440
        '';
        "workspace-move" = "sway move workspace output";
      };

      vuizvui.programs.foot = {
        enable = true;
        settings = {
          include = "${pkgs.foot.themes}/share/foot/themes/selenized-white";

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
        exec ${pkgs.gnome.gnome-keyring}/bin/gnome-keyring-daemon --start --components=secrets
        exec ${pkgs.nextcloud-client}/bin/nextcloud
      '';

      environment.systemPackages = [
        pkgs.nextcloud-client
      ];

      # so libsecret works
      services.gnome.gnome-keyring.enable = true;
      programs.dconf.enable = true;
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

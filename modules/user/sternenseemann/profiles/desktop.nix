# TODO(sterni) icons, GTK
# TODO(sterni) X11 cursor
# TODO(sterni): shrink this module by extracting a niri module
{ config, pkgs, lib, ... }:

let
  cfg = config.vuizvui.user.sternenseemann.profiles.desktop;

  inherit (pkgs.vuizvui.profpatsch)
    getBins
    ;

  inherit (pkgs.vuizvui.sternenseemann)
    tep
    ;

  inherit (pkgs) niri;

  bins = (getBins pkgs.bemenu [ "bemenu" "bemenu-run" ])
      // (getBins tep.wayland [ "tep" ])
      // (getBins config.vuizvui.user.sternenseemann.services.mako.package [ "makoctl" ])
      // (getBins pkgs.foot [ "footclient" ])
      // (getBins config.vuizvui.user.sternenseemann.programs.saneterm.package [ "saneterm" ])
      // (getBins pkgs.bash [ "sh" ])
      // (getBins pkgs.brightnessctl [ "brightnessctl" ])
      // (getBins pkgs.swaylock [ "swaylock" ])
      // (getBins pkgs.wireplumber [ "wpctl" ])
      // (getBins pkgs.niri [ "niri" ])
      // (getBins pkgs.jq [ "jq" ])
      // {
        niri-focus-any-window = pkgs.writeShellScript "niri-focus-any-window" ''
          set -euo pipefail

          # sort to have focused window as the last result
          formatWindows='sort_by(.is_focused) | .[] | "\(.id | tostring):\t\(.title) (\(.app_id))"'

          selection="$(${bins.niri} msg --json windows \
            | ${bins.jq} -r "$formatWindows" \
            | ${bins.bemenu} -p "jump to " -l 25 -i \
            | cut -d: -f1)"

          exec ${bins.niri} msg action focus-window --id "$selection"
        '';
      };

  defaultFont = builtins.head config.fonts.fontconfig.defaultFonts.monospace;
  defaultEmojiFont = builtins.head config.fonts.fontconfig.defaultFonts.emoji;

  mkDefEnableOption = text: lib.mkEnableOption text // {
    default = true;
    example = false;
  };

  # Colors used in acme(1), derived from plan9port
  acmeColors = {
    yellow = "#ffffea";
    # is that even cyan?
    darkCyan = "#9EEEEE";
    lightCyan = "#eaffff";
  };

  niriGaps = 10;
  niriBorder = 2;
in

{
  options = {
    vuizvui.user.sternenseemann.profiles.desktop = {
      enable = lib.mkEnableOption "Desktop Profile";
      saneterm.enable = mkDefEnableOption "Keyboard shortcuts for saneterm";
      tep.enable = mkDefEnableOption "Keyboard shortcuts for the tep emoji picker";
      nextcloud.enable = mkDefEnableOption "Automatically launching Nextcloud-Client";
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    # Core of the module, always active
    {
      # General prerequisites and general wayland hacks
      hardware.graphics.enable = true;
      # TODO(sterni): no longer valid setting, investigate qt.style, font fallback
      qt.platformTheme = "gtk";
      # TODO(sterni): reduce this list if possible
      environment.sessionVariables = {
        # HACK: niri won't detect configuration changes due to the chained
        # symlinks we use, so eliminate one. Note that niri itself unsets this
        # environment variable for some reason.
        # https://github.com/YaLTeR/niri/blob/6ecbf2db8a31484fe88b8faa399b9832da6c8a6a/src/utils/watcher.rs#L38-L45
        NIRI_CONFIG = toString /etc/static/niri/config.kdl;
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
      security.pam.services.swaylock = { };

      systemd.packages = [
        niri
        pkgs.xwayland-satellite
      ];
      environment.systemPackages = with pkgs; [
        niri                       # compositor

        bemenu                     # better dmenu
        qt5.qtwayland
        wl-clipboard               # instead of xsel
        adwaita-icon-theme         # TODO(sterni): do properly
        wdisplays                  # display layout GUI
      ];
      vuizvui.user.sternenseemann.programs.saneterm.enable = cfg.saneterm.enable;

      xdg.portal = {
        enable = true;
        extraPortals = with pkgs; [
          xdg-desktop-portal-gtk
          xdg-desktop-portal-gnome
          # keyring is added via its module
        ];
        # niri's screensharing depends on the GNOME portal
        config.common.default = "gnome";
      };

      systemd.user = {
        # pipewire MUST start before niri, otherwise screen sharing doesn't work
        services.pipewire = {
          wantedBy = [ "niri.service" ];
          before = [ "niri.service" ];
        };

        targets.graphical-session.wants = [
          # niri doesn't implement xwayland itself
          "xwayland-satellite.service"
          "foot-server.socket"
        ];
      };

      environment.etc."niri/config.kdl".text = ''
        input {
          keyboard {
            xkb {
              layout "de(neo),de"
            }
          }

          warp-mouse-to-focus
          focus-follows-mouse max-scroll-amount="0%"
        }

        output "eDP-1" {
          scale 1
          background-color "${acmeColors.yellow}"
        }

        // TODO(sterni): this doesn't work
        environment {
          // assume xwayland-satellite is running
          DISPLAY ":0"
        }

        screenshot-path "~/Pictures/screenshots/%Y%m%d_%Hh%Mm%Ss_niri.png"

        prefer-no-csd
        hotkey-overlay {
            skip-at-startup
        }

      '' + lib.optionalString cfg.nextcloud.enable ''
        // TODO(sterni): can we use systemd?
        spawn-at-startup "${pkgs.gnome-keyring}/bin/gnome-keyring-daemon" "--start" "--components=secrets";
        // TODO(sterni): needs some kind of bar
        spawn-at-startup "${pkgs.nextcloud-client}/bin/nextcloud"
      '' + ''

        layout {
          gaps ${toString niriGaps}
          focus-ring {
            off
          }
          border {
            width ${toString niriBorder}
            active-color "${acmeColors.darkCyan}"
            inactive-color "${acmeColors.lightCyan}"
          }

          preset-column-widths {
              proportion 0.4
              proportion 0.5
              proportion 0.6
              proportion 1.0
          }

          preset-window-heights {
              proportion 0.33333
              proportion 0.5
              proportion 0.66667
              proportion 1.0
          }
        }

        binds {
            Mod+Return { spawn "${bins.footclient}"; }
            // TODO(sterni): colors
            Mod+D { spawn "${bins.bemenu-run}" "-l" "25" "-i"; }
            Super+Alt+L { spawn "${bins.swaylock}" "-k" "-l" "-c" "${acmeColors.yellow}"; }
            Mod+c { spawn "${bins.makoctl}" "dismiss" "-a"; }
      '' + lib.optionalString cfg.saneterm.enable ''
            Mod+Shift+Return { spawn "${bins.saneterm}" "--" "${bins.sh}" "-l"; }
      '' + lib.optionalString cfg.tep.enable ''
            Mod+G { spawn "${bins.tep}" "copy" "-l" "25" "-p" "tep>" "-i"; }
      '' + ''

            XF86AudioRaiseVolume { spawn "${bins.wpctl}" "set-volume" "@DEFAULT_AUDIO_SINK@" "5%+"; }
            XF86AudioLowerVolume { spawn "${bins.wpctl}" "set-volume" "@DEFAULT_AUDIO_SINK@" "5%-"; }
            XF86AudioMute        { spawn "${bins.wpctl}" "set-mute" "@DEFAULT_AUDIO_SINK@" "toggle"; }
            XF86AudioMicMute     { spawn "${bins.wpctl}" "set-mute" "@DEFAULT_AUDIO_SOURCE@" "toggle"; }

            XF86MonBrightnessDown { spawn "${bins.brightnessctl}" "set" "5%-"; }
            XF86MonBrightnessUp   { spawn "${bins.brightnessctl}" "set" "5%+"; }

            Mod+Shift+Q { close-window; }

            Mod+Tab { spawn "${bins.niri-focus-any-window}"; }

            Mod+i { focus-column-left; }
            Mod+e { focus-column-right; }
            Mod+a { focus-window-down; }
            Mod+l { focus-window-up; }
            Mod+u { focus-column-first; }
            Mod+o { focus-column-last; }

            Mod+Shift+i { move-column-left; }
            Mod+Shift+e { move-column-right; }
            Mod+Shift+a { move-window-down; }
            Mod+Shift+l { move-window-up; }

            Mod+Ctrl+i { focus-monitor-left; }
            Mod+Ctrl+a { focus-monitor-down; }
            Mod+Ctrl+l { focus-monitor-up; }
            Mod+Ctrl+e { focus-monitor-right; }

            Mod+Shift+Ctrl+i { move-column-to-monitor-left; }
            Mod+Shift+Ctrl+a { move-column-to-monitor-down; }
            Mod+Shift+Ctrl+l { move-column-to-monitor-up; }
            Mod+Shift+Ctrl+e { move-column-to-monitor-right; }

            Mod+w      { focus-workspace-down; }
            Mod+x      { focus-workspace-up; }
            Mod+Shift+w { move-column-to-workspace-down; }
            Mod+Shift+x { move-column-to-workspace-up; }

            Mod+Ctrl+w { move-workspace-down; }
            Mod+Ctrl+x { move-workspace-up; }

            Mod+WheelScrollDown            { focus-workspace-down; }
            Mod+WheelScrollUp              { focus-workspace-up; }
            Mod+Shift+WheelScrollDown      { focus-column-right; }
            Mod+Shift+WheelScrollUp        { focus-column-left; }

            Mod+1 { focus-workspace 1; }
            Mod+2 { focus-workspace 2; }
            Mod+3 { focus-workspace 3; }
            Mod+4 { focus-workspace 4; }
            Mod+5 { focus-workspace 5; }
            Mod+6 { focus-workspace 6; }
            Mod+7 { focus-workspace 7; }
            Mod+8 { focus-workspace 8; }
            Mod+9 { focus-workspace 9; }
            Mod+Shift+1 { move-column-to-workspace 1; }
            Mod+Shift+2 { move-column-to-workspace 2; }
            Mod+Shift+3 { move-column-to-workspace 3; }
            Mod+Shift+4 { move-column-to-workspace 4; }
            Mod+Shift+5 { move-column-to-workspace 5; }
            Mod+Shift+6 { move-column-to-workspace 6; }
            Mod+Shift+7 { move-column-to-workspace 7; }
            Mod+Shift+8 { move-column-to-workspace 8; }
            Mod+Shift+9 { move-column-to-workspace 9; }

            Mod+Mod5+r { consume-window-into-column; } // Mod+) (paredit slurp)
            Mod+Mod5+e { expel-window-from-column; }   // Mod+} (paredit barf)

            Mod+R { switch-preset-column-width; }
            Mod+Shift+R { switch-preset-window-height; }
            Mod+Ctrl+R { reset-window-height; }
            Mod+F { maximize-column; }
            Mod+Shift+F { fullscreen-window; }

            Mod+Mod5+t { set-column-width "-5%"; } // Mod+t
            Mod+Mod5+b { set-column-width "+5%"; } // Mod++
            Mod+Mod5+Shift+t { set-window-height "-5%"; }
            Mod+Mod5+Shift+b { set-window-height "+5%"; }

            Print { screenshot; }
            Ctrl+Print { screenshot-screen; }
            Alt+Print { screenshot-window; }
        }
      '';

      vuizvui.user.sternenseemann.services.mako = {
        enable = true;
        settings =
          let
            inherit (config.vuizvui.user.sternenseemann.services.sway) colors;
          in
          {
            anchor = "bottom-right";
            text-color = "#000000";
            background-color = acmeColors.darkCyan;
            border-color = acmeColors.lightCyan;
            # top,right,bottom,left; default (inner) margin of 10 is applied on the right
            outer-margin = "0,${toString (niriGaps + niriBorder - 10 + 2)},${toString (niriGaps + niriBorder + 2)},0";
          };
      };

      services = {
        displayManager = {
          sessionPackages = [
            niri
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

    (lib.mkIf cfg.nextcloud.enable {
      environment.systemPackages = [
        pkgs.nextcloud-client
      ];

      # TODO(sterni): move out of conditional or kill conditional

      # so libsecret works
      services.gnome.gnome-keyring.enable = true;
      programs.dconf.enable = true;

      # for trash:// support in pcmanfm
      services.gvfs.enable = true;
    })
  ]);
}

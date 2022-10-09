{ config, pkgs, lib, ... }:

let

  cfg = config.vuizvui.user.sternenseemann.services.sway;

  inherit (pkgs.vuizvui.profpatsch)
    getBins
    ;

  bins = (getBins pkgs.pulseaudio [ "pactl" ])
      // (getBins pkgs.dbus [
        "dbus-update-activation-environment"
      ])
      // (getBins pkgs.systemd [ "systemd-cat" ])
      // (getBins cfg.package [ "sway" ])
      // (getBins pkgs.i3status [ "i3status" ])
      // (getBins pkgs.brightnessctl [ "brightnessctl" ])
      // (getBins pkgs.swaylock [ "swaylock" ])
      ;

  dpiXresources = pkgs.writeText "Xresources" ''
    Xft.dpi: 96
  '';

  workspaceConfig = lib.concatMapStringsSep "\n" (n: ''
    bindsym $mod+${n} workspace number ${n}
    bindsym $mod+Shift+${n} move container to workspace number ${n}
  '') (builtins.map builtins.toString [ 0 1 2 3 4 5 6 7 8 9 ]);

  additionalBindsConfig = lib.concatStringsSep "\n"
    (lib.mapAttrsToList (bind: cmd: ''
      bindsym ${bind} exec ${cmd}
    '') cfg.additionalBinds);

  mkColorOption = name: default: lib.mkOption {
    type = lib.types.str;
    description = "${name} color";
    inherit default;
  };

in {
  options = {
    vuizvui.user.sternenseemann.services.sway = {
      enable = lib.mkEnableOption "sterni's sway";

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.sway.override {
          withBaseWrapper = true;
          withGtkWrapper = true;
        };
        description = "Sway package to use.";
      };

      menu = lib.mkOption {
        type = lib.types.str;
        description = "Menu launcher command to use.";
      };

      term = lib.mkOption {
        type = lib.types.str;
        description = "Terminal emulator command to use.";
      };

      lockArgs = lib.mkOption {
        type = with lib.types; listOf str;
        description = ''
          Arguments to pass to swaylock.
        '';
        default = [];
      };

      additionalBinds = lib.mkOption {
        type = with lib.types; attrsOf str;
        default = {};
        description = ''
          Additional key bindings which are appended
          to the end of the sway config file.
        '';
      };

      colors = {
        background = mkColorOption "background" "#000000";
        active = mkColorOption "active window" "#ffffff";
        inactive = mkColorOption "inactive window" "#000000";
        urgent = mkColorOption "urgent window" "#900000";
        indicate = mkColorOption "indicator" "#d0d0d0";
      };

      font = {
        name = lib.mkOption {
          type = lib.types.str;
          description = ''
            Name of the font to use for bar and window titles.
          '';
          default = "monospace";
        };

        size = lib.mkOption {
          type = lib.types.int;
          description = ''
            Font size to use for bar and window titles.
          '';
          default = 12;
        };
      };

      autolaunchFish = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = ''
          Whether to automatically start sway when logging in on tty1.
        '';
      };

      extraConfig = lib.mkOption {
        type = lib.types.lines;
        default = "";
        description = ''
          Additional lines to append to the configuration.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    programs.fish.loginShellInit = lib.mkIf cfg.autolaunchFish ''
      if test -z "$DISPLAY"; and test -z "$WAYLAND_DISPLAY"; and test (tty) = "/dev/tty1"
        set -x SWAYSOCK "/run/user/"(id -u)"/sway.sock"
        exec ${bins.systemd-cat} ${bins.sway}
      end
    '';

    environment.systemPackages = [
      cfg.package
    ];

    environment.sessionVariables = {
      # firefox screencapture
      XDG_SESSION_TYPE = "wayland";
      XDG_CURRENT_DESKTOP = "sway";
      MOZ_ENABLE_WAYLAND = "1";
      MOZ_USE_XINPUT2 = "1";
      # SDL
      SDL_VIDEODRIVER = "wayland";
      # QT
      QT_QPA_PLATFORM = "wayland";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
      # keyboard layout
      XKB_DEFAULT_LAYOUT = "de";
      XKB_DEFAULT_VARIANT = "neo";
    };

    services.pipewire.enable = true;

    qt5.platformTheme = "gtk";

    xdg.portal = {
      enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
      ];
      gtkUsePortal = true;
    };

    security.pam.services.swaylock = {};

    hardware.opengl.enable = true;

    environment.etc = {
      "sway/config".text = ''
        # correct DPI after hotplugging
        exec ${pkgs.xorg.xrdb}/bin/xrdb -load ${dpiXresources}
        exec ${bins.dbus-update-activation-environment} --all --systemd

        # set the one true modifier
        set $mod Mod4

        # neo arrow keys
        set $left i
        set $right e
        set $up l
        set $down a

        bindsym $mod+Shift+c reload

        bindsym $mod+Shift+q kill

        bindsym $mod+f fullscreen

        bindsym $mod+h splith
        bindsym $mod+v splitv

        bindsym $mod+s layout stacking
        bindsym $mod+t layout tabbed
        bindsym $mod+n layout toggle split

        # focus
        bindsym $mod+Shift+space floating toggle
        bindsym $mod+space focus mode_toggle
        floating_modifier $mod normal

        bindsym $mod+p focus parent

        bindsym $mod+$left focus left
        bindsym $mod+$down focus down
        bindsym $mod+$up focus up
        bindsym $mod+$right focus right

        # moving
        bindsym $mod+Shift+$left move left
        bindsym $mod+Shift+$down move down
        bindsym $mod+Shift+$up move up
        bindsym $mod+Shift+$right move right

        # resizing
        mode "resize" {
          bindsym $left resize shrink width 10px
          bindsym $down resize grow height 10px
          bindsym $up resize shrink height 10px
          bindsym $right resize grow width 10px

          bindsym Return mode "default"
          bindsym Escape mode "default"
        }
        bindsym $mod+r mode "resize"

        ${workspaceConfig}

        bindsym $mod+u exec ${bins.swaylock} ${lib.escapeShellArgs cfg.lockArgs}

        # volume controls
        bindsym XF86AudioRaiseVolume exec ${bins.pactl} set-sink-volume @DEFAULT_SINK@ +5%
        bindsym XF86AudioLowerVolume exec ${bins.pactl} set-sink-volume @DEFAULT_SINK@ -5%
        bindsym XF86AudioMute exec ${bins.pactl} set-sink-mute @DEFAULT_SINK@ toggle
        bindsym XF86AudioMicMute exec ${bins.pactl} set-source-mute @DEFAULT_SOURCE@ toggle

        # brightness controls
        bindsym XF86MonBrightnessDown exec ${bins.brightnessctl} set 5%-
        bindsym XF86MonBrightnessUp exec ${bins.brightnessctl} set +5%

        # standard launch binds
        bindsym $mod+Return exec ${cfg.term}
        bindsym $mod+d exec ${cfg.menu}

        ${additionalBindsConfig}

        # aesthetics
        font "${cfg.font.name} ${builtins.toString cfg.font.size}"

        # colors
        set $bg     ${cfg.colors.background}
        set $act    ${cfg.colors.active}
        set $inact  ${cfg.colors.inactive}
        set $urg    ${cfg.colors.urgent}
        set $ind    ${cfg.colors.indicate}

        output * bg $bg solid_color

        # class                 border  backgr. text    indicator
        client.focused          $act    $act    $inact  $ind
        client.focused_inactive $inact  $inact  $act    $inact
        client.unfocused        $inact  $inact  $act    $inact
        client.urgent           $urg    $urg    $act    $urg

        # bar aesthetics
        bar {
          # TODO(sterni): replace i3status with something better™
          status_command ${bins.i3status}
          position top
          colors {
            font "${cfg.font.name} ${builtins.toString cfg.font.size}"
            statusline $act
            background $inact
            # type             border bg     text
            focused_workspace  $act   $act   $inact
            active_workspace   $act   $act   $ind
            inactive_workspace $inact $inact $act
            urgent_workspace   $urg   $urg   $act
          }
        }

        ${cfg.extraConfig}
      '';

      "xdg/i3status/config".text = ''
        general {
          output_format = "i3bar"
          colors = true
          interval = 1
        }

        order += "volume master"
        order += "battery all"
        order += "tztime kalenderwoche"
        order += "tztime dotdate"
        order += "tztime dottime"
        order += "tztime offset"

        volume master {
          format = "🔊: %volume"
          format_muted = "🔈: %volume"
          device = "pulse"
        }

        battery all {
          format = "%status: %percentage"
          status_chr = "⚡"
          status_bat = "🔋"
          status_unk = "❓"
          status_full = "💯"
          low_threshold = 10
        }

        tztime kalenderwoche {
          timezone = "UTC"
          format = "KW %V"
        }

        tztime dotdate {
          timezone = "UTC"
          format = "%Y-%m-%d"
        }

        tztime dottime {
          timezone = "UTC"
          format = "%H·%M"
        }

        tztime offset {
          format = "%z"
        }
      '';
    };
  };
}

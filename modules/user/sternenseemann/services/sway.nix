# Warning: This module is currently (2024-10-28) unused. It may or may not work.
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
      // (getBins pkgs.systemd [ "systemd-cat" "systemctl" ])
      // (getBins cfg.package [ "sway" ])
      // (getBins pkgs.i3status [ "i3status" ])
      // (getBins pkgs.brightnessctl [ "brightnessctl" ])
      // (getBins pkgs.swaylock [ "swaylock" ])
      ;

  dpiXresources = pkgs.writeText "Xresources" ''
    Xft.dpi: 96
  '';

  workspaceConfig =
    let
      key = n: builtins.substring (builtins.stringLength n - 1) 1 n;
    in

    lib.concatMapStringsSep "\n" (n: ''
      bindsym $mod+${key n} workspace number ${n}
      bindsym $mod+Shift+${key n} move container to workspace number ${n}
    '') (builtins.map builtins.toString (builtins.genList (builtins.add 1) 10));

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

      modifier = lib.mkOption {
        type = lib.types.str;
        default = "Mod4";
        description = "Key to use as the main modifier";
        example = "Mod1";
      };
      additionalBinds = lib.mkOption {
        type = with lib.types; attrsOf str;
        default = {};
        description = ''
          Additional key bindings which are appended
          to the end of the sway config file.
        '';
      };

      # i3 default colors, but less granular and no borders
      colors = {
        background = mkColorOption "background" "#000000";
        statusBackground = mkColorOption "background of status bar" "#000000";
        statusText = mkColorOption "status bar text color" "#ffffff";

        active = mkColorOption "active window" "#285577";
        activeText = mkColorOption "active window text" "#ffffff";

        inactive = mkColorOption "inactive window" "#5f676a";
        inactiveText = mkColorOption "inactive window text" "#ffffff";

        urgent = mkColorOption "urgent window" "#900000";
        urgentText = mkColorOption "urgent window text" "#ffffff";

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

      extraConfig = lib.mkOption {
        type = lib.types.lines;
        default = "";
        description = ''
          Additional lines to append to the configuration.
        '';
      };

      extraStatus = lib.mkOption {
        type = lib.types.attrsOf lib.types.lines;
        default = {};
        description = "Additional i3status entries to prepend to the bar.";
        example = {
          "path_exists LOCK" = ''
            path = "/run/lock"
          '';
        };
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      cfg.package
    ];

    environment.sessionVariables = {
      # keyboard layout
      XKB_DEFAULT_LAYOUT = "de";
      XKB_DEFAULT_VARIANT = "neo";
    };

    xdg.portal = {
      enable = true;
      wlr.enable = true;
      extraPortals = [
        pkgs.xdg-desktop-portal-gtk
      ];
      config.common = {
        default = [
          "wlr"
          "gtk"
        ];
      };
    };

    security.pam.services.swaylock = {};

    # Based on <https://github.com/alebastr/sway-systemd>
    systemd.user.targets = {
      sway-session = {
        description = "Sway Session";
        enable = true;
        bindsTo = [ "graphical-session.target" ];
        wants = [ "graphical-session-pre.target" ];
        after = [ "graphical-session-pre.target" ];
      };

      /* sway-session-shutdown = let
        conflicts = [
          "graphical-session.target"
          "graphical-session-pre.target"
          "sway-session.target"
        ];
      in {
        enable = true;
        description = "Shutdown Sway Session";
        inherit conflicts;
        after = conflicts;
        unitConfig = {
          DefaultDependencies = false;
          StopWhenUnneeded = false;
        };
      }; */
    };

    environment.etc = {
      "sway/config".text = ''
        # correct DPI after hotplugging
        exec ${pkgs.xorg.xrdb}/bin/xrdb -load ${dpiXresources}
        exec ${bins.dbus-update-activation-environment} --all --systemd

        set $mod ${cfg.modifier}

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
        set $bg          ${cfg.colors.background}
        set $act         ${cfg.colors.active}
        set $act_text    ${cfg.colors.activeText}
        set $inact       ${cfg.colors.inactive}
        set $inact_text  ${cfg.colors.inactiveText}
        set $urg         ${cfg.colors.urgent}
        set $urg_text    ${cfg.colors.urgentText}
        set $ind         ${cfg.colors.indicate}

        output * bg $bg solid_color

        # class                 border  backgr. text         indicator
        client.focused          $act    $act    $act_text    $ind
        client.focused_inactive $inact  $inact  $inact_text  $ind
        client.unfocused        $inact  $inact  $inact_text  $ind
        client.urgent           $urg    $urg    $urg_text    $ind

        # bar aesthetics
        bar {
          # TODO(sterni): replace i3status with something better™
          status_command ${bins.i3status}
          position top
          colors {
            font "${cfg.font.name} ${builtins.toString cfg.font.size}"
            statusline ${cfg.colors.statusText}
            background ${cfg.colors.statusBackground}
            # type             border bg     text
            focused_workspace  $act   $act   $act_text
            active_workspace   $act   $act   $act_text
            inactive_workspace $inact $inact $inact_text
            urgent_workspace   $urg   $urg   $urg_text
          }
        }

        ${cfg.extraConfig}

        exec_always ${bins.systemctl} --user start sway-session.target
      '';

      "xdg/i3status/config".text = ''
        general {
          output_format = "i3bar"
          colors = true
          interval = 1
        }

        ${
          lib.concatStrings (
            lib.mapAttrsToList
              (name: _: "order += \"${name}\"\n")
              cfg.extraStatus
          )
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
          status_idle = "💤"
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

        ${
          lib.concatStrings (
            lib.mapAttrsToList (name: config: ''
              ${name} {
                ${builtins.replaceStrings [ "\n" ] [ "\n  " ] config}
              }
            ''
            ) cfg.extraStatus
          )
        }
      '';
    };
  };
}

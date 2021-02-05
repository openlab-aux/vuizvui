# TODO(sterni) icons, GTK
# TODO(sterni) X11 cursor
{ config, pkgs, lib, ... }:

let
  inherit (pkgs.vuizvui.profpatsch)
    getBins
    ;

  inherit (pkgs.writers)
    writeDashBin
    ;

  bins = (getBins pkgs.systemd [ "systemctl" ])
      // (getBins pkgs.xurls [ "xurls" ])
      // (getBins pkgs.bemenu [ "bemenu" ])
      // (getBins pkgs.dbus [ "dbus-run-session" ])
      // (getBins pkgs.sway [ "sway" "swaymsg" ])
      ;

  # manual start script
  startSway = writeDashBin "start-sway" ''
    ${bins.systemctl} --user import-environment
    ${bins.systemctl} --user start sway.service
  '';

  # sway scripts for systemd
  swayService = writeDashBin "sway-service" ''
    ${bins.dbus-run-session} -- "${pkgs.sway}/bin/sway"
  '';

  exitSway = writeDashBin "exit-sway" ''
    ${bins.swaymsg} exit
    ${bins.systemctl} --user unset-environment WAYLAND_DISPLAY
    ${bins.systemctl} --user unset-environment DISPLAY
  '';

  # reduce repition in sway config by generating workspace switches
  wsConfig = lib.concatMapStringsSep "\n" (n: ''
    bindsym $mod+${toString n} workspace number ${toString n}
    bindsym $mod+Shift+${toString n} move container to workspace number ${toString n}
  '') [1 2 3 4 5 6 7 8 9];

  # .Xresources file to fix DPI issues in Xwayland
  xResources = pkgs.writeText "Xresources" ''
    Xft.dpi: 96
  '';
in

{
  imports = [
    ./fonts.nix
  ];

  config = {
    programs.sway = {
      enable = true;
      extraPackages = with pkgs; [
        vuizvui.sternenseemann.tep # emoji picker
        bemenu                     # better dmenu
        xwayland qt5.qtwayland
        wl-clipboard               # instead of xsel
        grim slurp                 # screenshots
        mako                       # notifications
      ];
      wrapperFeatures = {
        gtk = true;
        base = true;
      };
    };

    environment.sessionVariables = {
      # TODO get screen capture working in firefox
      XDG_SESSION_TYPE = "wayland";
      XDG_CURRENT_DESKTOP = "sway";
      MOZ_ENABLE_WAYLAND = "1";
      # SDL
      SDL_VIDEODRIVER = "wayland";
      # QT
      QT_QPA_PLATFORM = "wayland";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
    };

    services.dbus.packages = [ pkgs.mako ];

    services.pipewire.enable = true;
    xdg.portal = {
      enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
      ];
      gtkUsePortal = true;
    };

    environment.etc = {
      "sway/config".text = ''
        # correct DPI after hotplugging
        exec ${pkgs.xorg.xrdb}/bin/xrdb -load ${xResources}

        set $mod Mod4
        #set $term ${pkgs.kitty}/bin/kitty
        set $term ${pkgs.foot}/bin/foot
        set $menu ${pkgs.bemenu}/bin/bemenu-run -l 5 -i
        set $makoctl ${pkgs.mako}/bin/makoctl

        # neo arrow keys
        set $left i
        set $right e
        set $up l
        set $down a

        bindsym $mod+Shift+c reload

        bindsym $mod+Return exec $term
        bindsym $mod+d exec $menu

        bindsym $mod+c exec $makoctl dismiss -a

        bindsym $mod+g exec ${pkgs.vuizvui.sternenseemann.tep}/bin/tep copy -l 25 -p 'tep>' -i

        bindsym $mod+Shift+q kill

        bindsym $mod+h splith
        bindsym $mod+v splitv

        bindsym $mod+s layout stacking
        bindsym $mod+t layout tabbed
        bindsym $mod+n layout toggle split

        bindsym $mod+f fullscreen

        bindsym $mod+Shift+space floating toggle
        bindsym $mod+space focus mode_toggle
        floating_modifier $mod normal

        bindsym $mod+$left focus left
        bindsym $mod+$down focus down
        bindsym $mod+$up focus up
        bindsym $mod+$right focus right

        bindsym $mod+p focus parent

        bindsym $mod+Shift+$left move left
        bindsym $mod+Shift+$down move down
        bindsym $mod+Shift+$up move up
        bindsym $mod+Shift+$right move right

        mode "resize" {
          bindsym $left resize shrink width 10px
          bindsym $down resize grow height 10px
          bindsym $up resize shrink height 10px
          bindsym $right resize grow width 10px

          bindsym Return mode "default"
          bindsym Escape mode "default"
        }
        bindsym $mod+r mode "resize"

        # workspace shortcuts
        ${wsConfig}

        set $pactl ${config.hardware.pulseaudio.package}/bin/pactl
        bindsym XF86AudioRaiseVolume exec $pactl set-sink-volume @DEFAULT_SINK@ +5%
        bindsym XF86AudioLowerVolume exec $pactl set-sink-volume @DEFAULT_SINK@ -5%
        bindsym XF86AudioMute exec $pactl set-sink-mute @DEFAULT_SINK@ toggle
        bindsym XF86AudioMicMute exec $pactl set-source-mute @DEFAULT_SOURCE@ toggle

        set $brightnessctl ${pkgs.brightnessctl}/bin/brightnessctl
        bindsym XF86MonBrightnessDown exec $brightnessctl set 5%-
        bindsym XF86MonBrightnessUp exec $brightnessctl set +5%

        bindsym $mod+0 exec ${pkgs.swaylock}/bin/swaylock -c FFC0CB -k -l

        font "DejaVu Sans Mono normal 10"

        gaps inner 10

        output * bg #000000 solid_color

        # class        border  backgr. text    indicator
        client.focused #ffffff #ffffff #000000 #d0d0d0
        client.focused_inactive #000000 #000000 #ffffff #000000
        client.unfocused #000000 #000000 #ffffff #000000
        client.urgent #900000 #900000 #ffffff #900000

        bar {
          status_command ${pkgs.i3status}/bin/i3status
          position top
          colors {
            font "DejaVu Sans Mono normal 10"
            statusline #ffffff
            background #000000
            focused_workspace #ffffff #ffffff #000000
            active_workspace #ffffff #ffffff #c4c4c4
            inactive_workspace #000000 #000000 #ffffff
            urgent_workspace #900000 #900000 #ffffff
          }
        }
      '';
      "xdg/i3status/config".text = ''
        # TODO replace i3status?
        general {
          output_format = "i3bar"
          colors = true
          interval = 1
        }

        order += "volume master"
        order += "battery all"
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

    vuizvui.programs.foot = {
      enable = true;
      settings = {
        font = [
          "Bitstream Vera Sans Mono"
          {
            font = "Noto Color Emoji";
            options = { size = 8; };
          }
        ];

        key-bindings = {
          scrollback-up-page = "Control+Shift+Page_Up";
          scrollback-down-page = "Control+Shift+Page_Down";
          search-start = "Control+Shift+F";
          font-increase = "Control+Shift+plus";
          font-decrease = "Control+Shift+minus";
          font-reset = "Control+Shift+0";
          pipe-visible = {
            bind = "Control+Shift+e";
            cmd = ''sh -c "${lib.concatStringsSep " | " [
              bins.xurls
              "tac"
              "${bins.bemenu} -l 10"
              "xargs -r $BROWSER"
            ]}"'';
          };
        };

        mouse-bindings = {
          primary-paste = "none";
        };

        mouse = {
          alternate-scroll-mode = "no";
        };

        scrollback = {
          lines = 10000;
          indicator-position = "none";
        };
      };
    };

    environment.variables = {
      XKB_DEFAULT_LAYOUT = "de";
      XKB_DEFAULT_VARIANT = "neo";
      # TODO user env?
      SWAYSOCK = "/run/user/${toString config.users.users.lukas.uid}/sway.sock";
    };

    environment.systemPackages = with pkgs; [
      startSway
      xdg_utils
      networkmanagerapplet # for nm-connection-ediotr
      imv zathura
      foot
      gnome3.nautilus
    ];

    # allow ydotool to use /dev/uinput
    # services.udev.extraRules = ''
    #   KERNEL=="uinput", GROUP:="uinput", MODE:="0660"
    # '';
    # users.groups.uinput = {};
    # users.users.lukas.extraGroups = [ "uinput" ];

    # based on https://nixos.wiki/Sway
    systemd.user = {
      services.sway = {
        bindsTo = [ "graphical-session.target" ];
        wants = [ "graphical-session-pre.target" ];
        after = [ "graphical-session-pre.target" ];
        serviceConfig = {
          Type = "simple";
          ExecStart = "${swayService}/bin/sway-service";
          ExecStop = "${exitSway}/bin/exit-sway";
        };
      };
    };
  };
}

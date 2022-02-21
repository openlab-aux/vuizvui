# TODO(sterni) icons, GTK
# TODO(sterni) X11 cursor
{ config, pkgs, lib, ... }:

let
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
      // (getBins pkgs.dash [ "dash" ])
      // (getBins config.vuizvui.user.sternenseemann.programs.saneterm.package [ "saneterm" ])
      ;

  screenshot = pkgs.writers.writeDashBin "screenshot" ''
    if [ "$1" != "full" ]; then
      additionalOpts="-g '$(${bins.slurp})'"
    else
      shift
    fi

    $SHELL -c "${bins.grim} $additionalOpts $@"
  '';

  defaultFont = "Bitstream Vera Sans Mono";

in

{
  imports = [
    ./fonts.nix
  ];

  config = {
    vuizvui.user.sternenseemann.services.sway = {
      enable = true;
      additionalBinds = {
        "$mod+g" = "${bins.tep} copy -l 25 -p 'tep>' -i";
        "$mod+c" = "${bins.makoctl} dismiss -a";
        "$mod+x" = bins.screenshot;
        "$mod+Shift+Return" = "${bins.saneterm} -- ${bins.dash} -l";
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
      extraConfig = ''
        exec ${pkgs.nextcloud-client}/bin/nextcloud
      '';
    };

    vuizvui.programs.foot = {
      enable = true;
      settings = {
        font = [
          {
            font = defaultFont;
            options = { size = 8; };
          }
          {
            font = "Noto Color Emoji";
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

    vuizvui.user.sternenseemann.programs.saneterm.enable = true;

    # notifications
    services.dbus.packages = [ pkgs.mako ];

    # for nextcloud-client
    services.gnome.gnome-keyring.enable = true;
    programs.dconf.enable = true;

    environment.systemPackages = with pkgs; [
      bemenu                     # better dmenu
      xwayland qt5.qtwayland
      wl-clipboard               # instead of xsel
      grim slurp screenshot      # screenshots
      mako                       # notifications
      nextcloud-client
      gnome3.adwaita-icon-theme
    ];

    programs.fish.shellAliases = {
      "hdmi2-above" = ''
        sway output HDMI-A-2 position 0 0 && sway output eDP-1 position 0 1440
      '';
      "workspace-move" = "sway move workspace output";
    };
  };
}

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

  bins = (getBins pkgs.xurls [ "xurls" ])
      // (getBins pkgs.bemenu [ "bemenu" "bemenu-run" ])
      // (getBins tep [ "tep" ])
      // (getBins pkgs.grim [ "grim" ])
      // (getBins pkgs.slurp [ "slurp" ])
      // (getBins pkgs.mako [ "makoctl" ])
      // (getBins screenshot [ "screenshot" ])
      // (getBins pkgs.foot [ "foot" ])
      ;

  screenshot = pkgs.writers.writeDashBin "screenshot" ''
    if [ "$1" != "full" ]; then
      additionalOpts='-g "$(${bins.slurp})"'
      shift
    fi

    ${bins.grim} $additionalOpts -c -t png
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

        key-bindings = {
          scrollback-up-page = "Control+Shift+Page_Up";
          scrollback-down-page = "Control+Shift+Page_Down";
          search-start = "Control+Shift+F";
          font-increase = "Control+Shift+b";
          font-decrease = "Control+Shift+t";
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

    # notifications
    services.dbus.packages = [ pkgs.mako ];

    environment.systemPackages = with pkgs; [
      bemenu                     # better dmenu
      xwayland qt5.qtwayland
      wl-clipboard               # instead of xsel
      grim slurp screenshot      # screenshots
      mako                       # notifications
    ];
  };
}

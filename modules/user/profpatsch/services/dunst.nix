# dunst notification daemon (user service)
# partially stolen from https://github.com/nix-community/home-manager/blob/9b1b55ba0264a55add4b7b4e022bdc2832b531f6/modules/services/dunst.nix
# but simplified
{ pkgs, lib, config, ... }:

let
  cfg = config.vuizvui.user.profpatsch.services.dunst;

  eitherStrBoolIntList = with lib.types;
    either str (either bool (either int (listOf str)));

  toDunstINI = lib.generators.toINI {
    mkKeyValue = key: value:
      let
        value' = if builtins.isBool value then
          (if value then "yes" else "no")
        else if builtins.isString value then
          ''"${value}"''
        else
          toString value;
      in "${key}=${value'}";
  };


  themeType = lib.types.submodule {
    options = {
      package = lib.mkOption {
        type = lib.types.package;
        example = lib.literalExample "pkgs.gnome3.adwaita-icon-theme";
        description = "Package providing the theme.";
      };

      name = lib.mkOption {
        type = lib.types.str;
        example = "Adwaita";
        description = "The name of the theme within the package.";
      };
    };
  };


  themeSize = "32x32";

in {
  options.vuizvui.user.profpatsch.services.dunst = {
    enable = lib.mkEnableOption "dunst libnotify server";

    settings = lib.mkOption {
      type = with lib.types; attrsOf (attrsOf eitherStrBoolIntList);
      default = { };
      description = ''
        Settings for the dunst daemon which are directly translated
        to the used <literal>dunstrc</literal> config file. See
        <link xlink:href="https://github.com/dunst-project/dunst/blob/v${pkgs.dunst.version}/dunstrc" />
        for a list of available options.
      '';
    };

    iconTheme = lib.mkOption {
      type = themeType;
      description = "Set the icon theme.";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.user.services.dunst = {
      description = "dunst libnotify daemon";
      serviceConfig = {
        Type = "dbus";
        BusName = "org.freedesktop.Notifications";
        ExecStart = "${lib.getBin pkgs.dunst}/bin/dunst -config ${pkgs.writeText "dunst.conf" (toDunstINI cfg.settings)}";
        Restart = "on-failure";
        RestartSec = "1";
      };
      partOf = [ "graphical-session.target" ];
      wantedBy = [ "graphical-session.target" ];
    };


    vuizvui.user.profpatsch.services.dunst.settings = {
      global = {
        icon_position = "left";
        icon_path = let
          theme = cfg.iconTheme;

          categories = [
            "actions"
            "animations"
            "apps"
            "categories"
            "devices"
            "emblems"
            "emotes"
            "filesystem"
            "intl"
            "mimetypes"
            "places"
            "status"
            "stock"
            "legacy"
          ];
        in lib.concatMapStringsSep ":"
            (category: "${cfg.iconTheme.package}/share/icons/${theme.name}/${themeSize}/${category}")
            categories;

        dmenu = "${pkgs.dmenu}/bin/dmenu -p dunst:";
        browser = "xdg-open";
      };
      # TODO: set better urgency colors for low and high
      urgency_low = {
        background = "#F1EAD7";
        foreground = "#504D47";
      };
      urgency_normal = {
        background = "#F1EAD7";
        foreground = "#504D47";
      };
      urgency_high = {
        background = "#F1EAD7";
        foreground = "#504D47";
      };
    };

  };

}

{ config, options, pkgs, lib, ... }:

let
  cfg = config.vuizvui.user.sternenseemann.services.mako;

  # Patch mako to support /etc/xdg
  mako' = pkgs.mako.overrideAttrs (oldAttrs: {
    patches = oldAttrs.patches or [] ++ [
      ./mako-etc-xdg.patch
    ];
  });
in

{
  options.vuizvui.user.sternenseemann.services.mako = {
    enable = lib.mkEnableOption "mako";

    package = lib.mkOption {
      type = lib.types.package;
      # Module doesn't work without this patch
      readOnly = true;
      default = mako';
    };

    settings = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      description = "See {manpage}`mako(5)` for available settings.";
    };
  };

  config = lib.mkIf cfg.enable {
    services.dbus.packages = [ cfg.package ];
    environment = {
      systemPackages = [ cfg.package ];
      etc."xdg/mako/config".text = lib.concatMapStrings (s: s + "\n") (
        lib.mapAttrsToList (n: v: "${n}=${v}") cfg.settings
      );
    };
  };
}

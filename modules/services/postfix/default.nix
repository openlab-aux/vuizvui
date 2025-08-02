{ config, lib, ... }:

with lib;

let
  cfg = config.vuizvui.services.postfix;

  mkRestriction = name: specificDescription: {
    option.${name} = mkOption {
      default = null;
      type = types.nullOr (types.listOf types.str);
      description = ''
        A list of restrictions to apply or `null` to use the
        built-in default value from Postfix.
        ${specificDescription}
      '';
    };

    config = let
      restrictions = cfg.restrictions.${name};
    in mkIf (restrictions != null) {
      services.postfix.extraConfig = ''
        smtpd_${name}_restrictions = ${concatStringsSep ", " restrictions}
      '';
    };
  };

  restrictions = mapAttrsToList mkRestriction {
    client = ''
      SMTP server access restrictions in the context of a client SMTP connection
      request.
    '';
    data = ''
      Access restrictions that the Postfix SMTP server applies in the context of
      the SMTP DATA command.
    '';
    end_of_data = ''
      Access restrictions that the Postfix SMTP server applies in the context of
      the SMTP END-OF-DATA command.
    '';
    etrn = ''
      SMTP server access restrictions in the context of a client ETRN request.
    '';
    helo = ''
      Restrictions that the Postfix SMTP server applies in the context of the
      SMTP HELO command.
    '';
    recipient = ''
      Access restrictions that the Postfix SMTP server applies in the context of
      the RCPT TO command.
    '';
    sender = ''
      Restrictions that the Postfix SMTP server applies in the context of the
      MAIL FROM command.
    '';
  };

in {
  options.vuizvui.services.postfix = {
    enable = mkEnableOption "Vuizvui Postfix";
    restrictions = fold mergeAttrs {} (catAttrs "option" restrictions);
  };

  config = mkIf cfg.enable (mkMerge (catAttrs "config" restrictions));
}

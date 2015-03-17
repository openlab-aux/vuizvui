{ config, lib, ... }:

with lib;

let
  mkRestriction = name: specificDescription: {
    option.${name} = mkOption {
      default = null;
      type = types.nullOr types.list;
      description = ''
        A list of restrictions to apply or <option>null</option> to use the
        built-in default value from Postfix.
        ${specificDescription}
      '';
    };
    config = let
      cfg = config.labernix.postfix.restrictions.${name};
    in mkIf (cfg != null) ''
      smtpd_${name}_restrictions = ${concatStringsSep ", " cfg}
    '';
  };
  restrictions = mapAttrsToList mkRestriction {
    client = mkRestriction ''
      SMTP server access restrictions in the context of a client SMTP connection
      request.
    '';
    data = mkRestriction ''
      Access restrictions that the Postfix SMTP server applies in the context of
      the SMTP DATA command.
    '';
    end_of_data = mkRestriction ''
      Access restrictions that the Postfix SMTP server applies in the context of
      the SMTP END-OF-DATA command.
    '';
    etrn = mkRestriction ''
      SMTP server access restrictions in the context of a client ETRN request.
    '';
    helo = mkRestriction ''
      Restrictions that the Postfix SMTP server applies in the context of the
      SMTP HELO command.
    '';
    recipient = mkRestriction ''
      Access restrictions that the Postfix SMTP server applies in the context of
      the RCPT TO command.
    '';
    sender = mkRestriction ''
      Restrictions that the Postfix SMTP server applies in the context of the
      MAIL FROM command.
    '';
  };
in {
  options.labernix.postfix.restrictions = mapAttrs mkRestriction restrictions;
}

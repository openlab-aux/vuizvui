{ config, pkgs, lib, ... }: let
  vhostMap = {
    smtpd_sender_login_maps = [
      "SELECT username AS allowedUser"
      "FROM mailbox"
      "WHERE username='%s' AND active = 1"
      "UNION SELECT goto FROM alias"
      "WHERE address='%s' AND active = 1"
    ];

    virtual_alias_maps = [
      "SELECT goto"
      "FROM alias"
      "WHERE address='%s' AND active = '1'"
    ];

    virtual_mailbox_domains = [
      "SELECT domain"
      "FROM domain"
      "WHERE domain='%s' AND active = '1'"
    ];

    virtual_mailbox_maps = [
      "SELECT maildir"
      "FROM mailbox"
      "WHERE username='%s' AND active = '1'"
    ];
  };

  mkDbMap = query: "proxy:pgsql:${pkgs.writeText "database.cf" ''
    hosts = localhost
    user = postfix
    dbname = postfix
    query = ${query}
  ''}";

in {
  services.spamassassin.enable = true;

  services.postfix.enable = true;
  services.postfix.hostname = "mailtest.lan";

  # TODO: This is a dummy, replace it once we know about the real root fs.
  fileSystems."/".label = "root";
  boot.loader.grub.device = "nodev";

  vuizvui.services.postfix.enable = true;
  vuizvui.services.postfix.restrictions = {
    sender = [
      "reject_authenticated_sender_login_mismatch"
      "reject_unknown_sender_domain"
    ];
    recipient = [
      "permit_sasl_authenticated"
      "permit_mynetworks"
      "reject_unauth_destination"
      "reject_invalid_hostname"
      "reject_non_fqdn_hostname"
      "reject_non_fqdn_sender"
      "reject_non_fqdn_recipient"
      "reject_unknown_reverse_client_hostname"
    ];
    helo = [
      "permit_sasl_authenticated"
      "permit_mynetworks"
      "reject_invalid_hostname"
      "reject_unauth_pipelining"
      "reject_non_fqdn_hostname"
    ];
  };

  services.postfix.extraConfig = ''
    ${lib.concatStrings (lib.mapAttrsToList (cfgvar: query: ''
      ${cfgvar} = ${mkDbMap (lib.concatStringsSep " " query)}
    '') vhostMap)}

    # a bit more spam protection
    disable_vrfy_command = yes

    smtpd_sasl_type=dovecot
    smtpd_sasl_path=private/auth_dovecot XXXXXXXXXXXXXXX
    smtpd_sasl_auth_enable = yes
    smtpd_sasl_authenticated_header = yes
    broken_sasl_auth_clients = yes

    proxy_read_maps = ${lib.concatStringsSep " " (map (s: "\$${s}") [
      "local_recipient_maps" "mydestination" "virtual_alias_maps"
      "virtual_alias_domains" "virtual_mailbox_maps" "virtual_mailbox_domains"
      "relay_recipient_maps" "relay_domains" "canonical_maps"
      "sender_canonical_maps" "recipient_canonical_maps" "relocated_maps"
      "transport_maps" "mynetworks" "smtpd_sender_login_maps"
    ])}

    local_transport = virtual
    virtual_transport = dovecot

    virtual_uid_maps = static:5000 XXXXXXXXXXXX
    virtual_gid_maps = static:5000 XXXXXXXXXXXX

    smtpd_tls_cert_file=/etc/ssl/mail.crt XXXX: KEYS
    smtpd_tls_key_file=/etc/ssl/mail.key XXXX: KEYS
    smtpd_use_tls=yes
  '';

  services.postfix.extraMasterConf = ''
    mailman unix - n n - - pipe
      flags=FR user=list argv=/usr/lib/mailman/bin/postfix-to-mailman.py ''${nexthop} ''${user}
      # ^^^ FIXME: maybe not needed!

    dovecot unix - n n - - pipe
      flags=DRhu user=vmail:vmail argv=/usr/lib/dovecot/deliver -d ''${recipient}
      # ^^^ FIXME: maybe not needed!

    spamassassin unix - n n - - pipe
      user=${toString config.ids.uids.spamd} argv=${pkgs.spamassassin}/bin/spamc -f -e /var/setuid-wrappers/sendmail -oi -f ''${sender} ''${recipient}
      # ^^^ FIXME: maybe not needed!
  '';
}

home: passwordentry:
{ lib, writeScriptBin, xmpp-client, pass, fetchFromGitHub }:

let
  myClient = xmpp-client.overrideDerivation (old: {
    src = fetchFromGitHub {
      rev = "32cdd273edd354932ce0c5d28d0c4159068bd498";
      owner = "Profpatsch";
      repo = "xmpp-client";
      sha256 = "0ivppc8q2cp0g88dvrlggqipfdz194i7k2irfxq6c0dlzj1638jn";
    };
  });

in
writeScriptBin "xmpp-client" ''
  #!/usr/bin/env bash
  PASS=$(${lib.getBin pass}/bin/pass "${passwordentry}" | head -n1)

  # pipe config with password in fifo
  TMP="$(mktemp -d)/fifo"
  mkfifo "$TMP"
  sed "s/@PASS@/$PASS/" ${home}/.config/xmpp-client/config > "$TMP" &


  # execute the client with logging enabled
  mkdir -p ${home}/.local/share/xmpp-client
  LOG=${home}/.local/share/xmpp-client/history
  CMD="${lib.getBin myClient}/bin/xmpp-client --config-file $TMP"
  script --append --command "$CMD" "$LOG"
''


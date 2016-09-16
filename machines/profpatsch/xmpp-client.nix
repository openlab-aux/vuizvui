home: passwordentry:
{ lib, writeScriptBin, xmpp-client, pass }:

writeScriptBin "xmpp-client" ''
  #!/usr/bin/env bash
  PASS=$(${lib.getBin pass}/bin/pass "${passwordentry}" | head -n1)

  # pipe cofnig with password in fifo
  TMP=$(mktemp)
  mkfifo "$TMP"
  sed "s/@PASS@/$PASS/" ${home}/.config/xmpp-client/config > "$TMP"


  # execute the client with logging enabled
  mkdir -p ${home}/.local/share/xmpp-client
  LOG=${home}/.local/share/xmpp-client/history
  CMD="${lib.getBin xmpp-client}/bin/xmpp-client --config-file $TMP"
  script --append --command "$CMD" "$LOG"
''


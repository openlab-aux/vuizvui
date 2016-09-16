home: passwordentry:
{ lib, writeScriptBin, xmpp-client, pass }:

writeScriptBin "xmpp-client" ''
  #!/usr/bin/env bash
  PASS=$(${lib.getBin pass}/bin/pass "${passwordentry}" | head -n1)
  ${lib.getBin xmpp-client}/bin/xmpp-client \
    --config-file <(sed "s/@PASS@/$PASS/" ${home}/.config/xmpp-client/config)
''


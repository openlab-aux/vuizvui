home: passwordentry:
{ lib, writeScriptBin, xmpp-client, pass, fetchFromGitHub }:

let
  myClient = xmpp-client.overrideDerivation (old: {
    src = fetchFromGitHub {
      rev = "785ad1629930bab03a73cc858951db1f78156743";
      owner = "Profpatsch";
      repo = "xmpp-client";
      sha256 = "02nlx5kx0s1rz9rsyncgi9hmb62i1pl322a91ama4sm00qbi4fs7";
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


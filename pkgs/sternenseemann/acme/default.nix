{ lib
, runCommand
, plan9port
, makeWrapper
}:

runCommand "acme-${plan9port.version}" {
  pname = "acme";
  inherit (plan9port) version;

  nativeBuildInputs = [
    makeWrapper
  ];

  meta = plan9port.meta // {
    description = "Standalone version of Plan 9 from Userspace's acme";
  };
} ''
  for cmd in 9 9pserve acme devdraw win; do
    makeWrapper "${plan9port}/plan9/bin/$cmd" "$out/bin/$cmd" --prefix PATH : "$out/bin"
  done
''

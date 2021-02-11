{ lib, runCommand, go }:

runCommand "nman" {
  meta = with lib; {
    description = "Invoke manpage in temporary nix-shell";
    license = licenses.gpl3;
  };
} ''
    mkdir cache
    env GOCACHE="$PWD/cache" \
      ${lib.getBin go}/bin/go build -o nman ${./nman.go}
    install -D nman $out/bin/nman
''


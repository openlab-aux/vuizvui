{ lib, runCommandNoCC, go }:

runCommandNoCC "nman" {
  meta = with lib; {
    description = "Invoke manpage in temporary nix-shell";
    license = licenses.gpl3;
  };
} ''
    ${lib.getBin go}/bin/go build -o nman ${./nman.go}
    install -D nman $out/bin/nman
''


{ lib, runCommandCC }:

runCommandCC "nman" {
  meta = with lib; {
    description = "Invoke manpage in temporary nix-shell";
    license = licenses.gpl3;
  };
} ''
    cc -o nman ${./nman.c}
    install -D nman $out/bin/nman
''


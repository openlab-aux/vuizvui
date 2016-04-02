let
  pkgs = import <nixpkgs> {};
in

  pkgs.runCommand "gpg-agent-wrapper" {
    buildInputs = with pkgs; [ pkgconfig systemd ];
  } ''
    cc -Wall -shared -std=c11 \
      $(pkg-config --cflags --libs libsystemd) \
      "${./agent-wrapper.c}" -o "$out" -ldl -fPIC
  ''

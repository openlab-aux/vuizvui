let pkgs = import <nixpkgs> {};
    simple = import (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-dhall-nix";
      rev = "11f6eecab5c276a59858a10bbfcbbc5611187da03";
      sha256 = "02f5723rx4q4b53dbckmc7mgzfc1m27xbh1m844dhlkklwb5jydp";
    }) {};

  dhall-to-shell = pkgs.writers.writeBashBin "dhall-to-shell" ''
    ${simple.dhall-json-simple}/bin/dhall-to-json \
      | ${pkgs.jq}/bin/jq -r 'map(@sh) | join("\n")'
  '';

in
  pkgs.mkShell {
    name = "dhallsh";
    buildInputs = [
      dhall-to-shell
      simple.dhall-simple
    ];
  }

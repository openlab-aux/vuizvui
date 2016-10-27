{ stdenv, fetchFromGitHub, fetchpatch, git }:

let
  rev = "06e3a4b961b80ada9293632d0ec62493c0740282";
  sha256 = "0w2b1m9z42g56mw2g2sw5izb7gn0nhlp3v8c96m65r121gdil9fj";

  master = stdenv.mkDerivation rec {
    name = "nixops-upstream-patched";

    src = fetchFromGitHub {
      owner = "NixOS";
      repo = "nixops";
      inherit rev sha256;
    };

    phases = [ "unpackPhase" "patchPhase" "installPhase" ];

    patches = stdenv.lib.singleton (fetchpatch {
      url = "https://github.com/NixOS/nixops/pull/508.patch";
      sha256 = "1x0cjjpw7ykavnkz1ndxlkcymp9vx6lkyqhbf9wz5jvplqhpb9z7";
    });

    postPatch = ''
      sed -i -re 's!<nixpkgs([^>]*)>!${import ../../nixpkgs-path.nix}\1!g' \
        release.nix doc/manual/default.nix doc/manual/resource.nix
      sed -i -e 's/\<sqlite3\>//' release.nix
    '';

    installPhase = ''
      cp -a . "$out"
    '';
  };

  release = import "${master}/release.nix" {
    nixopsSrc = {
      outPath = master;
      inherit rev;
      revCount = 0;
      shortRev = builtins.substring 0 7 rev;
    };
    officialRelease = false;
  };
in stdenv.lib.getAttr stdenv.system release.build

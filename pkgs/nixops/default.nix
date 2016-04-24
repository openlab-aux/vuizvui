{ stdenv, fetchFromGitHub, fetchpatch, git }:

let
  rev = "7c4659663722c05959f74db45e85073759a0b5ee";
  sha256 = "026jsp34fw199saqcfrixhcv53wkpc5y0vn57qx9wdyygjasibf8";

  master = stdenv.mkDerivation rec {
    name = "nixops-upstream-patched";

    src = fetchFromGitHub {
      owner = "NixOS";
      repo = "nixops";
      inherit rev sha256;
    };

    phases = [ "unpackPhase" "patchPhase" "installPhase" ];

    postPatch = ''
      sed -i -re 's!<nixpkgs([^>]*)>!${import ../../nixpkgs-path.nix}\1!g' \
        release.nix doc/manual/default.nix doc/manual/resource.nix
      # XXX: Hack to get it to build for now.
      sed -i -e 's/pkgs\.libxslt/pkgs.libxslt.bin/g' \
        doc/manual/default.nix doc/manual/resource.nix
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

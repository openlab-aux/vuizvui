{ stdenv, fetchFromGitHub, fetchpatch, git }:

let
  rev = "d1907cb1a4defa1d01cd7c2d18e09c9276e716e5";
  sha256 = "0842qdq1497x0pk4ifhmrg3m5icxan6y51zqw6by5qiy3pyc379c";

  master = stdenv.mkDerivation rec {
    name = "nixops-upstream-patched";

    src = fetchFromGitHub {
      owner = "NixOS";
      repo = "nixops";
      inherit rev sha256;
    };

    phases = [ "unpackPhase" "patchPhase" "installPhase" ];

    postPatch = ''
      sed -i -re 's!<nixpkgs([^>]*)>!${import ../../../nixpkgs-path.nix}\1!g' \
        release.nix doc/manual/default.nix doc/manual/resource.nix
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

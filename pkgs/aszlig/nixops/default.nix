{ stdenv, fetchFromGitHub, fetchpatch, git }:

let
  rev = "bbf9a792d06c9a60c74dabe2937a9dfda9bff8f7";
  sha256 = "0a1mx0ngp0zg65r1rx99rina4wbfjyzrziw2z9788v629j58p4jd";

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

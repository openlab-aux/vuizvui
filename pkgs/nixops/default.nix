{ stdenv, fetchFromGitHub, fetchpatch, git }:

let
  rev = "b66db0aa1c2bea64fe821ff67d51555c85943572";
  sha256 = "15wwhah9ghwn7w3nq0jj0vqyz2al76w3vwfnl4aklbffmj2jbzwy";

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

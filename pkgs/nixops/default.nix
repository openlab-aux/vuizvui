{ stdenv, fetchFromGitHub, fetchpatch, git }:

let
  rev = "a932a9ecec15336a55f2aa2c2a20f08264adb837";
  sha256 = "1gyaammwzl4jvi00wg7bis9v9grviiqc59ayf39ap1xmp4767zdj";

  master = stdenv.mkDerivation rec {
    name = "nixops-upstream-patched";

    src = fetchFromGitHub {
      owner = "NixOS";
      repo = "nixops";
      inherit rev sha256;
    };

    phases = [ "unpackPhase" "patchPhase" "installPhase" ];

    patches = [
      (fetchpatch {
        url = "https://github.com/NixOS/nixops/pull/201.diff";
        sha256 = "1i5yycqayxggg3l1i6wk8lp64lqlxw5nmfya9fcrgmck8ls0rxid";
      })
      (fetchpatch rec {
        name = "read-write-by-default.diff";
        url = "https://github.com/aszlig/nixops/compare/"
            + "NixOS:master...aszlig:${name}";
        sha256 = "0a1jcqrqfi7dfvlha5r0609bzvin7p7nj523xxcrvwpgp6ag0zsa";
      })
    ];

    patchFlags = "--merge -p1";

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

{ stdenv, fetchFromGitHub, fetchpatch, git }:

let
  rev = "9d7fbce08380107d3ff6e2546add817b4ac40ee0";
  sha256 = "167silv9p27gayrlrzpm88rj60gj3hlxhkhnsp4ccpbvq6yw1wr3";

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

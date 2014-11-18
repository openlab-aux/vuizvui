{ stdenv, runCommand, fetchgit, fetchpatch, git }:

let
  mkRaw = cmd: import (runCommand "genraw.nix" {} ''
    (${cmd}) > "$out"
  '');

  mkString = cmd: import (runCommand "genstring.nix" {} ''
    echo "\"$(${cmd})\"" > "$out"
  '');

  master = stdenv.mkDerivation rec {
    name = "nixops-upstream-patched";

    src = fetchgit {
      url = "git://github.com/NixOS/nixops.git";
      rev = "dd589dcf54f2e9530ec4ee4cd45a8a6474d7ce46";
      sha256 = "0m5rxkggf4rxxwzk9bhffirgla8xjn0vacb8jjh751sn92h07az7";
      leaveDotGit = true;
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
      sed -i -e '/git ls-files/d' release.nix
    '';

    installPhase = ''
      cp -a . "$out"
    '';
  };

  release = import "${master}/release.nix" {
    nixopsSrc = {
      outPath = master;
      inherit (master.src) rev;
      revCount = mkRaw ''
        # FIXME: It's a shallow clone, so we always get 1.
        ${git}/bin/git -C "${master}" rev-list --count HEAD
      '';

      shortRev = mkString ''
        ${git}/bin/git -C "${master}" rev-parse --short HEAD
      '';
    };
    officialRelease = false;
  };
in stdenv.lib.getAttr stdenv.system release.build

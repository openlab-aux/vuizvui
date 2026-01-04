{ homeRepoSrc ? pkgs.fetchgit {
    url = "https://codeberg.org/Profpatsch/Profpatsch";
    rev = "2582dbe90bf8088928835a86bc98610b49a96a19"; # 2025-11-05
    sha256 = "sha256-tYKIihnYpyMP+31A360VbocyLL+EWPZDf4l9MTWTTn4=";
  }
, pkgs
}:

import homeRepoSrc {
  nixpkgsBisectPath = pkgs.path; # TODO: does this improve eval time significantly?
  localSystem =
    assert pkgs.stdenv.hostPlatform == pkgs.stdenv.buildPlatform;
    pkgs.stdenv.buildPlatform.system;
}

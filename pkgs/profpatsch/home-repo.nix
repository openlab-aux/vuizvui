{ homeRepoSrc ? pkgs.fetchgit {
    url = "https://codeberg.org/Profpatsch/Profpatsch";
    rev = "5d3104861c2794ad73abc113d1d1b9e8ede66f63"; # 2025-10-16
    sha256 = "0i56ia6khaj60la29cjr6cdc5dss9dd0ak610j7i69dbq9b40hk6";
  }
, pkgs
}:

import homeRepoSrc {
  nixpkgsBisectPath = pkgs.path; # TODO: does this improve eval time significantly?
  localSystem =
    assert pkgs.stdenv.hostPlatform == pkgs.stdenv.buildPlatform;
    pkgs.stdenv.buildPlatform.system;
}

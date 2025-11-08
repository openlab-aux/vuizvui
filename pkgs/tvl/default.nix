{ tvlSrc ? builtins.fetchGit {
    name = "tvl-depot";
    url = "https://code.tvl.fyi";
    rev = "52cdd1787163ce55c0668371c0e5f97b60c307c1"; # 2025-11-07
    ref = "canon";
  }
, pkgs
}:

import tvlSrc {
  nixpkgsBisectPath = pkgs.path; # TODO: does this improve eval time significantly?
  localSystem =
    assert pkgs.stdenv.hostPlatform == pkgs.stdenv.buildPlatform;
    pkgs.stdenv.buildPlatform.system;
}

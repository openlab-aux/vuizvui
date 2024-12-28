{ tvlSrc ? builtins.fetchGit {
    name = "tvl-depot";
    url = "https://code.tvl.fyi";
    rev = "22023fdc8d3e5ade6306fdab4dba5503dd40dc27"; # 2024-12-26
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

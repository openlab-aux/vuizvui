{ tvlSrc ? builtins.fetchGit {
    name = "tvl-depot";
    url = "https://code.tvl.fyi";
    rev = "8b880b4a5052a7deaf8663136ac6e436aefa64e8"; # 2022-10-06
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

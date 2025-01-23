{ tvlSrc ? builtins.fetchGit {
    name = "tvl-depot";
    url = "https://code.tvl.fyi";
    rev = "26edf809ffac167165fa6d29e82c3646564789db"; # 2025-01-23
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

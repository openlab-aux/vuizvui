{ tvlSrc ? builtins.fetchGit {
    name = "tvl-depot";
    url = "https://code.tvl.fyi";
    rev = "98ea614c6e9ea9428dc10dde630abf894bd01473"; # 2023-02-02
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

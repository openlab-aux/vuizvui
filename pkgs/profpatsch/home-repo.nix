{ homeRepoSrc ? pkgs.fetchgit {
    url = "https://codeberg.org/Profpatsch/Profpatsch";
    rev = "2659b58d5e52b80364feef89ded9aad65eed881b"; # 2026-03-18
    sha256 = "sha256-mJ2nXJhpq0KWypaC0u9iLeikgX9VmE6pBEt4cPV0AhA=";
  }
, pkgs
}:

import homeRepoSrc {
  nixpkgsBisectPath = pkgs.path; # TODO: does this improve eval time significantly?
  localSystem =
    assert pkgs.stdenv.hostPlatform == pkgs.stdenv.buildPlatform;
    pkgs.stdenv.buildPlatform.system;
}

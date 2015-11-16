{ goPackages, lib, fetchFromGitHub, fetchhg, fetchgit }:

goPackages.buildGoPackage {
  name = "beehive";
  goPackagePath = "github.com/muesli/beehive";
  src = fetchFromGitHub {
    owner = "muesli";
    repo = "beehive";
    rev = "74a7fc4927b8ef14b199254e04630c24f44429f7";
    sha256 = "1clgc6245yb3yxqdc14xj0f8hc8v4b9hgkv22c89zp0n1by8xrqx";
  };
  buildInputs = lib.mapAttrsToList (name: val: val) (import ./godeps.nix {
    inherit (goPackages) buildGoPackage;
    inherit lib fetchFromGitHub fetchhg fetchgit;
  });
  meta.broken = true;
}

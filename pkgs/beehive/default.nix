{ goPackages, lib, fetchFromGitHub, fetchhg, fetchgit }:

goPackages.buildGoPackage {
  name = "beehive";
  goPackagePath = "github.com/muesli/beehive";
  src = fetchFromGitHub {
    owner = "muesli";
    repo = "beehive";
    rev = "6a90eb15c16aaf8e35a28d28e86513701a330f34";
    sha256 = "18vz1s1fv1yf2i4yml6sxasiq1iyiq29yj52pjng7sxqx1ps9rcb";
  };
  buildInputs = lib.mapAttrsToList (name: val: val) (import ./godeps.nix {
    inherit (goPackages) buildGoPackage;
    inherit lib fetchFromGitHub fetchhg fetchgit;
  });
}

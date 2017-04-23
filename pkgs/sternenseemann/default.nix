{ haskellPackages, ocamlPackages_4_02 }:

{
  spacecookie = haskellPackages.callPackage ./spacecookie {};
  logbook = ocamlPackages_4_02.callPackage ./logbook {};
}

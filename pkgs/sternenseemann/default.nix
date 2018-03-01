{ haskellPackages, ocamlPackages }:

{
  spacecookie = haskellPackages.callPackage ./spacecookie {};
  logbook = ocamlPackages.callPackage ./logbook {};
}

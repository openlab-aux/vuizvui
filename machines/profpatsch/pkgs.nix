{ pkgs }:

let
  addRuntimeDeps = drv: ds: drv.overrideDerivation (old: {
    propagatedNativeBuildInputs = old.propagatedNativeBuildInputs ++ ds;
  });

in
with pkgs;
{

  offlineimap = addRuntimeDeps offlineimap [ pythonPackages.pygpgme ];

}

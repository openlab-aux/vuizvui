{ pkgs }:
{

  offlineimap =  with pkgs; offlineimap.overrideDerivation (old: {
    propagatedNativeBuildInputs = old.propagatedNativeBuildInputs ++ [ pythonPackages.pygpgme ];
  });

}

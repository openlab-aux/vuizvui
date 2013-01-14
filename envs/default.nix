pkgs:

with pkgs;

{
  aszligEnv = buildEnv {
    name = "aszlig-env";
    paths = let
      genAszligEnv = name: paths: buildEnv {
        name = "aszlig-${name}-packages";
        inherit paths;
        ignoreCollisions = true;
      };
      collection = import ../collections/aszlig.nix pkgs;
    in lib.mapAttrsToList genAszligEnv collection;
  };
}

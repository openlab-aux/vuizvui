pkgs:

{
  aszligEnv = (pkgs.buildEnv {
    name = "aszlig-env";
    paths = let
      genAszligEnv = name: paths: pkgs.buildEnv {
        name = "aszlig-${name}-packages";
        inherit paths;
        ignoreCollisions = true;
      };
      collection = import ../collections/aszlig.nix pkgs;
    in pkgs.lib.mapAttrsToList genAszligEnv collection;
  });
}

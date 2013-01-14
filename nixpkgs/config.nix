let
  aszligCollections = pkgs: let
    genAszligEnv = name: paths: pkgs.buildEnv {
      name = "aszlig-${name}-packages";
      inherit paths;
      ignoreCollisions = true;
    };
    collection = import ../collections/aszlig.nix pkgs;
  in pkgs.lib.mapAttrsToList genAszligEnv collection;
in {
  pulseaudio = true;
  chromium.enableGoogleTalkPlugin = true;
  chromium.jre = true;

  firefox.jre = true;

  packageOverrides = import ../overrides;
}

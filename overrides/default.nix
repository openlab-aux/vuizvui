pkgs:

with pkgs.lib;

let
  allPackages = (import ../pkgs { pkgs = everything; }) // misc;
  everything = pkgs // allPackages // drvOverrides // argOverrides;

  mapOverride = overrideFun: includePackages: let
    packages = pkgs // allPackages // includePackages;
    overrideName = name: overrideFun (getAttr name packages);
  in mapAttrs overrideName;

  # input attrset overrides using pkg.override
  argOverrides = mapOverride (getAttr "override") drvOverrides {
    netrw.checksumType = "mhash";
    pulseaudio.useSystemd = true;
    w3m.graphicsSupport = true;
    uqm.use3DOVideos = true;
    uqm.useRemixPacks = true;
    miro.enableBonjour = true;
  };

  # derivation overrides
  drvOverrides = mapOverride overrideDerivation argOverrides {};

  # misc
  misc = {};
in allPackages // drvOverrides // argOverrides

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
  drvOverrides = mapOverride overrideDerivation argOverrides {
    mpv = o: {
      installPhase = o.installPhase + ''
        cat > "$out/etc/mpv/mpv.conf" <<CONFIG
        ao=pulse
        CONFIG
      '';
    };
  };

  # misc
  misc = {
    kernelSourceVuizvui = {
      version = "4.0.0-rc3";
      src = everything.fetchgit {
        url = git://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git;
        rev = "affb8172de395a6e1db52ed9790ca0456d8c29a9";
        sha256 = "1zqnkds0mglldm1syv17gl8n4wl1vy7rrh2vf3iga5w2psqwkyj4";
      };
    };
  };
in allPackages // drvOverrides // argOverrides

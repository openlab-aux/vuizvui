pkgs:

with pkgs.lib;

let
  allPackages = newPackages // misc;
  everything = pkgs // allPackages // drvOverrides // argOverrides;

  callPackage = callPackageWith everything;

  mapOverride = overrideFun: includePackages: let
    packages = pkgs // allPackages // includePackages;
    overrideName = name: overrideFun (getAttr name packages);
  in mapAttrs overrideName;

  # input attrset overrides using pkg.override
  argOverrides = mapOverride (getAttr "override") drvOverrides {
    netrw.checksumType = "mhash";
    pulseaudio.useSystemd = true;
    w3m.graphicsSupport = true;
  };

  # derivation overrides
  drvOverrides = let
    tkabberRev = 2009;
  in mapOverride overrideDerivation argOverrides {
    tkabber = o: {
      name = "tkabber-1.0pre";
      src = everything.fetchsvn {
        url = "http://svn.xmpp.ru/repos/tkabber/trunk/tkabber";
        rev = tkabberRev;
        sha256 = "0lfh3bapqsfw142bndp11x7cs9crrcccw242lgwlh103r9gs123s";
      };
    };

    tkabber_plugins = o: {
      name = "tkabber-plugins-1.0pre";
      src = everything.fetchsvn {
        url = "http://svn.xmpp.ru/repos/tkabber/trunk/tkabber-plugins";
        rev = tkabberRev;
        sha256 = "181jxd7iwpcl7wllwciqshzznahdw69fy7r604gj4m2kq6qmynqf";
      };
    };
  };

  # new packages
  newPackages = {
    axbo = callPackage ./axbo { };
    blop = callPackage ./blop { };
    libCMT = callPackage ./libcmt { };
    librxtx_java = callPackage ./librxtx-java { };
    pvolctrl = callPackage ./pvolctrl { };
  };

  # misc
  misc = {
    kernelSourceAszlig = {
      version = "3.8.0-rc5";
      src = everything.fetchgit {
        url = /home/aszlig/linux;
        rev = "949db153b6466c6f7cad5a427ecea94985927311";
        sha256 = "1klcy21gz42lfqvcydm6lyiwylp43714pzlgxrnzhyvh6df7r0k4";
      };
    };

    testChromiumBuild = let
      buildChannels = [ "stable" "beta" "dev" ];
      buildChromium = chan: everything.chromium.override {
        channel = chan;
        gnomeSupport = true;
        gnomeKeyringSupport = true;
        proprietaryCodecs = true;
        cupsSupport = true;
        pulseSupport = true;
      };
    in everything.stdenv.mkDerivation {
      name = "test-chromium-build";

      buildCommand = let
        chanResults = flip map buildChannels (chan: ''
          echo "Build result for ${chan}: ${buildChromium chan}"
        '');
      in ''
        echo "Builds finished, the following derivations have been built:"
        ${concatStrings chanResults}
        false
      '';
    };
  };
in allPackages // drvOverrides // argOverrides

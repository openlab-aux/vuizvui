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
      version = "3.8.0-rc3";
      src = everything.fetchgit {
        url = /home/aszlig/linux;
        rev = "ecf02a607bd801e742d7bb35c6e40f7ca15edf03";
        sha256 = "19ad666ms61s55730sh2n2lp9dsn439lnr7cypdma978mx27aj7v";
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

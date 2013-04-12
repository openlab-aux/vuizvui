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
    tkabberRev = 2010;
  in mapOverride overrideDerivation argOverrides {
    tkabber = o: {
      name = "tkabber-1.0pre";
      src = everything.fetchsvn {
        url = "http://svn.xmpp.ru/repos/tkabber/trunk/tkabber";
        rev = tkabberRev;
        sha256 = "0ixvp3frpx7zhb7jyi0w463n78kafdzgmspkm2jhh6x28rimj0lz";
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
    tkabber_urgent_plugin = callPackage ./tkabber-urgent-plugin { };
  };

  # misc
  misc = {
    kernelSourceAszlig = {
      version = "3.9.0-pre-rc1";
      src = everything.fetchgit {
        url = /home/aszlig/linux;
        rev = "9811cc24e6aa037ce315729a9a81fb46a29c6e7f";
        sha256 = "05d25fa36jkd3hf6nh3rjmyhhi8mwdsb3g1rh3fclh7rnan8jxqa";
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

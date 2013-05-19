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
    uqm.use3DOVideos = true;
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
    fish = callPackage ./fish { };
    libCMT = callPackage ./libcmt { };
    librxtx_java = callPackage ./librxtx-java { };
    lockdev = callPackage ./lockdev { };
    pvolctrl = callPackage ./pvolctrl { };
    tkabber_urgent_plugin = callPackage ./tkabber-urgent-plugin { };
  };

  # misc
  misc = {
    kernelSourceAszlig = {
      version = "3.10.0-rc1";
      src = everything.fetchgit {
        url = /home/aszlig/linux;
        rev = "d5fe85af85367d5892e4308f596de4e2a5fb9017";
        sha256 = "0kpnh0hj4x07dp565mki4slk0sdycph6sd4ijxbpa8xq2pmwsan6";
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
      mkTest = chan: everything.writeScript "test-chromium-${chan}.sh" ''
        #!${everything.stdenv.shell}
        if datadir="$(${everything.coreutils}/bin/mktemp -d)"; then
          ${buildChromium chan}/bin/chromium --user-data-dir="$datadir"
          rm -rf "$datadir"
        fi
      '';
    in everything.stdenv.mkDerivation {
      name = "test-chromium-build";

      buildCommand = let
        chanResults = flip map buildChannels (chan: ''
          echo "Test script for ${chan}: ${mkTest chan}"
        '');
      in ''
        echo "Builds finished, the following derivations have been built:"
        ${concatStrings chanResults}
        false
      '';
    };
  };
in allPackages // drvOverrides // argOverrides

pkgs:

with {
  tkabberRev = 2009;
};
with pkgs;

let
  callPackage = lib.callPackageWith (pkgs // self);
  self = rec {
    pulseaudio = pkgs.pulseaudio.override {
      useSystemd = true;
    };

    kernelSourceAszlig = {
      version = "3.8.0-rc3";
      src = pkgs.fetchgit {
        url = /home/aszlig/linux;
        rev = "ecf02a607bd801e742d7bb35c6e40f7ca15edf03";
        sha256 = "19ad666ms61s55730sh2n2lp9dsn439lnr7cypdma978mx27aj7v";
      };
    };

    blop = callPackage ./blop { };

    libCMT = callPackage ./libcmt { };

    tkabber = lib.overrideDerivation pkgs.tkabber (o: {
      name = "tkabber-1.0pre";
      src = fetchsvn {
        url = "http://svn.xmpp.ru/repos/tkabber/trunk/tkabber";
        rev = tkabberRev;
        sha256 = "0lfh3bapqsfw142bndp11x7cs9crrcccw242lgwlh103r9gs123s";
      };
    });

    tkabber_plugins = lib.overrideDerivation pkgs.tkabber_plugins (o: {
      name = "tkabber-plugins-1.0pre";
      src = fetchsvn {
        url = "http://svn.xmpp.ru/repos/tkabber/trunk/tkabber-plugins";
        rev = tkabberRev;
        sha256 = "181jxd7iwpcl7wllwciqshzznahdw69fy7r604gj4m2kq6qmynqf";
      };
    });

    w3m = pkgs.w3m.override {
      graphicsSupport = true;
    };

    netrw = pkgs.netrw.override {
      checksumType = "mhash";
    };

    testChromiumBuild = let
      buildChannels = [ "stable" "beta" "dev" ];
      buildChromium = chan: chromium.override {
        channel = chan;
        gnomeSupport = true;
        gnomeKeyringSupport = true;
        proprietaryCodecs = true;
        cupsSupport = true;
        pulseSupport = true;
      };
    in stdenv.mkDerivation {
      name = "test-chromium-build";

      buildCommand = let
        chanResults = lib.flip map buildChannels (chan: ''
          echo "Build result for ${chan}: ${buildChromium chan}"
        '');
      in ''
        echo "Builds finished, the following derivations have been built:"
        ${lib.concatStrings chanResults}
        false
      '';
    };

    pvolctrl = callPackage ./pvolctrl { };

    librxtx_java = callPackage ./librxtx-java { };

    axbo = callPackage ./axbo { };
  };
in self

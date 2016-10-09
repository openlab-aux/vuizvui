{ pkgs ? import (import ../nixpkgs-path.nix) {} }:

let
  callPackage = pkgs.lib.callPackageWith (pkgs // self.vuizvui);

  self.vuizvui = {
    mkChannel = callPackage ./build-support/channel.nix { };

    aacolorize = callPackage ./aacolorize { };
    axbo = callPackage ./axbo { };
    # beehive = callPackage ./beehive { }; TODO get running again
    blop = callPackage ./blop { };
    git-detach = callPackage ./git-detach { };
    grandpa = callPackage ./grandpa { };
    greybird-xfce-theme = callPackage ./greybird-xfce-theme { };
    # TODO: Profpatsch needs to include these files!
    #iec16022 = callPackage ./iec16022 { };
    #multi-iec16022 = callPackage ./multi_iec16022 { };
    jmtpfs = pkgs.jmtpfs.override {
      libmtp = pkgs.libmtp.overrideDerivation (old: {
        patches = old.patches or [] ++ [ ./mtp-jolla.patch ];
      });
    };
    nixops = callPackage ./nixops { };
    libCMT = callPackage ./libcmt { };
    librxtx_java = callPackage ./librxtx-java { };
    list-gamecontrollers = callPackage ./list-gamecontrollers { };
    lockdev = callPackage ./lockdev { };
    pvolctrl = callPackage ./pvolctrl { };
    santander = callPackage ./santander { };
    show-qr-code = callPackage ./show-qr-code { };
    sidplayfp = callPackage ./sidplayfp { };
    tkabber_urgent_plugin = callPackage ./tkabber-urgent-plugin { };
    tomahawk = callPackage ./tomahawk {
      boost = pkgs.boost155;
    };
    twitchstream = callPackage ./twitchstream { };

    games = import ./games {
      inherit pkgs;
      config = pkgs.config.vuizvui.games or null;
    };

    kernelPatches = {
      bfqsched = callPackage ./kpatches/bfqsched { };
    };

    openlab = pkgs.recurseIntoAttrs {
      gitit = callPackage ./openlab/gitit { hlib = pkgs.haskell.lib; };
      stackenblocken = callPackage ./openlab/stackenblocken {};
    };
    profpatsch = pkgs.recurseIntoAttrs {
      display-infos = callPackage ./profpatsch/display-infos {};
    };
  };
in pkgs // self

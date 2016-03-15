{ pkgs ? import (import ../nixpkgs-path.nix) {} }:

let
  callPackage = pkgs.lib.callPackageWith (pkgs // self.vuizvui);

  self.vuizvui = {
    mkChannel = callPackage ./build-support/channel.nix { };

    aacolorize = callPackage ./aacolorize { };
    axbo = callPackage ./axbo { };
    beehive = callPackage ./beehive { };
    blop = callPackage ./blop { };
    grandpa = callPackage ./grandpa { };
    greybird-xfce-theme = callPackage ./greybird-xfce-theme { };
    nixops = callPackage ./nixops { };
    libCMT = callPackage ./libcmt { };
    librxtx_java = callPackage ./librxtx-java { };
    list-gamecontrollers = callPackage ./list-gamecontrollers { };
    lockdev = callPackage ./lockdev { };
    pvolctrl = callPackage ./pvolctrl { };
    show-qr-code = callPackage ./show-qr-code { };
    sidplayfp = callPackage ./sidplayfp { };
    tkabber_urgent_plugin = callPackage ./tkabber-urgent-plugin { };
    tomahawk = callPackage ./tomahawk { qt5 = pkgs.qt55; };
    twitchstream = callPackage ./twitchstream { };

    games = import ./games {
      inherit pkgs;
      config = pkgs.config.vuizvui.games or null;
    };

    kernelPatches = {
      bfqsched = callPackage ./kpatches/bfqsched { };
    };
  };
in pkgs // self

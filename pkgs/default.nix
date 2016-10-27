{ pkgs ? import (import ../nixpkgs-path.nix) {} }:

let
  inherit (pkgs.lib) callPackageWith;
  callPackage = callPackageWith (pkgs // self.vuizvui);
  callPackage_i686 = callPackageWith (pkgs.pkgsi686Linux // self.vuizvui);

  self.vuizvui = {
    mkChannel = callPackage ./build-support/channel.nix { };

    aacolorize = callPackage ./aacolorize { };
    axbo = callPackage ./axbo { };
    # beehive = callPackage ./beehive { }; TODO get running again
    blop = callPackage ./blop { };
    git-detach = callPackage ./git-detach { };
    grandpa = callPackage ./grandpa { };
    greybird-xfce-theme = callPackage ./greybird-xfce-theme { };
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
    santander = callPackage_i686 ./santander { };
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

    kernel = {
      bfqsched = callPackage ./kernel/bfqsched { };
      linux_4_7 = callPackage ./kernel/linux-4.7.nix {
        kernelPatches = [
          pkgs.kernelPatches.bridge_stp_helper
          pkgs.kernelPatches.cpu-cgroup-v2."4.7"
        ];
      };
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

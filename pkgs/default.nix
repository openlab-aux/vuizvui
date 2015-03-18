{ pkgs ? import <nixpkgs> {}
}:

let
  callPackage = pkgs.lib.callPackageWith (pkgs // self);

  self = {
    aacolorize = callPackage ./aacolorize { };
    axbo = callPackage ./axbo { };
    blop = callPackage ./blop { };
    grandpa = callPackage ./grandpa { };
    nixops = callPackage ./nixops { };
    libCMT = callPackage ./libcmt { };
    librxtx_java = callPackage ./librxtx-java { };
    lockdev = callPackage ./lockdev { };
    pvolctrl = callPackage ./pvolctrl { };
    sidplayfp = callPackage ./sidplayfp { };
    tkabber_urgent_plugin = callPackage ./tkabber-urgent-plugin { };
    tomahawk = callPackage ./tomahawk { };
    twitchstream = callPackage ./twitchstream { };

    vuizvuiKernelPatches = {
      bfqsched = callPackage ./kpatches/bfqsched.nix { };
    };
  };
in { vuizvui = self; }

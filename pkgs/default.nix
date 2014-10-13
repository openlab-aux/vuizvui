{ pkgs ? import <nixpkgs> {}
}:

let
  callPackage = pkgs.lib.callPackageWith (pkgs // self);

  self = {
    axbo = callPackage ./axbo { };
    blop = callPackage ./blop { };
    grandpa = callPackage ./grandpa { };
    libCMT = callPackage ./libcmt { };
    librxtx_java = callPackage ./librxtx-java { };
    lockdev = callPackage ./lockdev { };
    pvolctrl = callPackage ./pvolctrl { };
    sidplayfp = callPackage ./sidplayfp { };
    tkabber_urgent_plugin = callPackage ./tkabber-urgent-plugin { };
    twitchstream = callPackage ./twitchstream { };
    vim = callPackage ./vim { };

    vuizvuiKernelPatches = {
      bfqsched = callPackage ./kpatches/bfqsched.nix { };
    };
  };
in { vuizvui = self; }

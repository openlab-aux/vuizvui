{ pkgs ? import <nixpkgs> {}
}:

let
  callPackage = pkgs.lib.callPackageWith (pkgs // self);

  self = {
    axbo = callPackage ./axbo { };
    blop = callPackage ./blop { };
    fish = callPackage ./fish { };
    gajim = callPackage ./gajim/0.16-pre.nix {
      inherit (pkgs.xlibs) libX11;
    };
    libCMT = callPackage ./libcmt { };
    librxtx_java = callPackage ./librxtx-java { };
    lockdev = callPackage ./lockdev { };
    nbxmpp = callPackage ./nbxmpp { };
    pvolctrl = callPackage ./pvolctrl { };
    tkabber_urgent_plugin = callPackage ./tkabber-urgent-plugin { };

    aszligKernelPatches = {
      bfqsched = callPackage ./kpatches/bfqsched.nix { };
    };
  };
in { aszlig = self; }

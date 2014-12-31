{ email ? null, password ? null }:

with import <nixpkgs> {};

let
  self = rec {
    callPackage = pkgs.lib.callPackageWith (pkgs // self);

    fetchHumbleBundle = callPackage ./fetch-humble-bundle {
      inherit email password;
    };

    bastion = callPackage ./bastion.nix {};
    cavestoryplus = callPackage ./cavestoryplus.nix {};
    fez = callPackage ./fez.nix {};
    ftl = callPackage ./ftl.nix {};
    hammerwatch = callPackage ./hammerwatch.nix {};
    jamestown = callPackage ./jamestown.nix {};
    megabytepunch = callPackage ./megabytepunch.nix {};
    rocketbirds = callPackage ./rocketbirds.nix {};
    spaz = callPackage ./spaz.nix {};
    swordsandsoldiers = callPackage ./swordsandsoldiers.nix {};
  };
in self

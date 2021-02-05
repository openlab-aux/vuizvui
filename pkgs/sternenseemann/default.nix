{ pkgs, lib }:

let
  inherit (pkgs)
    callPackage
    fetchurl
    fetchFromGitHub
    ocamlPackages
    python3Packages
    writers
    haskell
    ;

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      emoji-generic = haskell.lib.overrideSrc
        (self.callPackage ./emoji-generic { }) {
          src = fetchFromGitHub {
            owner = "sternenseemann";
            repo = "emoji-generic";
            rev = "3b1267ca254e4b5740a5b963016be198dbde46a1";
            sha256 = "0xhh55lgjphwal0l0yrcv2cricbl2cimdw7bhc5zrgmjqkg84kk2";
          };
          version = "unstable-2020-11-22";
        };
    };
  };

in

{
  inherit (haskellPackages) emoji-generic;

  logbook = ocamlPackages.callPackage ./logbook { };

  shakti = callPackage ./shakti { };

  t = python3Packages.callPackage ./t { };

  tep = callPackage ./tep {
    inherit (haskellPackages)
      emoji-generic text utf8-light
      attoparsec bytestring;
    inherit (writers) writeBashBin writeHaskell;
    emojiTestTxt = fetchurl {
      url = "https://www.unicode.org/Public/emoji/13.1/emoji-test.txt";
      sha256 = "0n6d31533l1gnb1sxz8z486kv6rsggcpxyiq8wc1ald8l89c6g4f";
    };
  };
}

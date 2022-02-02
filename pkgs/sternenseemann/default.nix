{ pkgs, lib, profpatsch }:

let
  inherit (pkgs)
    callPackage
    dontRecurseIntoAttrs
    fetchurl
    fetchFromGitHub
    ocamlPackages
    python3Packages
    writers
    haskell
    ;

  inherit (profpatsch)
    getBins
    ;

  bins = getBins pkgs.bemenu [ "bemenu" ];

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      emoji-generic = haskell.lib.overrideSrc
        (self.callPackage ./emoji-generic { }) {
          src = fetchFromGitHub {
            owner = "sternenseemann";
            repo = "emoji-generic";
            rev = "59355e813fb01487011d0b6d1408b5c789434fc1";
            sha256 = "0sk7fhh6n2k684lzic5z23kyk7i91k33j38wkk41xd92cv3xzsss";
          };
          version = "unstable-2021-03-26";
        };
    };
  };

  rust = callPackage ./rust {
    inherit (profpatsch)
      writeRustSimpleBin
      writeRustSimpleLib
      ;
  };

in

lib.fix (self: {
  # nix utilities
  lib = callPackage ./lib { };

  buildGitTarball = callPackage ./build-git-tarball {
    inherit getBins;
  };

  bundleSignedReleases = callPackage ./bundle-signed-release {
    inherit getBins;
    inherit (self) buildGitTarball;
  };

  # packaged sterniware
  inherit (haskellPackages) emoji-generic;

  logbook = ocamlPackages.callPackage ./logbook { };

  inherit (rust)
    nix-env-diff
    temp
    ;

  schmecgit = callPackage ./schmecgit {
    inherit (pkgs.llvmPackages_latest) stdenv;
  };

  scripts = dontRecurseIntoAttrs (callPackage ./scripts {
    inherit (writers) writeBashBin;
    inherit (self) shakti;
    inherit getBins;
  });

  tep = callPackage ./tep {
    inherit (haskellPackages)
      emoji-generic text utf8-light
      attoparsec bytestring;
    inherit (writers) writeBashBin writeHaskell;
    emojiTestTxt = fetchurl {
      url = "https://www.unicode.org/Public/emoji/14.0/emoji-test.txt";
      sha256 = "02b1zj31cpaa4mf83pv3zisvifgvp4jr1pn1dp7af2k7fgh4nizc";
    };
  };

  unicode_clock = python3Packages.callPackage ./unicode_clock { };

  vuizvui-update-programs-sqlite = python3Packages.callPackage ./vuizvui-update-programs-sqlite {
    inherit (pkgs.writers) writePython3;
    inherit (profpatsch) getBins;
  };

  pass = (pkgs.pass.override {
    waylandSupport = true;
    x11Support = false;
    dmenuSupport = true;
  }).overrideAttrs (old: {
    postPatch = old.postPatch + ''
      substituteInPlace "contrib/dmenu/passmenu" \
        --replace "dmenu-wl" "${bins.bemenu}"
    '';
  });

  texlive = pkgs.texlive.combine {
    inherit (pkgs.texlive)
      scheme-medium minted titlesec units collection-bibtexextra wrapfig
      libertine enumitem dashrule ifmtarg xstring xgreek adjustbox
      collectbox csquotes biblatex-philosophy quoting breakurl
      capt-of;
  };

  # packaged 3rd party software
  saneterm = pkgs.python39Packages.callPackage ./saneterm { };

  shakti = callPackage ./shakti { };
})

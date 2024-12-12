{ pkgs, profpatsch, callPackage }:

let
  inherit (pkgs)
    dontRecurseIntoAttrs
    fetchurl
    fetchFromGitHub
    ocamlPackages
    python3Packages
    writers
    haskell
    lib
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
            rev = "df5debf70a0b84edc9ba2e93e7425e03297cf2ca";
            sha256 = "1niaa2r5xh3famp6r9pdz8c8c356sjvyidflkq64yg994j7xbmpb";
          };
          version = "unstable-2023-11-19";
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
  lib = import ./lib { inherit lib; };

  # packaged sterniware
  inherit (haskellPackages) emoji-generic;

  logbook = ocamlPackages.callPackage ./logbook { };

  inherit (rust)
    nix-env-diff
    temp
    ;

  scripts = dontRecurseIntoAttrs (callPackage ./scripts {
    inherit (writers) writeBashBin;
    inherit getBins;
  });

  tep = callPackage ./tep {
    inherit (haskellPackages)
      emoji-generic text utf8-light
      attoparsec bytestring;
    inherit (writers) writeBashBin writeHaskell;
    emojiTestTxt = fetchurl {
      url = "https://www.unicode.org/Public/emoji/15.1/emoji-test.txt";
      sha256 = "1nby2gl5wffhcpa8i91a0qxx3a2751qampx6rxvam3m2k8jfwxnq";
    };
  };

  vuizvui-update-programs-sqlite = python3Packages.callPackage ./vuizvui-update-programs-sqlite {
    inherit (pkgs.writers) writePython3;
    inherit getBins;
  };

  # customized third party software
  acme = callPackage ./acme {
    inherit (self) plan9port;
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

  plan9port = pkgs.plan9port.overrideAttrs (old: {
    patches = old.patches or [] ++ [
      ./plan9port/neo-modifier-fix.patch
    ];
  });

  texlive = pkgs.texlive.combine {
    inherit (pkgs.texlive)
      scheme-medium minted titlesec units collection-bibtexextra wrapfig
      libertine enumitem dashrule ifmtarg xstring xgreek adjustbox
      collectbox csquotes biblatex-philosophy quoting breakurl
      capt-of hyphen-greek chemfig chemformula;
  };

  # packaged 3rd party software
  saneterm = pkgs.python3Packages.callPackage ./saneterm { };
})

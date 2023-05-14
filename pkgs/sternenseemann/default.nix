{ pkgs, lib, profpatsch, callPackage }:

let
  inherit (pkgs)
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
            rev = "936167ca50bc22718b66b74db06da17b1197b21e";
            sha256 = "0gj58nh5agjqxim64pb2s32rd9s3hy02p4x5klfshgf25qpcwmvs";
          };
          version = "unstable-2022-04-09";
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
      url = "https://www.unicode.org/Public/emoji/15.0/emoji-test.txt";
      sha256 = "1nskm3qqb568dlsz54r0ympqbzyf9zhn40lxw5mhk3iqr0xg4ic4";
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
      ./acme/neo-modifier-fix.patch
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
  saneterm = pkgs.python39Packages.callPackage ./saneterm { };
})

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
    ;

  inherit (profpatsch)
    getBins
    writeRustSimpleBin
    writeRustSimpleLib
    ;

  bins = getBins pkgs.bemenu [ "bemenu" ];

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      emoji-generic = haskell.lib.overrideSrc
        (self.callPackage ./emoji-generic.nix { }) {
          src = fetchFromGitHub {
            owner = "sternenseemann";
            repo = "emoji-generic";
            rev = "e0133be42f7f5d446bf493f790a4b465c4c7c4d4";
            sha256 = "04kx8ib9mx71iy1w4fvajki72dgn2d8c7642rrb3wbgv16cncadq";
          };
          version = "unstable-2025-05-09";
        };
    };
  };

in

{
  # packaged sterniware
  inherit (haskellPackages) emoji-generic;

  logbook = ocamlPackages.callPackage ./logbook.nix { };

  temp = writeRustSimpleLib "temp" {
    release = false;
    verbose = true;
    meta = {
      description = "Tiny temp dir/file crate for rust";
    };
  } ./temp/temp.rs;

  nix-env-diff = writeRustSimpleBin "nix-env-diff" {
    meta = {
      description = "Print changed attrs / outpath for nix-env outputs";
    };
  } ./nix-env-diff.rs;

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
      url = "https://www.unicode.org/Public/emoji/16.0/emoji-test.txt";
      sha256 = "15wn2f2yvk66576xs7lij8ip0gkawkqfhlv997i45wbcx0scbw14";
    };
  };

  vuizvui-update-programs-sqlite = python3Packages.callPackage ./vuizvui-update-programs-sqlite {
    inherit (pkgs.writers) writePython3;
    inherit getBins;
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
      # collections
      scheme-medium
      collection-bibtexextra
      # collection-langgreek
      # individual packages
      adjustbox
      biblatex-philosophy
      breakurl
      capt-of
      chemfig
      chemformula
      collectbox
      csquotes
      dashrule
      enumitem
      footmisc
      hyphen-greek
      ifmtarg
      libertine
      minted
      noto
      noto-emoji
      notomath
      quoting
      titlesec
      units
      wrapfig
      xgreek
      xstring
      ;
  };

  # packaged 3rd party software
  saneterm = pkgs.python3Packages.callPackage ./saneterm.nix { };
}

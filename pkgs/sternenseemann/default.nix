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

  texliveCommon = {
    inherit (pkgs.texlive)
      adjustbox
      biber
      biblatex
      biblatex-anonymous
      biblatex-archaeology
      biblatex-arthistory-bonn
      biblatex-bookinarticle
      biblatex-bookinother
      biblatex-chicago
      biblatex-dw
      biblatex-ext
      biblatex-fiwi
      biblatex-german-legal
      biblatex-historian
      biblatex-ieee
      biblatex-iso690
      biblatex-morenames
      biblatex-multiple-dm
      biblatex-oxref
      biblatex-philosophy
      biblatex-publist
      biblatex-readbbl
      biblatex-realauthor
      biblatex-shortfields
      biblatex-socialscienceshuberlin
      biblatex-software
      biblatex-source-division
      biblatex-trad
      biblatex-true-citepages-omit
      biblatex-unified
      biblatex-vancouver
      breakurl
      capt-of
      chemfig
      chemformula
      collectbox
      csquotes
      dashrule
      enumitem
      footmisc
      geschichtsfrkl
      hyphen-greek
      ifmtarg
      latexmk
      libertine
      minted
      noto
      noto-emoji
      notomath
      quoting
      texcount
      titlesec
      units
      wrapfig
      xgreek
      xstring
    ;
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
      url = "https://www.unicode.org/Public/emoji/17.0/emoji-test.txt";
      sha256 = "1nmc06i066r322br8hs5wb55p6b77kppy5n5yxz2z5fpi17r92hx";
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

  texlive = pkgs.texlive.combine ({
    inherit (pkgs.texlive)
      # collections
      scheme-medium
      collection-bibtexextra
      collection-latex
      # collection-langgreek
      ;
  } // texliveCommon);

  # avoid dependency on Java since this is unsupported on i686
  texlive-small = pkgs.texlive.combine ({
    inherit (pkgs.texlive) scheme-small;
  } // texliveCommon);

  # packaged 3rd party software
  saneterm = pkgs.python3Packages.callPackage ./saneterm.nix { };
}

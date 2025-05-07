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
            rev = "b34181b801550ae8cae3187dd3d6b1a6623bac98";
            sha256 = "0nmblk65cij082h8m8n1g06s5y5pvzhphbqpc297l31spsw623z2";
          };
          version = "unstable-2025-05-08";
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
      scheme-medium minted titlesec units collection-bibtexextra wrapfig
      libertine enumitem dashrule ifmtarg xstring xgreek adjustbox
      collectbox csquotes biblatex-philosophy quoting breakurl
      capt-of hyphen-greek chemfig chemformula;
  };

  # packaged 3rd party software
  saneterm = pkgs.python3Packages.callPackage ./saneterm.nix { };
}

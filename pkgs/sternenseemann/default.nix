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
            rev = "3b1267ca254e4b5740a5b963016be198dbde46a1";
            sha256 = "0xhh55lgjphwal0l0yrcv2cricbl2cimdw7bhc5zrgmjqkg84kk2";
          };
          version = "unstable-2020-11-22";
        };
    };
  };

  rust = callPackage ./rust {
    inherit (profpatsch)
      writeRustSimpleLib
      testRustSimple
      ;
  };

in

lib.fix (self: {
  inherit (haskellPackages) emoji-generic;

  lib = callPackage ./lib { };

  logbook = ocamlPackages.callPackage ./logbook { };

  mandoc = pkgs.mandoc.overrideAttrs (old: rec {
    src = pkgs.fetchcvs {
      sha256 = "19cqasw7fjsmhshs5khxrv8w3vdhf8xadls70l0gzqn7cyjmgsb9";
      date = "2021-02-07";
      cvsRoot = "anoncvs@mandoc.bsd.lv:/cvs";
      module = "mandoc";
    };
    version = "unstable-${src.date}"; # actually early but idc
    # fix makewhatis(1) skipping all man pages that
    # are symlinks to /nix/store
    patches = [ ./patches/mandoc-nix-store.patch ];
    patchFlags = [ "-p0" ];
    preConfigure = old.preConfigure + ''
      echo NIXSTOREDIR="$(dirname "$out")" >> configure.local
    '';
  });

  pass = (pkgs.pass.override {
    waylandSupport = true;
    x11Support = false;
  }).overrideAttrs (old: {
    patches = old.patches ++ [ ./patches/passmenu-wayland.patch ];
    postPatch = ''
      ${old.postPatch}
      substituteInPlace "contrib/dmenu/passmenu" \
        --replace "bemenu" "'${bins.bemenu} -l10'"
    '';
    postInstall = ''
      ${old.postInstall}
      cp "contrib/dmenu/passmenu" "$out/bin/"
    '';
  });

  inherit (rust)
    temp
    ;

  # don't bother hydra with trivial text substitution
  scripts = dontRecurseIntoAttrs (callPackage ./scripts {
    inherit (writers) writeBashBin;
    inherit (self) shakti;
    inherit getBins;
  });

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

  texlive = pkgs.texlive.combine {
    inherit (pkgs.texlive)
      scheme-medium minted titlesec units collection-bibtexextra wrapfig
      libertine enumitem dashrule ifmtarg xstring xgreek adjustbox
      collectbox csquotes biblatex-philosophy quoting breakurl;
  };

  unicode_clock = python3Packages.callPackage ./unicode_clock { };
})

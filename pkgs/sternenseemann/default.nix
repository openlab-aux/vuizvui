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
      url = "https://www.unicode.org/Public/emoji/13.1/emoji-test.txt";
      sha256 = "0n6d31533l1gnb1sxz8z486kv6rsggcpxyiq8wc1ald8l89c6g4f";
    };
  };

  unicode_clock = python3Packages.callPackage ./unicode_clock { };

  vuizvui-update-programs-sqlite = python3Packages.callPackage ./vuizvui-update-programs-sqlite {
    inherit (pkgs.writers) writePython3;
    inherit (profpatsch) getBins;
  };

  # patched packages
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
      echo READ_ALLOWED_PATH=\"$(dirname "$out")\" >> configure.local
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

  texlive = pkgs.texlive.combine {
    inherit (pkgs.texlive)
      scheme-medium minted titlesec units collection-bibtexextra wrapfig
      libertine enumitem dashrule ifmtarg xstring xgreek adjustbox
      collectbox csquotes biblatex-philosophy quoting breakurl;
  };

  # packaged 3rd party software
  shakti = callPackage ./shakti { };

  t = python3Packages.callPackage ./t { };
})

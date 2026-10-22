{ pkgs, profpatsch, callPackage }:

let
  inherit (pkgs)
    lib
    fetchurl
    fetchFromGitHub
    ocamlPackages
    python3Packages
    writers
    runCommand
    makeWrapper
    ;

  inherit (profpatsch)
    getBins
    writeRustSimpleBin
    writeRustSimpleLib
    ;

  bins = getBins pkgs.bemenu [ "bemenu" ];

  packageScriptFile =
    { name
    , file ? ./scripts + "/${name}"
    , interpreter
    , isShell ? false
    , runtimeDependencies ? []
    }:

    let
      binPath = lib.makeBinPath runtimeDependencies;
    in

    runCommand name {
      buildInputs = [ interpreter ];
      nativeBuildInputs = [ makeWrapper ];
      meta.mainProgram = name;
    } (''
      install -Dm755 "${file}" "$out/bin/${name}"
      patchShebangs "$out/bin/${name}"
    '' + lib.optionalString (runtimeDependencies != []) (
      if isShell then ''
        sed -i \
          -e '2i export PATH="'${lib.escapeShellArg binPath}':$PATH"' \
          "$out/bin/${name}"
      '' else ''
        wrapProgram "$out/bin/${name}" \
          --prefix PATH : ${lib.escapeShellArg binPath}
      ''
    ));

in

{
  # packaged sterniware
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

  scripts = lib.dontRecurseIntoAttrs (callPackage ./scripts {
    inherit (writers) writeBashBin;
    inherit getBins packageScriptFile;
  });

  tep = callPackage ./tep {
    inherit packageScriptFile;
    inherit (writers) writeBashBin;
    emojiTestTxt = fetchurl {
      urls = [
        "https://www.unicode.org/Public/emoji/17.0/emoji-test.txt"
        "https://www.unicode.org/Public/emoji/latest/emoji-test.txt"
      ];
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
        --replace-fail "dmenu-wl" "${bins.bemenu}"
    '';
  });

  sacc = pkgs.sacc.overrideAttrs (oldAttrs: {
    postPatch = ''
      ${oldAttrs.postPatch or ""}
      substituteInPlace config.def.h --replace-fail xclip wl-copy
    '';
  });

  texlive = pkgs.texlive.schemes.texliveMedium.withPackages (ps: [
    ps.collection-bibtexextra
    ps.collection-latex
    ps.collection-langgreek

    ps.adjustbox
    ps.biber
    ps.biblatex
    ps.biblatex-anonymous
    ps.biblatex-archaeology
    ps.biblatex-arthistory-bonn
    ps.biblatex-bookinarticle
    ps.biblatex-bookinother
    ps.biblatex-chicago
    ps.biblatex-dw
    ps.biblatex-ext
    ps.biblatex-fiwi
    ps.biblatex-german-legal
    ps.biblatex-historian
    ps.biblatex-ieee
    ps.biblatex-iso690
    ps.biblatex-morenames
    ps.biblatex-multiple-dm
    ps.biblatex-oxref
    ps.biblatex-philosophy
    ps.biblatex-publist
    ps.biblatex-readbbl
    ps.biblatex-realauthor
    ps.biblatex-shortfields
    ps.biblatex-socialscienceshuberlin
    ps.biblatex-software
    ps.biblatex-source-division
    ps.biblatex-trad
    ps.biblatex-true-citepages-omit
    ps.biblatex-unified
    ps.biblatex-vancouver
    ps.breakurl
    ps.capt-of
    ps.chemfig
    ps.chemformula
    ps.collectbox
    ps.csquotes
    ps.dashrule
    ps.enumitem
    ps.footmisc
    ps.geschichtsfrkl
    ps.hyphen-greek
    ps.ifmtarg
    ps.latexmk
    ps.libertine
    ps.minted
    ps.noto
    ps.noto-emoji
    ps.notomath
    ps.quoting
    ps.standalone
    ps.tikz-cd
    ps.texcount
    ps.titlesec
    ps.units
    ps.wrapfig
    ps.xgreek
    ps.xstring
  ]);

  # packaged 3rd party software
  arr =
    let
      rev = "7f7b69afd3d6f9dad9901ffe2194464a53acd7a9";
    in
    pkgs.stdenv.mkDerivation {
      pname = "arr";
      version = lib.substring 0 7 rev;
      src = pkgs.fetchFromGitHub {
        owner = "leahneukirchen";
        repo = "arr";
        inherit rev;
        hash = "sha256-+uoZKbuPZmNwnMS1Kd7GimrnBqX9CMGaj1crhR6Y7/o=";
      };

      makeFlags = [
        "DESTDIR=${placeholder "out"}"
        "PREFIX="
      ];

      buildInputs = [
        pkgs.ruby
      ];
    };

  saneterm = pkgs.python3Packages.callPackage ./saneterm.nix { };
}

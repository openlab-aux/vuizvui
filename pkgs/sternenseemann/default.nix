{ pkgs, profpatsch, callPackage }:

let
  inherit (pkgs)
    lib
    dontRecurseIntoAttrs
    fetchurl
    fetchFromGitHub
    ocamlPackages
    python3Packages
    writers
    runCommandNoCC
    makeWrapper
    ;

  inherit (profpatsch)
    getBins
    writeRustSimpleBin
    writeRustSimpleLib
    ;

  bins = getBins pkgs.bemenu [ "bemenu" ];

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

    runCommandNoCC name {
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

  scripts = dontRecurseIntoAttrs (callPackage ./scripts {
    inherit (writers) writeBashBin;
    inherit getBins packageScriptFile;
  });

  tep = callPackage ./tep {
    inherit packageScriptFile;
    inherit (writers) writeBashBin;
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
        --replace-fail "dmenu-wl" "${bins.bemenu}"
    '';
  });

  sacc = pkgs.sacc.overrideAttrs (oldAttrs: {
    postPatch = ''
      ${oldAttrs.postPatch or ""}
      substituteInPlace config.def.h --replace-fail xclip wl-copy
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

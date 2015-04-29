{ vuizvuiSrc ? null
, nixpkgsSrc ? null
, supportedSystems ? [ "i686-linux" "x86_64-linux" ]
}:

let
  nixpkgsRevCount = nixpkgsSrc.revCount or 12345;
  nixpkgsShortRev = nixpkgsSrc.shortRev or "abcdefg";
  nixpkgsVersion = "pre${toString nixpkgsRevCount}.${nixpkgsShortRev}-vuizvui";

  nixpkgs = let
    patchedNixpkgs = (import nixpkgsSrc {}).stdenv.mkDerivation {
      name = "nixpkgs-${nixpkgsVersion}";
      src = nixpkgsSrc;
      phases = [ "unpackPhase" "installPhase" ];
      installPhase = ''
        sed -i -re 's!<nixpkgs([^>]*)>!<vuizvui/nixpkgs\1>!g' \
          nixos/modules/installer/tools/nixos-rebuild.sh
        cp -r . "$out"
      '';
    };
  in if nixpkgsSrc == null then <nixpkgs> else patchedNixpkgs;

  vuizvuiRevCount = vuizvuiSrc.revCount or 12345;
  vuizvuiShortRev = vuizvuiSrc.shortRev or "abcdefg";
  vuizvuiVersion = "pre${toString vuizvuiRevCount}.${vuizvuiShortRev}";

  vuizvui = let
    patchedVuizvui = (import nixpkgs {}).stdenv.mkDerivation {
      name = "vuizvui-${vuizvuiVersion}";
      inherit nixpkgsVersion;
      src = vuizvuiSrc;
      phases = [ "unpackPhase" "installPhase" ];
      installPhase = ''
        cp -r --no-preserve=mode,ownership "${nixpkgs}/" nixpkgs
        echo -n "$nixpkgsVersion" > nixpkgs/.version-suffix
        echo -n ${nixpkgs.rev or nixpkgsShortRev} > nixpkgs/.git-revision
        echo './nixpkgs' > nixpkgs-path.nix
        cp -r . "$out"
      '';
    };
  in if vuizvuiSrc == null then ./. else patchedVuizvui;

  system = "x86_64-linux";
  pkgsUpstream = import nixpkgs { inherit system; };
  root = import vuizvui { inherit system; };

in with pkgsUpstream.lib; with builtins; {

  machines = mapAttrsRecursiveCond (m: !(m ? build)) (path: attrs:
    attrs.build.config.system.build.toplevel
  ) (import "${vuizvui}/machines" { inherit system; });

  tests = mapAttrsRecursiveCond (t: !(t ? test)) (const id)
    (import "${vuizvui}/tests" { inherit system; });

  pkgs = let
    releaseLib = import "${nixpkgs}/pkgs/top-level/release-lib.nix" {
      inherit supportedSystems;
      packageSet = attrs: (import vuizvui attrs).pkgs;
    };
  in with releaseLib; mapTestOn (packagePlatforms releaseLib.pkgs);

  channels = let
    mkChannel = attrs: root.pkgs.mkChannel (rec {
      name = "vuizvui-channel-${attrs.name or "generic"}-${vuizvuiVersion}";
      src = vuizvui;
      patchPhase = ''
        touch .update-on-nixos-rebuild
      '';
    } // removeAttrs attrs [ "name" ]);

  in {
    generic = mkChannel {};

    machines = mapAttrsRecursiveCond (m: !(m ? build)) (path: attrs: mkChannel {
      name = "machine-${last path}";
      constituents = singleton attrs.build.config.system.build.toplevel;
    }) (import "${vuizvui}/machines" { inherit system; });
  };

  manual = let
    modules = import "${nixpkgs}/nixos/lib/eval-config.nix" {
      modules = import "${vuizvui}/modules/module-list.nix";
      check = false;
      inherit system;
    };

    isVuizvui = opt: head (splitString "." opt.name) == "vuizvui";
    filterDoc = filter (opt: isVuizvui opt && opt.visible && !opt.internal);
    optionsXML = toXML (filterDoc (optionAttrSetToDocList modules.options));
    optionsFile = toFile "options.xml" (unsafeDiscardStringContext optionsXML);
  in pkgsUpstream.stdenv.mkDerivation {
    name = "vuizvui-options";

    buildInputs = singleton pkgsUpstream.libxslt;

    xsltFlags = ''
      --param section.autolabel 1
      --param section.label.includes.component.label 1
      --param html.stylesheet 'style.css'
      --param xref.with.number.and.title 1
      --param admon.style '''
    '';

    buildCommand = ''
      xsltproc -o options-db.xml \
        "${nixpkgs}/nixos/doc/manual/options-to-docbook.xsl" \
        ${optionsFile}

      cat > manual.xml <<XML
      <book xmlns="http://docbook.org/ns/docbook"
            xmlns:xlink="http://www.w3.org/1999/xlink"
            xmlns:xi="http://www.w3.org/2001/XInclude">
        <title>Vuizvui-specific NixOS options</title>
        <para>
          The following NixOS options are specific to Vuizvui:
        </para>
        <xi:include href="options-db.xml" />
      </book>
      XML

      xsltproc -o "$out/manual.html" $xsltFlags -nonet -xinclude \
        ${pkgsUpstream.docbook5_xsl}/xml/xsl/docbook/xhtml/docbook.xsl \
        manual.xml

      cp "${nixpkgs}/nixos/doc/manual/style.css" "$out/style.css"

      mkdir -p "$out/nix-support"
      echo "doc manual $out manual.html" \
        > "$out/nix-support/hydra-build-products"
    '';
  };
}

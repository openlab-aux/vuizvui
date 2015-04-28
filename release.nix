{ vuizvui ? { outPath = ./.; revCount = 12345; shortRev = "abcdefg"; }
, nixpkgs ? { outPath = <nixpkgs>; revCount = 12345; shortRev = "abcdefg"; }
, supportedSystems ? [ "i686-linux" "x86_64-linux" ]
}:

let
  system = "x86_64-linux";
  pkgsUpstream = import <nixpkgs> { inherit system; };
  root = import ./default.nix { inherit system; };

  patchNixpkgsReference = path: ''
    find -iname '*.nix' -type f -exec \
      sed -i -re 's!<nixpkgs([^>]*)>!${path}\1!g' {} +
  '';

  patchedNixpkgs = pkgsUpstream.stdenv.mkDerivation rec {
    name = "nixpkgs-${version}";
    version = "${toString nixpkgs.revCount}.${nixpkgs.shortRev}";
    src = nixpkgs;
    phases = [ "unpackPhase" "patchPhase" "installPhase" ];
    installPhase = "cp -r . \"$out\"";
    patchPhase = (patchNixpkgsReference "'\"$out\"'") + ''
      sed -i -re 's!<nixpkgs([^>]*)>!<vuizvui/nixpkgs\1>!g' \
        nixos/modules/installer/tools/nixos-rebuild.sh
    '';
  };

in with pkgsUpstream.lib; with builtins; {

  machines = mapAttrsRecursiveCond (m: !(m ? build)) (path: attrs:
    attrs.build.config.system.build.toplevel
  ) (import ./machines { inherit system; });

  tests = mapAttrsRecursiveCond (t: !(t ? test)) (const id) (import ./tests {
    inherit system;
  });

  pkgs = let
    releaseLib = import <nixpkgs/pkgs/top-level/release-lib.nix> {
      inherit supportedSystems;
      packageSet = attrs: (import ./default.nix attrs).pkgs;
    };
  in with releaseLib; mapTestOn (packagePlatforms releaseLib.pkgs);

  channels = let
    mkChannel = attrs: root.pkgs.mkChannel (rec {
      name = "vuizvui-channel-${attrs.name or "generic"}-${version}";
      version = "${toString vuizvui.revCount}.${vuizvui.shortRev}";
      src = vuizvui;
      patchPhase = (patchNixpkgsReference patchedNixpkgs) + ''
        ln -s "${patchedNixpkgs}" nixpkgs
      '';
    } // removeAttrs attrs [ "name" ]);

  in {
    generic = mkChannel {};

    machines = mapAttrsRecursiveCond (m: !(m ? build)) (path: attrs: mkChannel {
      name = "machine-${last path}";
      constituents = singleton attrs.build.config.system.build.toplevel;
    }) (import ./machines { inherit system; });
  };

  manual = let
    modules = import <nixpkgs/nixos/lib/eval-config.nix> {
      modules = import ./modules/module-list.nix;
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
        ${<nixpkgs/nixos/doc/manual/options-to-docbook.xsl>} \
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

      cp "${<nixpkgs/nixos/doc/manual/style.css>}" "$out/style.css"

      mkdir -p "$out/nix-support"
      echo "doc manual $out manual.html" \
        > "$out/nix-support/hydra-build-products"
    '';
  };
}

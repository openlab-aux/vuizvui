let
  system = "x86_64-linux";
  pkgs = import <nixpkgs> { inherit system; };

in with pkgs.lib; with builtins; {

  machines = mapAttrsRecursiveCond (m: !(m ? build)) (path: attrs:
    attrs.build.config.system.build.toplevel
  ) (import ./machines { inherit system; });

  tests = {
    i3 = import ./tests/i3.nix { inherit system; };
    heinrich = import ./tests/heinrich.nix { inherit system; };
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
  in pkgs.stdenv.mkDerivation {
    name = "vuizvui-options";

    buildInputs = singleton pkgs.libxslt;

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
        ${pkgs.docbook5_xsl}/xml/xsl/docbook/xhtml/docbook.xsl \
        manual.xml

      cp "${<nixpkgs/nixos/doc/manual/style.css>}" "$out/style.css"

      mkdir -p "$out/nix-support"
      echo "doc manual $out manual.html" \
        > "$out/nix-support/hydra-build-products"
    '';
  };
}

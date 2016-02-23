{ stdenv, fetchSteam, makeWrapper, SDL, mesa, flavor ? "stable" }:

let
  renameAttrs = f: let
    rename = name: value: {
      name = f name;
      inherit value;
    };
  in stdenv.lib.mapAttrs' rename;

  darwinize = renameAttrs (bin: "Starbound.app/Contents/MacOS/${bin}");
  winize = renameAttrs (bin: "${bin}.exe");

  mkOsBinaryDeps = with stdenv.lib;
    if stdenv.system == "x86_64-darwin" then darwinize
    else if elem stdenv.system [ "i686-cygwin" "x86_64-cygwin" ] then winize
    else id;

  binaryDeps = mkOsBinaryDeps {
    starbound.deps = [ SDL mesa ];
    starbound_server.name = "starbound-server";
    asset_packer.name = "starbound-asset-packer";
    asset_unpacker.name = "starbound-asset-unpacker";
    dump_versioned_json.name = "starbound-dump-versioned-json";
    make_versioned_json.name = "starbound-make-versioned-json";
    planet_mapgen.name = "starbound-planet-mapgen";
  };

  binpath = if stdenv.system == "x86_64-linux" then "linux64"
            else if stdenv.system == "i686-linux" then "linux32"
            else if stdenv.system == "x86_64-darwin" then "osx"
            else if stdenv.system == "i686-cygwin" then "win32"
            else if stdenv.system == "x86_64-cygwin" then "win64"
            else throw "Unsupported system ${stdenv.system} for Starbound";

  upstreamInfo = if flavor == "stable" then {
    name = "starbound";
    version = "20151216";
    appId = 211820;
    depotId = 211821;
    manifestId = 1842730272313189605;
    sha256 = "0qppfn56c778wsg38hi6sxgi3rl9nv72h9rmmxybi1vzpf3p49py";
  } else if flavor == "unstable" then {
    name = "starbound-unstable";
    version = "20160223";
    appId = 367540;
    depotId = 367541;
    manifestId = 6970641909803280413;
    sha256 = "0qppfn56c778wsg38hi6sxgi3rl9nv72h9rmmxybi1vzpf3p49py";
  } else throw "Unsupported flavor, use either `stable' or `unstable'.";

  upstream = fetchSteam {
    inherit (upstreamInfo) name appId depotId manifestId sha256;
    fileList = [
      "^(?:assets|tiled)/"
      ( "^${binpath}(?:/Starbound\\.app/Contents/MacOS)?"
      + "/(?:[a-zA-Z0-9_-]+(?:\\.exe)?|sbboot\\.config)$")
    ];
  };

in stdenv.mkDerivation {
  name = "${upstreamInfo.name}-${upstreamInfo.version}";
  inherit (upstreamInfo) version;

  unpackPhase = ":";

  configurePhase = ''
    libexec="$out/libexec/starbound"
    data="$out/share/starbound"
  '';

  inherit binpath upstream;

  buildInputs = [ makeWrapper ];

  buildPhase = with stdenv.lib; ''
    mkdir -p patched
    sed -e 's,\.\./assets,'"$data/assets"',g' \
        -e 's,\.\./giraffe_storage,.,g' \
        "$upstream/$binpath/sbboot.config" > "patched/sbboot.config"
  '' + concatStrings (mapAttrsToList (bin: attrs: ''
    mkdir -p "patched/$(dirname "${bin}")"
    cp -t "patched/$(dirname "${bin}")" "$upstream/$binpath/${bin}"
    chmod +x "patched/$(basename "${bin}")"
    patchelf \
      --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
      --set-rpath "${stdenv.lib.makeLibraryPath (attrs.deps or [])}" \
      "patched/$(basename "${bin}")"
    if ldd "patched/$(basename "${bin}")" | grep -F 'not found'; then
      exit 1;
    fi
  '') binaryDeps);

  doCheck = true;

  checkPhase = ''
    checkFailed=
    for i in "$upstream/$binpath"/*; do
      [ -f "$i" ] || continue
      [ "$(basename "$i")" != launcher ] || continue
      [ ! -e "patched/$(basename "$i")" ] || continue

      echo "Found missing binary $i from the upstream tree."
      checkFailed=1
    done
    [ -z "$checkFailed" ]
  '';

  installPhase = ''
    mkdir -p "$out/bin"
    ${stdenv.lib.concatStrings (stdenv.lib.mapAttrsToList (bin: attrs: ''
      prog="$(basename "${bin}")"
      install -vD "patched/$prog" "$libexec/$prog"
      cat > "$out/bin/${attrs.name or "$prog"}" <<EOF
      #!${stdenv.shell} -e
      [ -n "\$XDG_DATA_HOME" ] || XDG_DATA_HOME="\$HOME/.local/share"
      mkdir -p "\$XDG_DATA_HOME/starbound/mods"
      ln -sf "$out/etc/sbboot.config" "\$XDG_DATA_HOME/starbound/sbboot.config"
      cd "\$XDG_DATA_HOME/starbound"
      exec "$libexec/$prog" "\$@"
      EOF
      chmod +x "$out/bin/${attrs.name or "$prog"}"
    '') binaryDeps)}

    install -vD -m 644 patched/sbboot.config "$out/etc/sbboot.config"

    mkdir -p "$data"
    for i in assets tiled; do
      echo -n "Installing $i..."
      cp -rt "$data" "$upstream/$i"
      echo " done."
    done
  '';

  dontStrip = true;
}

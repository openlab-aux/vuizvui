{ stdenv, fetchSteam, fetchurl, writeText, SDL, mesa, jq, makeDesktopItem
, flavor ? "stable"
}:

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
    starbound.hasBootconfigArg = true;
    starbound_server.name = "starbound-server";
    starbound_server.hasBootconfigArg = true;
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

  throwUnsupported = throw "Unsupported flavor `${flavor}', use either "
                         + "`stable' or `unstable'.";

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
  } else throwUnsupported;

  upstream = fetchSteam {
    inherit (upstreamInfo) name appId depotId manifestId sha256;
    fileList = [
      "^(?:assets|tiled)/"
      ( "^${binpath}(?:/Starbound\\.app/Contents/MacOS)?"
      + "/(?:[a-zA-Z0-9_-]+(?:\\.exe)?|sbboot\\.config)$")
    ];
  };

  staticBootOverrides = writeText "bootconfig.overrides" (builtins.toJSON {
    assetSources = [
      "${upstream}/assets/packed.pak"
      "${upstream}/assets/user"
    ];
  });

  bootOverrides = {
    storageDirectory = "$XDG_DATA_HOME/${settingsDir}/";
    modSource = "$XDG_DATA_HOME/${settingsDir}/mods/";
  };

  settingsDir =
    if flavor == "stable" then "starbound"
    else if flavor == "unstable" then "starbound-unstable"
    else throwUnsupported;

  mkProg = bin: attrs: let
    basename = builtins.baseNameOf bin;

    hasBootconfigArg = attrs.hasBootconfigArg or false;

    bootconfigArgs = with stdenv.lib; let
      mkArg = opt: val: "-${opt} \"${val}\"";
    in " " + (concatStringsSep " " (mapAttrsToList mkArg {
      bootconfig = "$XDG_DATA_HOME/${settingsDir}/sbboot.config";
    }));

    wrapper = writeText "starbound-wrapper.sh" ''
      #!${stdenv.shell} -e
      [ -n "$XDG_DATA_HOME" ] || XDG_DATA_HOME="$HOME/.local/share"

      mkdir -p "${bootOverrides.storageDirectory}" \
               "${bootOverrides.modSource}"

      "${jq}/bin/jq" -s '.[0] * .[1]' "@out@/etc/sbboot.config" - \
        > "$XDG_DATA_HOME/${settingsDir}/sbboot.config" \
      <<BOOTCONFIG_OVERRIDES
      ${builtins.toJSON bootOverrides}
      BOOTCONFIG_OVERRIDES

      ${if hasBootconfigArg then ''
        # This is needed because Starbound aborts if
        # a command line argument is specified twice.
        hasBootconfigArg() {
          while [ $# -gt 0 ]; do
            if [ "x''${1#-}" != "x$1" ]; then
              case "''${1#-}" in
                bootconfig) return 0;;
                # Arguments that expect a parameter
                loglevel|logfile|configfile|setconfig) shift;;
              esac
            fi
            shift
          done
          return 1
        }

        if hasBootconfigArg "$@"; then
          exec "@out@/libexec/starbound/${basename}" "$@"
        else
          exec "@out@/libexec/starbound/${basename}"${bootconfigArgs} "$@"
        fi
      '' else ''
        exec "@out@/libexec/starbound/${basename}" "$@"
      ''}
    '';

  in ''
    install -vD "patched/${basename}" "$out/libexec/starbound/${basename}"
    substituteAll "${wrapper}" "$out/bin/${attrs.name or basename}"
    chmod +x "$out/bin/${attrs.name or basename}"
  '';

  desktopItem = makeDesktopItem {
    name = "starbound";
    exec = "starbound";
    icon = fetchurl {
      url = "http://i1305.photobucket.com/albums/s544/ClockworkBarber/"
          + "logo_zps64c4860d.png";
      sha256 = "11fiiy0vcxzix1j81732cjh16wi48k4vag040vmbhad50ps3mg0q";
    };
    comment = "An extraterrestrial sandbox adventure game";
    desktopName = "Starbound";
    genericName = "starbound";
    categories = "Game;";
  };

  patchBinary = bin: attrs: ''
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
  '';

in stdenv.mkDerivation {
  name = "${upstreamInfo.name}-${upstreamInfo.version}";
  inherit (upstreamInfo) version;

  unpackPhase = ":";

  inherit binpath upstream;

  buildPhase = with stdenv.lib;
    concatStrings (mapAttrsToList patchBinary binaryDeps);

  doCheck = true;

  checkPhase = ''
    checkFailed=
    for i in "$upstream/$binpath"/*; do
      [ -f "$i" ] || continue
      [ "$(basename "$i")" != sbboot.config ] || continue
      [ "$(basename "$i")" != launcher ] || continue
      [ ! -e "patched/$(basename "$i")" ] || continue

      echo "Found missing binary $i from the upstream tree."
      checkFailed=1
    done
    [ -z "$checkFailed" ]
  '';

  installPhase = ''
    mkdir -p "$out/bin" "$out/etc"
    sed -e 's,//.*,,' "${upstream}/${binpath}/sbboot.config" \
      | "${jq}/bin/jq" -s '.[0] * .[1]' - "${staticBootOverrides}" \
      > "$out/etc/sbboot.config"
    ${stdenv.lib.concatStrings (stdenv.lib.mapAttrsToList mkProg binaryDeps)}
    install -m 0644 -vD "${desktopItem}/share/applications/starbound.desktop" \
      "$out/share/applications/starbound.desktop"
  '';

  dontStrip = true;
}

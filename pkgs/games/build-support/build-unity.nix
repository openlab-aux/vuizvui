{ stdenv, lib, buildGame, makeWrapper, gtk2-x11, gdk-pixbuf, glib
, libGL, xorg, libpulseaudio, udev, zlib
}:

{ name, version, fullName
, saveDir ? null
, nativeBuildInputs ? []
, buildInputs ? []
, runtimeDependencies ? []
, sandbox ? {}
, ...
}@attrs:

let
  arch = if stdenv.system == "x86_64-linux" then "x86_64" else "x86";
  executable = "${fullName}.${arch}";
  dataDir = "${fullName}_Data";
  maybeSavedir = lib.optionalString (saveDir != null) "/${saveDir}";

in buildGame ({
  name = "${name}-${version}";
  inherit fullName version arch executable dataDir;
  slugName = name;

  nativeBuildInputs = [ makeWrapper ] ++ nativeBuildInputs;

  buildInputs = [ gtk2-x11 gdk-pixbuf glib ] ++ buildInputs;

  runtimeDependencies = [
    libGL xorg.libX11 xorg.libXcursor xorg.libXrandr udev zlib
  ] ++ runtimeDependencies;

  sandbox = sandbox // {
    paths = (sandbox.paths or {}) // {
      required = (sandbox.paths.required or []) ++ [
        "$XDG_CONFIG_HOME/unity3d${maybeSavedir}"
      ];
    };
  };

  installPhase = ''
    runHook preInstall

    install -vD "$executable" "$out/libexec/$slugName/$slugName"
    ln -s "$out/share/$slugName" "$out/libexec/$slugName/Data"

    mkdir -p "$out/bin"
    makeWrapper "$out/libexec/$slugName/$slugName" "$out/bin/$slugName" \
      --run "cd '$out/share/$slugName'" $makeWrapperArgs

    iconpath="$out/share/$slugName/Resources/UnityPlayer.png"
    mkdir -p "$out/share/applications"
    cat > "$out/share/applications/$slugName.desktop" <<EOF
    [Desktop Entry]
    Name=$fullName
    Type=Application
    Version=1.1
    Exec=$out/bin/$slugName
    Icon=$iconpath
    Categories=Game
    StartupNotify=true
    EOF

    cp -vRd "$dataDir" "$out/share/$slugName"

    if [ -d "$fullName.app" ]; then
      cp -vRd -t "$out/share/$slugName" "$fullName.app"
    fi

    if [ ! -e "$iconpath" ]; then
      echo "Desktop icon not found at $iconpath." >&2
      exit 1
    fi

    runHook postInstall
  '';
} // removeAttrs attrs [
  "name" "version" "fullName" "nativeBuildInputs" "buildInputs"
  "runtimeDependencies" "sandbox"
])

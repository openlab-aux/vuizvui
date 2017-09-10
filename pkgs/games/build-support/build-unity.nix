{ stdenv, makeWrapper, mesa, xorg, libpulseaudio, libudev }:

{ name, version, fullName, buildPhase ? "", rpath ? [], ... }@attrs:

let
  arch = if stdenv.system == "x86_64-linux" then "x86_64" else "x86";
  executable = "${fullName}.${arch}";
  dataDir = "${fullName}_Data";

in stdenv.mkDerivation ({
  name = "${name}-${version}";
  inherit fullName version arch executable dataDir;
  slugName = name;

  nativeBuildInputs = [ makeWrapper ];

  buildPhase = let
    mainRpath = stdenv.lib.makeLibraryPath ([
      stdenv.cc.cc mesa xorg.libX11 xorg.libXcursor xorg.libXrandr
      libpulseaudio libudev
    ] ++ rpath);
  in ''
    runHook preBuild

    patchelf \
      --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
      --set-rpath ${stdenv.lib.escapeShellArg mainRpath} "$executable"

    ${buildPhase}

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    install -vD "$executable" "$out/libexec/$slugName/$slugName"
    ln -s "$out/share/$slugName" "$out/libexec/$slugName/Data"

    mkdir -p "$out/bin"
    makeWrapper "$out/libexec/$slugName/$slugName" "$out/bin/$slugName"

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

    if [ ! -e "$iconpath" ]; then
      echo "Desktop icon not found at $iconpath." >&2
      exit 1
    fi

    runHook postInstall
  '';

  dontStrip = true;
  dontPatchELF = true;
} // removeAttrs attrs [ "name" "version" "fullName" "buildPhase" "rpath" ])

{ stdenv, lib, file, unzip, gcc, makeSetupHook

, withPulseAudio ? true, libpulseaudio ? null
, alsaLib
}:

assert withPulseAudio -> libpulseaudio != null;

{ buildInputs ? []
, nativeBuildInputs ? []
, preUnpack ? ""
, setSourceRoot ? ""
, installCheckPhase ? ""
, runtimeDependencies ? []
, extraSandboxPaths ? [ "$XDG_DATA_HOME" "$XDG_CONFIG_HOME" ]
, ...
}@attrs:

let
  sandboxHook = makeSetupHook {
    substitutions = {
      inherit gcc;
      sandbox_main = ./sandbox.c;
    };
  } ./setup-hooks/make-sandbox.sh;

in stdenv.mkDerivation ({
  buildInputs = [ stdenv.cc.cc ] ++ buildInputs;

  nativeBuildInputs = [
    unzip file ./setup-hooks/auto-patchelf.sh
  ] ++ nativeBuildInputs;

  preUnpack = preUnpack + ''
    mkdir "$name"
    pushd "$name" &> /dev/null
  '';

  # Try to evade tarbombs
  setSourceRoot = ''
    popd &> /dev/null
  '' + lib.optionalString (setSourceRoot == "") ''
    unpackedFiles="$(find "$name" -mindepth 1 -maxdepth 1 -print)"
    if [ $(echo "$unpackedFiles" | wc -l) -gt 1 ]; then
      sourceRoot="$name"
    else
      sourceRoot="$name/''${unpackedFiles##*/}"
    fi
  '';

  # Use ":!*!:" as delimiter as we can consider this highly unlikely to
  # be part of a real path component and we're out of Nix territory, so
  # the path components could contain almost anything.
  extraSandboxPaths = lib.concatStringsSep ":!*!:" extraSandboxPaths;

  runtimeDependencies = let
    deps = lib.singleton alsaLib
        ++ lib.optional withPulseAudio libpulseaudio
        ++ runtimeDependencies;
  in map (dep: dep.lib or dep) deps;

  doInstallCheck = true;

  installCheckPhase = ''
    runHook preInstallCheck

    echo "checking dependencies for libraries and executables" >&2

    local errors="$(
        IFS=$'\n'
        for elf in $(findElfs "$prefix"); do checkElfDep "$elf"; done
    )"

    if [ -n "$errors" ]; then
        echo "$errors" >&2
        exit 1
    fi

    ${installCheckPhase}

    runHook postInstallCheck
  '';

  dontStrip = true;
  dontPatchELF = true;
} // removeAttrs attrs [
  "buildInputs" "nativeBuildInputs" "preUnpack" "setSourceRoot"
  "installCheckPhase" "runtimeDependencies" "extraSandboxPaths"
])

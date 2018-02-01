{ stdenv, lib, file, unzip, buildSandbox, autoPatchelfHook

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
, sandbox ? {}
, ...
}@attrs:

buildSandbox (stdenv.mkDerivation ({
  buildInputs = [ stdenv.cc.cc ] ++ buildInputs;

  nativeBuildInputs = [ unzip autoPatchelfHook ] ++ nativeBuildInputs;

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

  runtimeDependencies = let
    deps = lib.singleton alsaLib
        ++ lib.optional withPulseAudio libpulseaudio
        ++ runtimeDependencies;
  in map (dep: dep.lib or dep) deps;

  doInstallCheck = true;

  installCheckPhase = ''
    runHook preInstallCheck

    echo "checking dependencies for libraries and executables" >&2

    checkElfDep() {
        local errors ldout="$(ldd "$1" 2> /dev/null)"
        if errors="$(echo "$ldout" | grep -F "not found")"; then
            echo -e "Library dependencies missing for $1:\n$errors"
        fi
    }

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
  "installCheckPhase" "runtimeDependencies" "sandbox"
])) (sandbox // {
  paths = let
    paths = sandbox.paths or {};
  in paths // {
    required = paths.required or [ "$XDG_DATA_HOME" "$XDG_CONFIG_HOME" ];
    runtimeVars = [ "LD_LIBRARY_PATH" ] ++ paths.runtimeVars or [];
  };
})

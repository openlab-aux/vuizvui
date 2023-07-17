{ stdenv, lib, file, unzip, buildSandbox, autoPatchelfHook, gogUnpackHook

, withPulseAudio ? true, libpulseaudio ? null
, alsa-lib
}:

assert withPulseAudio -> libpulseaudio != null;

{ buildInputs ? []
, nativeBuildInputs ? []
, preUnpack ? ""
, setSourceRoot ? ""
, runtimeDependencies ? []
, sandbox ? {}
, ...
}@attrs:

buildSandbox (stdenv.mkDerivation ({
  buildInputs = [ stdenv.cc.cc ] ++ buildInputs;

  nativeBuildInputs = [
    unzip autoPatchelfHook gogUnpackHook
  ] ++ nativeBuildInputs;

  preUnpack = preUnpack + ''
    mkdir "$name"
    pushd "$name" &> /dev/null
  '';

  # Try to evade tarbombs
  setSourceRoot = ''
    popd &> /dev/null
  '' + lib.optionalString (setSourceRoot == "") ''
    sourceRoot="$(find "$name" -type d -exec sh -c '
      ndirs="$(find "$1" -mindepth 1 -maxdepth 1 -type d -printf x | wc -m)"
      nelse="$(find "$1" -mindepth 1 -maxdepth 1 ! -type d -printf x | wc -m)"
      ! [ "$ndirs" -eq 1 -a "$nelse" -eq 0 ]
    ' -- {} \; -print -quit)"
  '';

  runtimeDependencies = let
    deps = lib.singleton alsa-lib
        ++ lib.optional withPulseAudio libpulseaudio
        ++ runtimeDependencies;
  in map (dep: dep.lib or dep) deps;

  dontStrip = true;
  dontPatchELF = true;
} // removeAttrs attrs [
  "buildInputs" "nativeBuildInputs" "preUnpack" "setSourceRoot"
  "runtimeDependencies" "sandbox"
])) (sandbox // {
  paths = let
    paths = sandbox.paths or {};
  in paths // {
    required = paths.required or [ "$XDG_DATA_HOME" "$XDG_CONFIG_HOME" ];
    runtimeVars = [ "LD_LIBRARY_PATH" ] ++ paths.runtimeVars or [];
  };
})

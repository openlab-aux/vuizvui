{ stdenv, lib, file }:

{ buildInputs ? []
, nativeBuildInputs ? []
, installCheckPhase ? ""
, ...
}@attrs:

stdenv.mkDerivation ({
  buildInputs = [ stdenv.cc.cc ] ++ buildInputs;

  nativeBuildInputs = [
    file ./setup-hooks/auto-patchelf.sh
  ] ++ nativeBuildInputs;

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
  "buildInputs" "nativeBuildInputs" "installCheckPhase"
])

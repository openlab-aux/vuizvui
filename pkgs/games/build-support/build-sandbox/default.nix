{ stdenv, lib }:

drv: { extraSandboxPaths ? [], ... }@attrs:

stdenv.mkDerivation ({
  name = "${drv.name}-sandboxed";

  src = drv;

  phases = [ "buildPhase" "installPhase" ];

  exportReferencesGraph = [ "sandbox-closure" drv ];

  buildPhase = ''
    runtimeDeps="$(sed -ne '
      p; n; n

      :cdown
      /^0*$/b
      :l; s/0\(X*\)$/X\1/; tl

      s/^\(X*\)$/9\1/; tdone
      ${lib.concatMapStrings (num: ''
        s/${toString num}\(X*\)$/${toString (num - 1)}\1/; tdone
      '') (lib.range 1 9)}

      :done
      y/X/9/
      x; n; p; x
      bcdown
    ' sandbox-closure | sort -u)"

    echo 'static bool setup_app_paths(void) {' > params.c

    for dep in $runtimeDeps; do
      echo 'if (!bind_mount("'"$dep"'", true)) return false;' >> params.c
    done

    ${lib.concatMapStrings (extra: let
      escaped = lib.escapeShellArg (lib.escape ["\\" "\""] extra);
      result = "echo 'if (!extra_mount(\"'${escaped}'\")) return false;'";
    in "${result} >> params.c") extraSandboxPaths}

    echo 'return true; }' >> params.c
    cat params.c
  '';

  installPhase = ''
    mkdir -p "$out/bin"
    for bin in "$src"/bin/*; do
      progname="$(basename "$bin")"
      gcc -g -std=gnu11 -Wall \
        -DWRAPPED_PATH=\""$bin"\" \
        -DWRAPPED_PROGNAME=\""$progname"\" \
        -DPARAMS_FILE=\""$(pwd)/params.c"\" \
        -o "$out/bin/$progname" ${./sandbox.c}
    done
  '';

} // removeAttrs attrs [ "extraSandboxPaths" ])

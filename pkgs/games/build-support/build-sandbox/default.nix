{ stdenv, lib, pkgconfig, nix }:

drv: { extraSandboxPaths ? [], ... }@attrs:

stdenv.mkDerivation ({
  name = "${drv.name}-sandboxed";

  src = ./src;

  inherit drv;

  exportReferencesGraph = [ "sandbox-closure" drv ];

  configurePhase = ''
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
    ' ../sandbox-closure | sort -u)"

    echo 'static bool setup_app_paths(void) {' > params.c

    for dep in $runtimeDeps; do
      echo 'if (!bind_mount("'"$dep"'", true, true)) return false;' >> params.c
    done

    ${lib.concatMapStringsSep "\n" (extra: let
      escaped = lib.escapeShellArg (lib.escape ["\\" "\""] extra);
      result = "echo 'if (!extra_mount(\"'${escaped}'\")) return false;'";
    in "${result} >> params.c") extraSandboxPaths}

    echo 'return true; }' >> params.c
    cat params.c
  '';

  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ nix ];
  makeFlags = [ "BINDIR=${drv}/bin" ];

} // removeAttrs attrs [ "extraSandboxPaths" ])

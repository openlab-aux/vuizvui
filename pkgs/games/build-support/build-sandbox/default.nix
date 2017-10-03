{ stdenv, lib, pkgconfig, nix }:

drv: { extraSandboxPaths ? [], runtimePathVars ? [], ... }@attrs:

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

    echo '#include "setup.h"' > params.c
    echo 'bool setup_app_paths(void) {' >> params.c

    for dep in $runtimeDeps; do
      echo 'if (!bind_mount("'"$dep"'", true, true)) return false;' >> params.c
    done

    ${lib.concatMapStringsSep "\n" (extra: let
      escaped = lib.escapeShellArg (lib.escape ["\\" "\""] extra);
      result = "echo 'if (!extra_mount(\"'${escaped}'\")) return false;'";
    in "${result} >> params.c") extraSandboxPaths}

    echo 'return true; }' >> params.c

    echo 'bool mount_runtime_path_vars(struct query_state *qs) {' >> params.c

    ${lib.concatMapStringsSep "\n" (pathvar: let
      escaped = lib.escapeShellArg (lib.escape ["\\" "\""] pathvar);
      fun = "mount_from_path_var";
      result = "echo 'if (!${fun}(qs, \"'${escaped}'\")) return false;'";
    in "${result} >> params.c") runtimePathVars}

    echo 'return true; }' >> params.c
  '';

  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ nix ];
  makeFlags = [ "BINDIR=${drv}/bin" ];

} // removeAttrs attrs [ "extraSandboxPaths" "runtimePathVars" ])

{ stdenv, lib, pkgconfig, nix }:

drv: { paths ? {}, ... }@attrs:

let
  # Extra paths that are required so they are created prior to bind-mounting.
  pathsRequired    = paths.required    or [];
  # Extra paths that are skipped if they don't exist.
  pathsWanted      = paths.wanted      or [];
  # Paths extracted from PATH-like environment variables, eg. LD_LIBRARY_PATH.
  pathsRuntimeVars = paths.runtimeVars or [];

  # Create code snippets for params.c to add extra_mount() calls.
  mkExtraMountParams = isRequired: lib.concatMapStringsSep "\n" (extra: let
    escaped = lib.escape ["\\" "\""] extra;
    reqBool = if isRequired then "true" else "false";
    code = "if (!extra_mount(\"${escaped}\", ${reqBool})) return false;";
  in "echo ${lib.escapeShellArg code} >> params.c");

in stdenv.mkDerivation ({
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

    ${mkExtraMountParams true  pathsRequired}
    ${mkExtraMountParams false pathsWanted}

    echo 'return true; }' >> params.c

    echo 'bool mount_runtime_path_vars(struct query_state *qs) {' >> params.c

    ${lib.concatMapStringsSep "\n" (pathvar: let
      escaped = lib.escapeShellArg (lib.escape ["\\" "\""] pathvar);
      fun = "mount_from_path_var";
      result = "echo 'if (!${fun}(qs, \"'${escaped}'\")) return false;'";
    in "${result} >> params.c") pathsRuntimeVars}

    echo 'return true; }' >> params.c
  '';

  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ nix ];
  makeFlags = [ "BINDIR=${drv}/bin" ];

} // removeAttrs attrs [ "paths" ])

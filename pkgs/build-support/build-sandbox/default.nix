{ stdenv, lib, pkgconfig, closureInfo, nix, boost, dash }:

drv: { paths ? {}, ... }@attrs:

let
  # Extra paths that are required so they are created prior to bind-mounting.
  pathsRequired    = paths.required    or [];
  # Extra paths that are skipped if they don't exist.
  pathsWanted      = paths.wanted      or [];
  # Paths extracted from PATH-like environment variables, eg. LD_LIBRARY_PATH.
  pathsRuntimeVars = paths.runtimeVars or [];
  # Mount a dash shell in /bin/sh inside the chroot.
  allowBinSh       = attrs.allowBinSh or false;
  # Enable nix builds from within the sandbox.
  # Has to write the full nix store to make the outputs accessible.
  # TODO: get rid of nix & pkg-config if this is enabled (in the Makefile)
  fullNixStore = attrs.fullNixStore or false;

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

  closureInfo = closureInfo {
    rootPaths = lib.singleton drv ++ lib.optional allowBinSh dash;
  };

  configurePhase = ''
    echo '#include "setup.h"' > params.c
    echo 'bool setup_app_paths(void) {' >> params.c

    ${if fullNixStore then ''
      # /nix/var needs to be writable for nix to work inside the sandbox
      echo 'if (!bind_mount("/nix/var", false, true, true)) return false;' \
        >> params.c
      echo 'if (!bind_mount("/nix/store", true, true, true)) return false;' \
        >> params.c

    '' else ''
      for dep in $(< "$closureInfo/store-paths"); do
        echo 'if (!bind_mount("'"$dep"'", true, true, true)) return false;' \
          >> params.c
      done
    ''}

    ${mkExtraMountParams true  pathsRequired}
    ${mkExtraMountParams false pathsWanted}

    echo 'return true; }' >> params.c

   ${lib.optionalString (!fullNixStore) ''
      echo 'bool mount_runtime_path_vars(struct query_state *qs) {' >> params.c

      ${lib.concatMapStringsSep "\n" (pathvar: let
        escaped = lib.escapeShellArg (lib.escape ["\\" "\""] pathvar);
        fun = "mount_from_path_var";
        result = "echo 'if (!${fun}(qs, \"'${escaped}'\")) return false;'";
      in "${result} >> params.c") pathsRuntimeVars}

      echo 'return true; }' >> params.c
    ''}
  '';

  postInstall = ''
    for df in "$drv/share/applications/"*.desktop; do
      mkdir -p "$out/share/applications"
      sed -e 's!'"$drv"'/bin!'"$out"'/bin!g' "$df" \
        > "$out/share/applications/$(basename "$df")"
    done
  '';

  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ boost nix ];
  makeFlags = [ "BINDIR=${drv}/bin" ]
           ++ lib.optional allowBinSh "BINSH_EXECUTABLE=${dash}/bin/dash"
           ++ lib.optional fullNixStore "FULL_NIX_STORE=1";

} // removeAttrs attrs [ "paths" "allowBinSh" ])

{ stdenv, lib, runCommandLocal, coreutils, nix }:

let
  mkNixRemote = proto: let
    hostAndQuery = "nix-remote-build@taalo.headcounter.org?compress=true";
  in "${proto}://${hostAndQuery}";

  remoteCopyEsc = lib.escapeShellArg (mkNixRemote "ssh");
  remoteEsc = lib.escapeShellArg (mkNixRemote "ssh-ng");
  mkNix = cmd: lib.escapeShellArg "${nix}/bin/${cmd}";

  errorOnly = cmd:
    "if ! outerr=\"$(${cmd} 2>&1)\"; then echo \"$outerr\" >&2; exit 1; fi";

  remoteRealize = pre: arg: ''
    ${errorOnly "${mkNix "nix"} copy -s --quiet --to ${remoteCopyEsc} ${arg}"}
    NIX_REMOTE=${remoteEsc} ${pre}${mkNix "nix-store"} -r ${arg}
  '';

  emitScript = content: let
    result = "#!${stdenv.shell}\nset -e\n${content}";
  in "echo -n ${lib.escapeShellArg result}";

in runCommandLocal "taalo-build" {} ''
  mkdir -p "$out/bin"

  ${emitScript (''
    gctmp="$(${lib.escapeShellArg "${coreutils}/bin/mktemp"} -d)"
    trap 'rm -rf "$gctmp"' EXIT
    drv="$(${mkNix "nix-instantiate"} --add-root "$gctmp/drv" --indirect "$@")"
  '' + remoteRealize "" "$drv")} > "$out/bin/taalo-build"

  ${emitScript (remoteRealize "exec " "\"$@\"")} > "$out/bin/taalo-realize"

  chmod +x "$out"/bin/taalo-{build,realize}
''

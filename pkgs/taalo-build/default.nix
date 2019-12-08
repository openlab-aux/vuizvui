{ stdenv, lib, runCommandLocal, coreutils, nixUnstable }:

let
  nixRemote = "ssh-ng://nix-remote-build@taalo.headcounter.org?compress=true";
  remoteEsc = lib.escapeShellArg nixRemote;
  mkNix = cmd: lib.escapeShellArg "${nixUnstable}/bin/${cmd}";

  errorOnly = cmd:
    "if ! outerr=\"$(${cmd} 2>&1)\"; then echo \"$outerr\" >&2; exit 1; fi";

  remoteRealize = pre: arg: ''
    ${errorOnly "${mkNix "nix"} copy -s --quiet --to ${remoteEsc} ${arg}"}
    NIX_REMOTE=${remoteEsc} ${pre}${mkNix "nix-store"} -r ${arg}
  '';

  emitScript = content:
    "echo -n ${lib.escapeShellArg "#!${stdenv.shell}\nset -e\n${content}"}";

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

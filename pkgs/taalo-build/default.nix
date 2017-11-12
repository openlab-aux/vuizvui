{ stdenv, lib, runCommand, nixUnstable }:

let
  nixRemote = "ssh-ng://nix-remote-build@taalo.headcounter.org?compress=true";

  mkScript = cmd: lib.escapeShellArg ''
    #!${stdenv.shell}
    export NIX_REMOTE=${lib.escapeShellArg nixRemote}
    exec ${lib.escapeShellArg nixUnstable}/bin/${cmd} "$@"
  '';

in runCommand "taalo-build" {} ''
  mkdir -p "$out/bin"

  echo -n ${mkScript "nix-build"} > "$out/bin/taalo-build"
  echo -n ${mkScript "nix-store -r"} > "$out/bin/taalo-realize"

  chmod +x "$out"/bin/taalo-{build,realize}
''

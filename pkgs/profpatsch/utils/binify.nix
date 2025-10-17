{ pkgs, lib, ... }:

# Create a store path where the executable `exe`
# is linked to $out/bin/${name}.
# This is useful for e.g. including it as a "package"
# in `buildInputs` of a shell.nix.
#
# For example, if I have the exeutable /nix/store/…-hello,
# I can make it into /nix/store/…-binify-hello/bin/hello
# with `binify { exe = …; name = "hello" }`.
{ exe, name }:
  pkgs.runCommandLocal "binify-${name}" {} ''
    mkdir -p $out/bin
    ln -sT ${lib.escapeShellArg exe} $out/bin/${lib.escapeShellArg name}
  ''

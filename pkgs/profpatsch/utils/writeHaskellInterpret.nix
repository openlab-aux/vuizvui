{ pkgs, lib, ... }:

# TODO: upstream
nameOrPath: { withPackages ? lib.const [] }: content:
  let ghc = pkgs.haskellPackages.ghcWithPackages withPackages; in
  pkgs.writers.makeScriptWriter {
    interpreter = "${ghc}/bin/runhaskell";
    check = pkgs.writers.writeDash "ghc-typecheck" ''
      ln -s "$1" ./Main.hs
      ${ghc}/bin/ghc -fno-code -Wall ./Main.hs
    '';
  } nameOrPath content

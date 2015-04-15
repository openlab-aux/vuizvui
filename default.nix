{ system ? builtins.currentSystem, ... }@args:

with (import <nixpkgs> { inherit system; }).lib;

{
  machines = mapAttrsRecursiveCond (m: !(m ? build)) (path: attrs:
    attrs.build.config.system.build.toplevel
  ) (import ./machines { inherit system; });

  pkgs = import ./pkgs {
    pkgs = import <nixpkgs> args;
  };

  # Inherit upstream lib until we have our own lib.
  lib = import <nixpkgs/lib>;
}

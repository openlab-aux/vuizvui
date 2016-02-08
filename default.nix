{ system ? builtins.currentSystem, ... }@args:

with (import (import ./nixpkgs-path.nix) { inherit system; }).lib;

{
  machines = let
    getBuild = const (getAttr "build");
    allMachines = import ./machines { inherit system; };
  in mapAttrsRecursiveCond (m: !(m ? eval)) getBuild allMachines;

  pkgs = import ./pkgs {
    pkgs = import (import ./nixpkgs-path.nix) args;
  };

  # Inherit upstream lib until we have our own lib.
  lib = import "${import ./nixpkgs-path.nix}/lib";
}

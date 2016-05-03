{ system ? builtins.currentSystem, ... }@args:

with (import (import ./nixpkgs-path.nix) { inherit system; }).lib;

{
  machines = import ./machines {
    inherit system;
  };

  pkgs = import ./pkgs {
    pkgs = import (import ./nixpkgs-path.nix) args;
  };

  lib = import "${import ./nixpkgs-path.nix}/lib" // import ./lib;
}

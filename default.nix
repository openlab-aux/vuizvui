{ system ? builtins.currentSystem, ... }@args:

{
  machines = import ./machines;

  pkgs = import ./pkgs {
    pkgs = import (import ./nixpkgs-path.nix) args;
  };

  lib = import "${import ./nixpkgs-path.nix}/lib" // {
    vuizvui = import ./lib;
  };
}

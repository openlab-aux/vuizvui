{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.pulp
    pkgs.purescript
    pkgs.purenix
    pkgs.nodePackages.purs-tidy
  ];
}

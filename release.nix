{
  skunkworks = (import <nixpkgs/nixos> { configuration = ./entry-skunkworks.nix; }).system;
  eris = (import <nixpkgs/nixos> { configuration = ./entry-eris.nix; }).system;
}

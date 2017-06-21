### THIS FILE IS FOR HYDRA BUILD ONLY! YOU CAN IGNORE THIS FILE IF YOU ARE NOT USING A HYDRA!

let
  supportedSystems = [ "x86_64-linux" ];
  system = "x86_64-linux";

in

{
  skunkworks = (import <nixpkgs/nixos> { configuration = ./entry-skunkworks.nix; }).system;
  titan = (import <nixpkgs/nixos> { configuration = ./entry-titan.nix; }).system;
  eris = (import <nixpkgs/nixos> { configuration = ./entry-eris.nix; }).system;
}

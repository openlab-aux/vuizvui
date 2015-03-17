{ pkgs ? import <nixpkgs> {} }:

let
  callPackage = pkgs.lib.callPackageWith (pkgs // self);

  self = {
    beehive = callPackage ./beehive {};
  };
in self

testFun:

{ system ? builtins.currentSystem
, nixpkgsPath ? import ../nixpkgs-path.nix
, ...
}@args: let

  lib = import "${nixpkgsPath}/lib";

  pkgs = import nixpkgsPath { inherit system; };

  testLib = import "${nixpkgsPath}/nixos/lib/testing-python.nix" {
    inherit pkgs system;
  };

  testArgs = if builtins.isFunction testFun then testFun (args // {
    pkgs = pkgs // {
      vuizvui = import ../pkgs { inherit pkgs; };
    };
    inherit nixpkgsPath;
  }) else testFun;

  nodes = testArgs.nodes or (if testArgs ? machine then {
    inherit (testArgs) machine;
  } else {});

  injectCommon = name: conf: {
    imports = [ conf ] ++ import ../modules/module-list.nix;
  };

  testArgsWithCommon = removeAttrs testArgs [ "machine" ] // {
    nodes = lib.mapAttrs injectCommon nodes;
  };

in testLib.makeTest testArgsWithCommon

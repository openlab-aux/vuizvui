f: { system ? builtins.currentSystem, ... } @ args: let
  nixpkgsPath = import ../nixpkgs-path.nix;

  lib = import "${nixpkgsPath}/lib";

  testLib = import "${nixpkgsPath}/nixos/lib/testing.nix" {
    inherit system;
  };

  pkgs = import nixpkgsPath { inherit system; };

  testArgs = if builtins.isFunction f then f (args // {
    pkgs = pkgs // {
      vuizvui = import ../pkgs { inherit pkgs; };
    };
  }) else f;

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

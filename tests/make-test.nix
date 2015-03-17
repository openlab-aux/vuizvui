f: { system ? builtins.currentSystem, ... } @ args: let
  lib = import <nixpkgs/lib>;

  testLib = import <nixpkgs/nixos/lib/testing.nix> {
    inherit system;
  };

  pkgs = import <nixpkgs> { inherit system; };

  testArgs = if builtins.isFunction f then f (args // {
    pkgs = pkgs // {
      labernix = import ../pkgs { inherit pkgs; };
    };
  }) else f;

  nodes = testArgs.nodes or (if testArgs ? machine then {
    inherit (testArgs) machine;
  } else {});

  injectCommon = name: conf: {
    imports = [ ../common.nix conf ];
  };

  testArgsWithCommon = removeAttrs testArgs [ "machine" ] // {
    nodes = lib.mapAttrs injectCommon nodes;
  };

in testLib.makeTest testArgsWithCommon

f: { system ? builtins.currentSystem, ... } @ args: let
  testLib = import <nixpkgs/nixos/lib/testing.nix> {
    inherit system;
  };

  pkgs = import <nixpkgs> { inherit system; };

  testArgs = if builtins.isFunction f then f (args // {
    pkgs = pkgs // {
      labernix = import ../pkgs { inherit pkgs; };
    };
  }) else f;

  nodes = if testArgs ? machine then {
    inherit (testArgs) machine;
  } else testArgs.nodes;

  injectCommon = name: values: {
    imports = [ ../common.nix values ];
  };

  testArgsWithCommon = removeAttrs testArgs [ "machine" ] // {
    nodes = testLib.mapAttrs injectCommon nodes;
  };

in testLib.makeTest testArgs

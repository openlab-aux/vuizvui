with import <nixpkgs/lib>;

let
  system = "x86_64-linux";

  genMachine = name: cfg: (import <nixpkgs/nixos/lib/eval-config.nix> {
    inherit system;
    modules = [ cfg ];
  }).config.system.build.toplevel;

in {
  machines = mapAttrs genMachine (import ./default.nix).machines;
  tests = {
    heinrich = import ./tests/heinrich.nix { inherit system; };
  };

  pkgs = import ./pkgs {
    pkgs = import <nixpkgs> {
      inherit system;
    };
  };
}

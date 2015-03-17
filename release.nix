with import <nixpkgs/lib>;

let
  genMachine = name: cfg: (import <nixpkgs/nixos/lib/eval-config.nix> {
    system = "x86_64-linux";
    modules = [ cfg ];
  }).config.system.build.toplevel;

in {
  machines = mapAttrs genMachine (import ./default.nix).machines;
}

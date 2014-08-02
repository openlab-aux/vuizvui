with import <nixpkgs/lib>;

{
  machines = mapAttrs (name: configuration: (import <nixpkgs/nixos> {
    inherit configuration;
  }).system) (import ./network.nix);

  tests = {
    i3 = import ./tests/i3.nix { system = "x86_64-linux"; };
  };

  envs = (import ./envs) (import <nixpkgs> {
    config = import ./nixpkgs/config.nix;
  });
}

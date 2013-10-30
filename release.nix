with import <nixpkgs/lib>;

{
  machines = mapAttrs (name: configuration: (import <nixpkgs/nixos> {
    inherit configuration;
  }).system) (import ./network.nix);

  envs = (import ./envs) (import <nixpkgs> {});
}

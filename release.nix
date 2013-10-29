with import <nixpkgs/lib>;

mapAttrs (name: configuration: (import <nixpkgs/nixos> {
  inherit configuration;
}).system) (import ./network.nix)

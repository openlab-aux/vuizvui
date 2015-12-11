{ system ? builtins.currentSystem, nixpkgs ? import ../nixpkgs-path.nix }:

with import "${nixpkgs}/lib";

{
  nixos = let
    upstreamTests = (import "${nixpkgs}/nixos/release.nix" {
      inherit nixpkgs;
    }).tests;
    isTestOrSystems = attr: attr ? test || attr ? ${system};
    cond = attr: !isTestOrSystems attr;
    reduce = attr: if attr ? test then attr else attr.${system};
  in mapAttrsRecursiveCond cond (path: reduce) upstreamTests;

  vuizvui = import ../tests {
    inherit system;
  };
}

{ system ? builtins.currentSystem
, nixpkgs ? import ../nixpkgs-path.nix
, vuizvuiTests ? ../tests
, excludeVuizvuiGames ? false
}:

with import "${nixpkgs}/lib";

{
  nixos = let
    upstreamTests = (import "${nixpkgs}/nixos/release.nix" {
      inherit nixpkgs;
    }).tests;
    isTestOrJob = attr: (attr.type or null) == "derivation" || attr ? test;
    isTestOrSystems = attr: isTestOrJob attr || attr ? ${system};
    cond = attr: !isTestOrSystems attr;
    reduce = attr: if isTestOrJob attr then attr else attr.${system};
  in mapAttrsRecursiveCond cond (path: reduce) upstreamTests;

  vuizvui = removeAttrs (import vuizvuiTests {
    inherit system;
    nixpkgsPath = nixpkgs;
  }) (optional excludeVuizvuiGames "games");
}

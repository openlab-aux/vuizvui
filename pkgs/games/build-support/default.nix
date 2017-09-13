{ callPackage, ... }:

{
  buildGame = callPackage ./build-game.nix {};
  buildUnity = callPackage ./build-unity.nix {};
}

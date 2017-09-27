{ config, callPackage, ... }:

{
  buildGame = callPackage ./build-game.nix {
    withPulseAudio = config.pulseaudio or true;
  };
  buildSandbox = callPackage ./build-sandbox {};
  buildUnity = callPackage ./build-unity.nix {};
}

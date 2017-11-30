{ config, callPackage, ... }:

{
  buildGame = callPackage ./build-game.nix {
    withPulseAudio = config.pulseaudio or true;
  };
  # XXX: Pass through from parent scope!
  buildSandbox = callPackage ../../build-support/build-sandbox {};
  buildUnity = callPackage ./build-unity.nix {};
}

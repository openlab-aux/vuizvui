{ config, callPackage, callPackages, ... }:

{
  buildGame = callPackage ./build-game.nix {
    withPulseAudio = config.pulseaudio or true;
  };
  buildUnity = callPackage ./build-unity.nix {};
  monogamePatcher = callPackage ./monogame-patcher {};

  inherit (callPackages ./setup-hooks {}) gogUnpackHook;
}

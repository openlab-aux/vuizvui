{ system ? builtins.currentSystem, ... }:

let
  callTest = path: import ./make-test.nix (import path) {
    inherit system;
  };

in {
  games = {
    sandbox = callTest ./games/sandbox.nix;
    starbound = callTest ./games/starbound.nix;
  };
  programs = {
    gnupg = callTest ./programs/gnupg;
  };
  system = {
    kernel.bfq = callTest ./system/kernel/bfq.nix;
  };
}

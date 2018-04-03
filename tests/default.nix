{ system ? builtins.currentSystem
, nixpkgsPath ? import ../nixpkgs-path.nix
, ...
}:

let
  callTest = path: import ./make-test.nix (import path) {
    inherit system nixpkgsPath;
  };

in {
  games = {
    starbound = callTest ./games/starbound.nix;
  };
  programs = {
    gnupg = callTest ./programs/gnupg;
  };
  sandbox = callTest ./sandbox.nix;
  system = {
    kernel.bfq = callTest ./system/kernel/bfq.nix;
  };
}

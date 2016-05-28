{ system ? builtins.currentSystem, ... }:

let
  callTest = path: import ./make-test.nix (import path) {
    inherit system;
  };

in {
  games = {
    starbound = callTest ./games/starbound.nix;
  };
  programs = {
    gnupg = callTest ./programs/gnupg;
  };
  richi235 = {
    # Currently broken
    #multipath-vpn = callTest ./richi235/multipath-vpn.nix;
  };
}

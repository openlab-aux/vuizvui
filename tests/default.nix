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
  system = {
    kernel.bfq = callTest ./system/kernel/bfq.nix;
  };
  richi235 = {
    # Currently broken
    #multipath-vpn = callTest ./richi235/multipath-vpn.nix;
  };
}

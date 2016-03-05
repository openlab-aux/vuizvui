{ system ? builtins.currentSystem, ... }:

let
  callTest = path: import ./make-test.nix (import path) {
    inherit system;
  };

in {
  aszlig = {
    i3 = callTest ./aszlig/i3.nix;
  };
  richi235 = {
    # Currently broken
    #multipath-vpn = callTest ./richi235/multipath-vpn.nix;
  };
}

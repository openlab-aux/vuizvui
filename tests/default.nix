{ system ? builtins.currentSystem, ... }:

let
  callTest = path: import ./make-test.nix (import path) {
    inherit system;
  };

in {
  aszlig = {
    i3 = callTest ./aszlig/i3.nix;
  };
  labnet = {
    heinrich = callTest ./labnet/heinrich.nix;
  };
}

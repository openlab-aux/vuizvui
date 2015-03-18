{ system ? builtins.currentSystem, ... }:

let
  callMachine = path: rec {
    config = import path;
    build = import <nixpkgs/nixos/lib/eval-config.nix> {
      inherit system;
      modules = [ config ] ++ import ../modules/module-list.nix;
    };
  };

in {
  aszlig = {
    dnyarri   = callMachine ./aszlig/dnyarri.nix;
    mmrnmhrm  = callMachine ./aszlig/mmrnmhrm.nix;
    arilou    = callMachine ./aszlig/arilou.nix;
    kzerza    = callMachine ./aszlig/kzerza.nix;
    tishtushi = callMachine ./aszlig/tishtushi.nix;
  };
  labnet = {
    heinrich = callMachine ./labnet/heinrich.nix;
    labtop   = callMachine ./labnet/labtop.nix;
  };
  misc = {
    mailserver = callMachine ./misc/mailserver.nix;
  };
}

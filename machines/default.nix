{ system ? builtins.currentSystem, ... }:

let
  callMachine = import ../lib/call-machine.nix;
in {
  aszlig = {
    dnyarri   = callMachine ./aszlig/dnyarri.nix {};
    mmrnmhrm  = callMachine ./aszlig/mmrnmhrm.nix {};
    arilou    = callMachine ./aszlig/arilou.nix {};
    kzerza    = callMachine ./aszlig/kzerza.nix {};
    tishtushi = callMachine ./aszlig/tishtushi.nix {};
    managed = {
      notsure = callMachine ./aszlig/managed/notsure.nix {};
    };
  };
  labnet = {
    heinrich = callMachine ./labnet/heinrich.nix {};
    labtop   = callMachine ./labnet/labtop.nix {};
  };
  profpatsch = {
    katara = callMachine ./profpatsch/katara.nix {};
  };
  misc = {
    mailserver = callMachine ./misc/mailserver.nix {};
  };
  sternenseemann = {
    fliewatuet = callMachine ./sternenseemann/fliewatuet.nix {};
  }
}

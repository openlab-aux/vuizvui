with import ../lib;

{
  aszlig = {
    dnyarri   = callMachine ./aszlig/dnyarri.nix {};
    mmrnmhrm  = callMachine ./aszlig/mmrnmhrm.nix {};
    arilou    = callMachine ./aszlig/arilou.nix {};
    kzerza    = callMachine ./aszlig/kzerza.nix {};
    meshuggah = callMachine ./aszlig/meshuggah.nix {};
    tishtushi = callMachine ./aszlig/tishtushi.nix {};
    managed = {
      brawndo = callMachine ./aszlig/managed/brawndo.nix {};
      notsure = callMachine ./aszlig/managed/notsure.nix {};
      tyree   = callMachine ./aszlig/managed/tyree.nix {};
    };
  };
  labnet = {
    labtops = callNetwork ./labnet/labtops.nix {};
  };
  profpatsch = {
    katara = callMachine ./profpatsch/katara.nix {};
  };
  misc = {
    mailserver = callMachine ./misc/mailserver.nix {};
  };
  sternenseemann = {
    fliewatuet   = callMachine ./sternenseemann/fliewatuet.nix {};
    schnurrkadse = callMachine ./sternenseemann/schnurrkadse.nix {};
    schaf        = callMachine ./sternenseemann/schaf.nix {};
  };
}

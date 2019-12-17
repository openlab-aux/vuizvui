with import ../lib;

{
  aszlig = {
    dnyarri   = callMachine ./aszlig/dnyarri.nix {};
    meshuggah = callMachine ./aszlig/meshuggah.nix {};
    tishtushi = callMachine ./aszlig/tishtushi.nix {};
    managed = {
      brawndo = callMachine ./aszlig/managed/brawndo.nix {};
      shakti  = callMachine ./aszlig/managed/shakti.nix {};
      tyree   = callMachine ./aszlig/managed/tyree.nix {};
    };
  };
  devhell = {
    eir       = callMachine devhell/eir.nix {};
    sigrun = callMachine devhell/sigrun.nix {};
    hildr      = callMachine devhell/hildr.nix {};
    gunnr      = callMachine devhell/gunnr.nix {};
  };
  profpatsch = {
    shiki = callMachine ./profpatsch/shiki.nix {};
    haku   = callMachine ./profpatsch/haku.nix {};
    mikiya = callMachine ./profpatsch/mikiya.nix {};
  };
  misc = {
    mailserver = callMachine ./misc/mailserver.nix {};
  };
}

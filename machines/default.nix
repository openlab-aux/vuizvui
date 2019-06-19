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
    eris       = callMachine devhell/eris.nix {};
    skunkworks = callMachine devhell/skunkworks.nix {};
    titan      = callMachine devhell/titan.nix {};
    gunnr      = callMachine devhell/gunnr.nix {};
  };
  openlab = {
    hannswurscht = callMachine ./openlab/hannswurscht.nix {};
  };
  profpatsch = {
    shiki = callMachine ./profpatsch/shiki.nix {};
    haku   = callMachine ./profpatsch/haku.nix {};
    mikiya = callMachine ./profpatsch/mikiya.nix {};
  };
  misc = {
    mailserver = callMachine ./misc/mailserver.nix {};
  };
  sternenseemann = {
    schnurrkadse = callMachine ./sternenseemann/schnurrkadse.nix {};
  };
}

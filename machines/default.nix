with import ../lib;

{
  aszlig = {
    dnyarri   = callMachine ./aszlig/dnyarri.nix {};
    meshuggah = callMachine ./aszlig/meshuggah.nix {};
    slylandro = callMachine ./aszlig/slylandro.nix {};
    managed = {
      brawndo = callMachine ./aszlig/managed/brawndo.nix {};
      tyree   = callMachine ./aszlig/managed/tyree.nix {};
    };
  };
  devhell = {
    eir       = callMachine devhell/eir.nix {};
    sigrun    = callMachine devhell/sigrun.nix {};
    hildr     = callMachine devhell/hildr.nix {};
    gunnr     = callMachine devhell/gunnr.nix {};
    herja     = callMachine devhell/herja.nix {};
  };
  profpatsch = {
    shiki = callMachine ./profpatsch/shiki.nix {};
    haku = callMachine ./profpatsch/haku.nix {};
    legosi = callMachine ./profpatsch/legosi.nix {};
    leguin = callMachine ./profpatsch/leguin.nix {};
    # mikiya = callMachine ./profpatsch/mikiya.nix {};
  };
  sternenseemann = {
    ludwig = callMachine ./sternenseemann/ludwig.nix {};
    wolfgang = callMachine ./sternenseemann/wolfgang.nix {};
  };
  misc = {
    mailserver = callMachine ./misc/mailserver.nix {};
  };
}

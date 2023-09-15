with import ../lib;

{
  aszlig = {
    dnyarri   = callMachine ./aszlig/dnyarri.nix {};
    meshuggah = callMachine ./aszlig/meshuggah.nix {};
    slylandro = callMachine ./aszlig/slylandro.nix {};
  };
  devhell = {
    eir       = callMachine devhell/eir.nix {};
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
}

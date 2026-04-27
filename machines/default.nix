with import ../lib;

in
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
  sternenseemann = {
    wolfgang = callMachine ./sternenseemann/wolfgang.nix {};
  };
}

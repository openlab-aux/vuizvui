with import ../lib;

{
  aszlig = {
    dnyarri   = callMachine ./aszlig/dnyarri {};
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

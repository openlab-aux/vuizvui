with import ../lib;

let
  # Module selection for profpatsch machines (nixpkgs stable compatibility)
  profpatschModules = {
    _moduleSelection = [
      # Core modules (needed for vuizvui basics)
      "./core/common.nix"
      "./core/licensing.nix"
      "./core/tests.nix"

      "./hardware/thinkpad.nix"
      "./hardware/t14s.nix"
      "./hardware/tolino.nix"

      "./programs/gnupg"
      "./programs/fish/fasd.nix"

      "./services/upower-minimal.nix"

      # All profpatsch user modules
      "./user/profpatsch/"
    ];
  };

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
  profpatsch = {
    rolery = callMachine ./profpatsch/rolery.nix profpatschModules;
    haku = callMachine ./profpatsch/haku.nix profpatschModules;
    legosi = callMachine ./profpatsch/legosi.nix profpatschModules;
    leguin = callMachine ./profpatsch/leguin.nix profpatschModules;
  };
  sternenseemann = {
    wolfgang = callMachine ./sternenseemann/wolfgang.nix {};
  };
}

{ system ? builtins.currentSystem, ... }:

let
  callMachine = import ../lib/call-machine.nix;
  callMachines = path: args: let
    machines = import path;
  in with builtins; listToAttrs (map (name: {
    inherit name;
    value = callMachine machines.${name} ({
      extraConfig = { lib, ... }: {
        imports = lib.singleton (args.extraConfig or {});
        networking.hostName = lib.mkOverride 900 name;
      };
    } // removeAttrs args [ "extraConfig" ]);
  }) (attrNames machines));
  nixpkgs = import (import ../nixpkgs-path.nix) {};
in {
  aszlig = {
    dnyarri   = callMachine ./aszlig/dnyarri.nix {};
    mmrnmhrm  = callMachine ./aszlig/mmrnmhrm.nix {};
    arilou    = callMachine ./aszlig/arilou.nix {};
    kzerza    = callMachine ./aszlig/kzerza.nix {};
    tishtushi = callMachine ./aszlig/tishtushi.nix {};
    managed = {
      notsure = callMachine ./aszlig/managed/notsure.nix {};
      tyree   = callMachine ./aszlig/managed/tyree.nix {};
    };
  };
  labnet = {
    labtops = import ./labnet/labtop.nix { inherit (nixpkgs) lib; };
  };
  profpatsch = {
    katara = callMachine ./profpatsch/katara.nix {};
  };
  misc = {
    mailserver = callMachine ./misc/mailserver.nix {};
  };
  sternenseemann = {
    fliewatuet = callMachine ./sternenseemann/fliewatuet.nix {};
  };
}

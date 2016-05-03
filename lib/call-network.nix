path: args:

with import "${import ../nixpkgs-path.nix}/lib";

let
  machineAttrs = import path;

  mkMachine = name: {
    inherit name;
    value = import ./call-machine.nix machineAttrs.${name} ({
      extraConfig = { lib, ... }: {
        imports = lib.singleton (args.extraConfig or {});
        networking.hostName = lib.mkOverride 900 name;
        _module.args.nodes = mapAttrs (const (m: m ? eval)) machines;
      };
    } // removeAttrs args [ "extraConfig" ]);
  };

  machines = listToAttrs (map mkMachine (attrNames machineAttrs));

in machines

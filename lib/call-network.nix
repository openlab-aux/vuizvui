path: args:

let
  machineAttrs = import path;
  machineNames = builtins.attrNames machineAttrs;

  mkMachine = name: {
    inherit name;
    value = import ./call-machine.nix machineAttrs.${name} ({ lib, ... }: {
      imports = lib.singleton (args.extraConfig or {});
      networking.hostName = lib.mkOverride 900 name;
      _module.args.nodes = lib.mapAttrs (lib.const (m: m ? eval)) machines;
    } // removeAttrs args [ "extraConfig" ]);
  };

  machines = builtins.listToAttrs (map mkMachine machineNames);

in machines

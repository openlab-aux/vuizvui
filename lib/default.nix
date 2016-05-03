rec {
  callMachine = import ./call-machine.nix;

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
}

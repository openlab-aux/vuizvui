path: args:

with builtins;

let
  machines = import path;
in listToAttrs (map (name: {
  inherit name;
  value = import ./call-machine.nix machines.${name} ({
    extraConfig = { lib, ... }: {
      imports = lib.singleton (args.extraConfig or {});
      networking.hostName = lib.mkOverride 900 name;
    };
  } // removeAttrs args [ "extraConfig" ]);
}) (attrNames machines))

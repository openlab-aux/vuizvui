path: args:

let
  __withPkgsPath = nixpkgs: let
    lib = import "${nixpkgs}/lib";

    machineAttrs = import path;

    mkMachine = name: {
      inherit name;
      value = (import ./call-machine.nix machineAttrs.${name} ({ lib, ... }: {
        imports = lib.singleton (args.extraConfig or {});
        networking.hostName = lib.mkOverride 900 name;
        _module.args.nodes = lib.mapAttrs (lib.const (m: m ? eval)) machines;
      } // removeAttrs args [ "extraConfig" ])).__withPkgsPath nixpkgs;
    };

    machines = lib.listToAttrs (map mkMachine (lib.attrNames machineAttrs));

  in machines;

in __withPkgsPath (import ../nixpkgs-path.nix) // {
  inherit __withPkgsPath;
}

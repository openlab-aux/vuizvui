{
  description = "Vuizvui";

  outputs = _: {
    nixosModules.vuizvui = import modules/module-list.nix;
    overlay = _: pkgs: { vuizvui = import ./pkgs { inherit pkgs; }; };
  };
}

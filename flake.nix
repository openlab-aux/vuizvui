{
  description = "Vuizvui";

  outputs = { self, nixpkgs }: {
    nixosModules.vuizvui = import modules/module-list.nix;
    overlay = _: pkgs: { vuizvui = import ./pkgs { inherit pkgs; }; };

    legacyPackages = nixpkgs.lib.mapAttrs (_: pkgs: let
      inherit (pkgs.extend self.overlay) vuizvui;
    in vuizvui) nixpkgs.legacyPackages;
  };
}

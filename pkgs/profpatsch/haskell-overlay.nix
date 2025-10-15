{ pkgs }:

let hlib = pkgs.haskell.lib.compose;


in pkgs.haskellPackages.override {
  overrides = self: super: {

    # https://github.com/NixOS/nixpkgs/pull/328896
    #purescript = pkgs.lib.pipe super.purescript [
    #  #(hlib.appendPatches [./purescript-import-fix.patch ])
    #  hlib.unmarkBroken
    #];
    purenix = pkgs.lib.pipe super.purenix [
	    (hlib.overrideCabal (drv: {
	      patches = [
            # ./purenix-import-fix.patch
            ./0001-chore-adjust-to-purescript-0.15.15-corefn-changes.patch
          ];
	    }))
    ];
  };
}

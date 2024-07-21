{ pkgs }:

let hlib = pkgs.haskell.lib.compose;


in pkgs.haskellPackages.override {
  overrides = self: super: {

    # https://github.com/NixOS/nixpkgs/pull/328896
    purescript = pkgs.lib.pipe super.purescript [
      (hlib.appendPatches [./purescript-import-fix.patch ])
      hlib.unmarkBroken
    ];
    purenix = pkgs.lib.pipe super.purenix [
      (hlib.appendPatches [
        ./purenix-import-fix.patch
        ./purenix-purescript-0_15_12.patch
      ])
      hlib.unmarkBroken
    ];
  };
}

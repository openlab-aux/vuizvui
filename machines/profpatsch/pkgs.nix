{ pkgs, lib }:

let
  addRuntimeDeps = drv: ds: drv.overrideDerivation (old: {
    propagatedNativeBuildInputs = old.propagatedNativeBuildInputs ++ ds;
  });

in
with pkgs;
{

  offlineimap = addRuntimeDeps offlineimap [ pythonPackages.pygpgme ];

  taffybar = taffybar.override {
    ghcWithPackages = (haskellPackages.override {
      overrides = _: super: {
        taffybar = super.taffybar.overrideDerivation (old: {
          name = old.name + "foo";
          patches = (old.patches or []) ++ [ ./taffybar.patch ];
        });
      };
    }).ghcWithPackages;
  };

}

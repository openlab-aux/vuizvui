{ pkgs, lib }:

let

  addPythonRuntimeDeps = drv: deps: drv.overrideDerivation (old: {
    propagatedNativeBuildInputs = old.propagatedNativeBuildInputs ++ deps;
  });

in
with pkgs;
{

  offlineimap = addPythonRuntimeDeps offlineimap [ pythonPackages.pygpgme ];

  taffybar = taffybar.override {
    ghcWithPackages = (haskellPackages.override {
      overrides = _: super: {
        taffybar = super.taffybar.overrideDerivation (old: {
          name = old.name + "foo";
          patches = (old.patches or []) ++ [ ./taffybar.patch ];
          postPatch = old.postPathPhase or "" + ''
            patch -R ${./taffybar-color.patch}
          '';
        });
      };
    }).ghcWithPackages;
  };

  # sent = pkgs:q.sent.override { patches = [ ./sent-bg.patch ]; };
  inherit sent;

  # mpv = pkgs.mpv.override { scripts = [ pkgs.mpvScripts.convert ]; };
  inherit mpv;

  beets = pkgs.beets.override { enableAlternatives = true; };

}

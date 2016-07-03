{ pkgs, lib }:

with pkgs;
let

  addPythonRuntimeDeps = drv: deps: drv.overrideDerivation (old: {
    propagatedNativeBuildInputs = old.propagatedNativeBuildInputs ++ deps;
  });

  # containered = name: packages: users: { ... }:
  #   {
  #     containers."${name}" = {
  #       config = {
  #         environment.systemPackages = packages;
  #         users.users = users;
  #         services.sshd.enable = true;
  #       };
  #       privateNetwork = true;
  #       localAddress = "127.0.0.2";
  #     };
  #     nixpkgs.config.allowUnfree = true;
  #   };

  # pkgs

  offlineimap = addPythonRuntimeDeps pkgs.offlineimap [ pkgs.pythonPackages.pygpgme ];

  taffybar = pkgs.taffybar.override {
    ghcWithPackages = (pkgs.haskellPackages.override {
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

  sent = pkgs.sent.override { patches = [ ./sent-bg.patch ]; };

  mpv = pkgs.mpv.override { scripts = [ pkgs.mpvScripts.convert ]; };

  beets = pkgs.beets.override { enableAlternatives = true; };

  git-annex = pkgs.gitAndTools.git-annex.overrideDerivation (old: {
      buildInputs = old.buildInputs ++ [ pkgs.makeWrapper ];
      postFixup = ''
        wrapProgram $out/bin/git-annex --prefix PATH ":" "${pkgs.lsof}/bin";
      '';
  });

  poezio = pkgs.python34Packages.poezio;

  vim = vim_configurable;

in
{ inherit taffybar offlineimap sent mpv beets git-annex poezio vim; }

{ pkgs, lib, myLib }:

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

  taffybar = pkgs.taffybar.override {
    ghcWithPackages = (pkgs.haskellPackages.override {
      overrides = _: super: {
        taffybar = super.taffybar.overrideDerivation (old: {
          name = old.name + "foo";
          patches = (old.patches or []) ++ [ ./patches/taffybar.patch ];
          postPatch = old.postPathPhase or "" + ''
            patch -R ${./patches/taffybar-color.patch}
          '';
        });
      };
    }).ghcWithPackages;
  };

  mpv = pkgs.mpv.override { scripts = [ pkgs.mpvScripts.convert ]; };

  beets = pkgs.beets.override { enableAlternatives = true; };

  # git-annex = hplts.git-annex.overrideDerivation (old: {
  #     buildInputs = old.buildInputs ++ [ pkgs.makeWrapper ];
  #     postFixup = ''
  #       wrapProgram $out/bin/git-annex --prefix PATH ":" "${getBin pkgs.lsof}/bin";
  #     '';
  # });

  poezio = pkgs.python34Packages.poezio;

  vim = vim_configurable;

  fast-init = pkgs.haskellPackages.callPackage (import "${(pkgs.fetchFromGitHub {
    owner = "Profpatsch";
    repo = "fast-init";
    # TODO fix version
    rev = "master";
    sha256 = "03006xzs250knzcyr6j564kn9jf2a6cp3mxkpqsqmmyp6v28w90z";
  })}/overrides.nix") {};

  xmpp-client = pkgs.callPackage (import ./xmpp-client.nix myLib.philip.home "irc/xmppOla.wtf") { inherit (pkgs) xmpp-client; };

  # searx = pkgs.searx.overrideAttrs (old: {
  #   propagatedBuildInputs = old.propagatedBuildInputs ++ [ pythonPackages.pyxdg ];
  #   patches = old.patches or [] ++ [
  #     ./patches/searx-secret-key.patch
  #     ./patches/searx-rm-soundcloud.patch
  #   ];
  #   # xdg.BaseDirectory.save_cache_path() will try to create leading dirs, but
  #   # within the builder we don't have a writable home directory.
  #   preCheck = (old.preCheck or "") + ''
  #     export XDG_CACHE_HOME="$TMPDIR/cache"
  #   '';
  # });

  # A ghci with some sane default packages in scope, & hoogle
  saneGhci = haskellPackages.ghcWithHoogle (h: with h; [ protolude pretty-show ]);

  # # not upstream-compatible yet
  # nix-gen = haskellPackages.mkDerivation {
  #   pname = "nix-gen";
  #   version = "0.0.1";
  #   license = lib.licenses.gpl3;
  #   isExecutable = true;
  #   src = /home/philip/code/nix/nix-gen;
  #   buildDepends = with haskellPackages; [ hnix ansi-wl-pprint protolude data-fix ];
  # };

  pyrnotify =
    let src = pkgs.fetchFromGitHub {
          owner = "arnottcr";
          repo = "weechat-pyrnotify";
          rev = "5063ba19b5ba7ba3d4ecb2a76ad9e4b7bf89964b";
          sha256 = "0r07glz7hkmcnp2vl4dy24i9vfsa9shm7k4q0jb47881z0y2dm2p";
        };
        notify-send = "${pkgs.libnotify.overrideAttrs (old: {
          patches = old.patches or [] ++ [ ./patches/libnotify.patch ];
        })}/bin/notify-send";
    in pkgs.runCommand "pyrnotify.py" {} ''
      substitute "${src}/pyrnotify.py" $out \
        --replace 'notify-send' '${notify-send}'
    '';


in
{ inherit
    taffybar
    mpv
    beets
    poezio
    vim
    fast-init
    xmpp-client
    saneGhci
    /*nix-gen*/
    /*searx*/
    pyrnotify
    ;
}

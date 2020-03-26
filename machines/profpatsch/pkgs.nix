{ pkgs, lib, myLib
, withUnfree ? false, unfreeAndNonDistributablePkgs ? null
}:

assert withUnfree -> unfreeAndNonDistributablePkgs != null;

let

  mpv = pkgs.mpv-with-scripts.override {
    scripts = [ pkgs.mpvScripts.convert ];
  };

  beets = pkgs.beets.override { enableAlternatives = true; };

  vim = pkgs.vim_configurable;

  fast-init = pkgs.haskellPackages.callPackage (import "${(pkgs.fetchFromGitHub {
    owner = "Profpatsch";
    repo = "fast-init";
    # TODO fix version
    rev = "master";
    sha256 = "03006xzs250knzcyr6j564kn9jf2a6cp3mxkpqsqmmyp6v28w90z";
  })}/overrides.nix") {};

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
    in pkgs.runCommandLocal "pyrnotify.py" {} ''
      substitute "${src}/pyrnotify.py" $out \
        --replace 'notify-send' '${notify-send}'
    '';

  zoomboxed = pkgs.vuizvui.buildSandbox unfreeAndNonDistributablePkgs.zoom-us {
    paths.required = [
      "$XDG_CONFIG_HOME/zoomus.conf"
      "$XDG_CONFIG_HOME/.zoom"
    ];
    allowBinSh = true;
  };

in
{ inherit
    mpv
    beets
    vim
    # fast-init
    pyrnotify
    zoomboxed
    ;
}

{ pkgs, lib, myLib
, withUnfree ? false, unfreeAndNonDistributablePkgs ? null
}:

assert withUnfree -> unfreeAndNonDistributablePkgs != null;

let

  mpv = pkgs.mpv.override {
    scripts = [
      pkgs.mpvScripts.videoclip
      pkgs.mpvScripts.mpris
    ];
  };

  beets = pkgs.beets.override { enableAlternatives = true; };

  # vim = pkgs.vim-full;
  vim = pkgs.vim;

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

  mumble =
    pkgs.mumble;
    # speechdSupport is broken
    # pkgs.mumble.override { speechdSupport = true; };

  # The implementation is buggy and produces an error like
  # Name Error (Connection ":1.4380" is not allowed to own the service "org.linuxtv.Zbar" due to security policies in the configuration file)
  # for every scanned code.
  zbar = zbar.override { enableDbus = false; };

in
{ inherit
    mpv
    beets
    vim
    # fast-init
    pyrnotify
    zoomboxed
    mumble
    ;
}

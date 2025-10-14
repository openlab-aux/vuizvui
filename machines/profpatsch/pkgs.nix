{ pkgs, lib, myLib
, withUnfree ? false, unfreeAndNonDistributablePkgs ? null
}:

assert withUnfree -> unfreeAndNonDistributablePkgs != null;

let

  mpv = pkgs.mpv.override {
    scripts = [
      pkgs.mpvScripts.videoclip
      pkgs.mpvScripts.mpris
      pkgs.vuizvui.profpatsch.lyric.mpv-script
      pkgs.vuizvui.profpatsch.lyric.timing-mpv-script
    ];
  };

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
    zoomboxed
    ;
}

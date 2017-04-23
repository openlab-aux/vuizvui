{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.vuizvui.user.aszlig.programs.gajim;

  gtkTheme = pkgs.writeText "gajim.gtkrc" ''
    style "default" {
      fg[NORMAL] = "#d5faff"
      fg[ACTIVE] = "#fffeff"
      fg[SELECTED] = "#fffeff"
      fg[INSENSITIVE] = "#85aaaf"
      fg[PRELIGHT] = "#d7f2ff"

      text[NORMAL] = "#fffefe"
      text[ACTIVE] = "#fffeff"
      text[SELECTED] = "#fffeff"
      text[INSENSITIVE] = "#85aaaf"
      text[PRELIGHT] = "#d7f2ff"

      bg[NORMAL] = "#0f4866"
      bg[ACTIVE] = "#0c232e"
      bg[SELECTED] = "#005a56"
      bg[INSENSITIVE] = "#103040"
      bg[PRELIGHT] = "#1d5875"

      base[NORMAL] = "#0c232e"
      base[ACTIVE] = "#0f4864"
      base[SELECTED] = "#005a56"
      base[INSENSITIVE] = "#103040"
      base[PRELIGHT] = "#1d5875"
    }

    class "GtkWidget" style "default"

    gtk-enable-animations = 0
  '';

  gajimPatched = let
    o = pkgs.vuizvui.aszlig.gajim.drvAttrs;
  in pkgs.stdenv.mkDerivation (pkgs.vuizvui.aszlig.gajim.drvAttrs // {
    patches = (o.patches or []) ++ singleton (pkgs.substituteAll {
      src = ./config.patch;
      nix_config = pkgs.writeText "gajim.config" (import ./config.nix lib);
    });
    propagatedBuildInputs = (o.propagatedBuildInputs or []) ++ [
      pkgs.pythonPackages.python-axolotl
    ];
    postPatch = (o.postPatch or "") + ''
      # Disable a few config-related and GUI tests that won't work with our
      # patches.
      sed -i -e '/integration\.test_roster/d' \
             -e '/unit.test_gui_interface/d' \
             test/runtests.py

      sed -i -e '/^export/i export GTK2_RC_FILES="${gtkTheme}"' \
        scripts/gajim.in
    '';
  });

in {
  options.vuizvui.user.aszlig.programs.gajim = {
    enable = mkEnableOption "aszlig's Gajim";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ gajimPatched ];
  };
}

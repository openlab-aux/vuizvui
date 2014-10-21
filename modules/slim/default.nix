{ pkgs, config, lib, ... }:

with lib;

let
  cfg = config.vuizvui.slim;
  randrHeads = config.services.xserver.xrandrHeads;
in {
  options.vuizvui.slim.enable = mkEnableOption "Vuizvui SLiM";

  config.services.xserver.displayManager.slim = mkIf cfg.enable {
    enable = true;
    defaultUser = "aszlig";
    theme = pkgs.stdenv.mkDerivation {
      name = "nixos-theme-vuizvui";
      src = pkgs.slimThemes.nixosSlim;
      phases = [ "unpackPhase" "patchPhase" "installPhase" ];
      patchPhase = let
        headFactor = if randrHeads == [] then 1 else lib.length randrHeads;
        centerLeft = 100 / (headFactor * 2);
      in ''
        ${pkgs.imagemagick}/bin/mogrify \
          -fill '#080010' -draw 'color 0,0 reset' \
          share/slim/themes/nixos-slim-testing/background.png
        ${pkgs.imagemagick}/bin/mogrify \
          -negate -region 100x110+0+0 -negate -fill white -colorize 20% \
          share/slim/themes/nixos-slim-testing/panel.png
        sed -i \
          -e 's/^\([a-z_]\+_x[^0-9]*\)[0-9]\+%/\1${toString centerLeft}%/' \
          share/slim/themes/nixos-slim-testing/slim.theme
        cat >> share/slim/themes/nixos-slim-testing/slim.theme <<EOF
        session_x      ${toString centerLeft}%
        msg_color      #ffffff
        username_color #ffffff
        password_color #ffffff
        input_color    #ffffff
        EOF
      '';
      installPhase = ''
        cp -R share/slim/themes/nixos-slim-testing "$out"
      '';
    };
  };
}

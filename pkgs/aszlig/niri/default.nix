{ lib, replaceVarsWith, niri, brightnessctl, systemd, catppuccin-cursors
, xwayland-satellite
}:

niri.overrideAttrs (drv: {
  patches = (drv.patches or []) ++ lib.singleton (replaceVarsWith {
    name = "config.patch";
    src = ./config.patch.in;

    replacements = {
      brightnessctl = "${lib.getBin brightnessctl}/bin/brightnessctl";
      systemctl = "${lib.getBin systemd}/bin/systemctl";
      xcursorPath = "${catppuccin-cursors.frappeLavender}/share/icons";

      xwaylandSatellite = "${
        lib.getBin xwayland-satellite
      }/bin/xwayland-satellite";

      # These values are special identifiers for wpctl which we need to keep
      # unaltered.
      DEFAULT_AUDIO_SINK = null;
      DEFAULT_AUDIO_SOURCE = null;
    };
  });

  postInstallCheck = (drv.postInstallCheck or "") + ''
    "$out/bin/niri" validate --config resources/default-config.kdl
  '';
})

{ lib, anyrun, replaceVarsWith, kitty, dash, systemd }:

anyrun.overrideAttrs (drv: {
  patches = (drv.patches or []) ++ lib.singleton (replaceVarsWith {
    name = "config.patch";
    src = ./config.patch.in;

    replacements = {
      kitty = "${lib.getBin kitty}/bin/kitty";
      dash = "${lib.getBin dash}/bin/dash";
      systemd-run = "${lib.getBin systemd}/bin/systemd-run";
    };
  });
})

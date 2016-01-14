{ config, pkgs, lib, ... }:

let
  cfg = config.vuizvui.user.aszlig.programs.xpdf;

  xpdf = pkgs.xpdf.overrideDerivation (drv: {
    postInstall = (drv.postInstall or "") + ''
      echo 'bind ctrl-o any toggleOutline' >> "$out/etc/xpdfrc"
    '';
  });

in {
  options.vuizvui.user.aszlig.programs.xpdf = {
    enable = lib.mkEnableOption "aszlig's xpdf";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = lib.singleton xpdf;
  };
}

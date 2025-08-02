{ pkgs, lib, config, ... }:

let
  cfg = config.vuizvui.user.sternenseemann.programs.saneterm;
in

{
  options = {
    vuizvui.user.sternenseemann.programs.saneterm = {
      enable = lib.mkEnableOption "saneterm";
      package = lib.mkOption {
        type = lib.types.package;
        description = ''
          saneterm derivation to use.
        '';
        default = pkgs.vuizvui.sternenseemann.saneterm;
        defaultText = lib.literalExpression "pkgs.vuizvui.sternenseemann.saneterm";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment = {
      etc."profile".text = ''
        if [ "$TERM" = "dumb" ]; then
          export PAGER=cat
          export GIT_PAGER=cat
        fi

        export PS1="; "
      '';
      systemPackages = [
        cfg.package
      ];
    };
  };
}

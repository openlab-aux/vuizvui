{ pkgs, config, lib, ... }:

with lib;

let cfg = config.vuizvui.programs.fish.fasd;
in

{
  options.vuizvui.programs.fish.fasd = {
    enable = mkEnableOption "fasd integration in fish";
  };

  config = mkIf cfg.enable {

    environment.systemPackages = [ pkgs.fasd ];

    programs.fish = {
      interactiveShellInit = let fasd = "${pkgs.fasd}/bin/fasd"; in ''
        function _run_fasd -e fish_preexec
          ${fasd} --proc (${fasd} --sanitize "$argv") > "/dev/null" 2>&1
        end
        function z --description "Jump to folder by usage frequency"
          cd (fasd -d -e 'printf %s' "$argv")
        end
        set PATH (dirname ${fasd}) $PATH
      '';
    };

  };
}

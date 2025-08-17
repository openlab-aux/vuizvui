{ config, options, lib, pkgs, ... }:

let
  cfg = config.vuizvui.user.sternenseemann.profiles.editors;

  tvl = pkgs.vuizvui.tvl.users.sterni;

  # Slightly modified version of
  # https://leahneukirchen.org/blog/archive/2020/05/a-minimal-vimrc.html
  vimrc = pkgs.writeText "vimrc" ''
    set nocp bs=2 hid is ru sm t_te= t_ti= vb wim=longest,list
    set ignorecase smartcase
  '';
  vim = pkgs.writeShellScriptBin "vim" ''
    exec "${lib.getBin pkgs.vim}/bin/vim" -u "${vimrc}" "$@"
  '';

  editors = lib.filter
    (option: !(lib.elem option [ "enable" "editor" "alternate" ]))
    (builtins.attrNames options.vuizvui.user.sternenseemann.profiles.editors);

  mkEditor = { name, pkg, exe ? name }: {
    enable = lib.mkEnableOption name;
    package = lib.mkOption {
      type = lib.types.package;
      description = "Package to use for ${name}";
      default = pkg;
    };
    exe = lib.mkOption {
      description = "Executable to run for ${name}";
      type = lib.types.path;
      default = lib.getExe' cfg.${name}.package exe;
    };
  };
in

{
  options = {
    vuizvui.user.sternenseemann.profiles.editors = {
      enable = lib.mkEnableOption "sterni's editors profile";

      editor = lib.mkOption {
        type = lib.types.str;
        description = ''
          Default editor to use. Must correspond to a sub-option of
          {option}`vuizvui.users.sternenseemann.profiles.editors`.
        '';
        example = "emacs";
      };
      alternate = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        example = "vim";
        description = ''
          Editor to use as a fallback to
          {option}`vuizvui.users.sternenseemann.profiles.editors.editor`.
          Must correspond to a sub-option of
          {option}`vuizvui.users.sternenseemann.profiles.editors`.
        '';
      };

      acme = mkEditor {
        name = "acme";
        pkg = tvl.acme.plan9port.wrapper;
      };

      emacs = mkEditor {
        name = "emacs";
        pkg = tvl.emacs;
        exe = "emacsclient";
      };

      ma = mkEditor {
        name = "ma";
        pkg = config.vuizvui.user.sternenseemann.programs.ma.package;
      };

      vim = mkEditor {
        name = "vim";
        pkg = vim;
      };
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      assertions = [
        {
          assertion = cfg.alternate != null -> cfg.editor == "emacs";
          message = "ALTERNATE_EDITOR is only supported by emacs";
        }
        {
          assertion = cfg.${cfg.editor}.enable;
          message = "Chosen editor \"${cfg.editor}\" must be enabled.";
        }
        {
          assertion = cfg.alternate != null -> cfg.${cfg.alternate}.enable;
          message = "Chosen alternate editor \"${cfg.alternate}\" must be enabled.";
        }
      ];

      environment = {
        systemPackages = lib.concatMap
          (editor: lib.optional cfg.${editor}.enable cfg.${editor}.package)
          editors;

        variables = {
          EDITOR = cfg.${cfg.editor}.exe;
          VISUAL = cfg.${cfg.editor}.exe;
          ALTERNATE_EDITOR = lib.mkIf (cfg.alternate != null) cfg.${cfg.alternate}.exe;
        };
      };
    }

    (lib.mkIf cfg.emacs.enable {
      programs.fish.shellInit = ''
        alias e "emacsclient -n"
      '';
    })

    (lib.mkIf cfg.ma.enable {
      vuizvui.user.sternenseemann.programs.ma.enable = true;
    })
  ]);
}

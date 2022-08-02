{ config, lib, pkgs, ... }:

let
  inherit (lib) types;
  cfg = config.vuizvui.user.aszlig.programs.git;

  gitPatched = pkgs.gitFull.overrideAttrs (git: {
    makeFlags = let
      oldFlags = git.makeFlags or [];
      newVal = "ETC_GITCONFIG=${cfg.configFile}";
    in if lib.isList oldFlags
       then oldFlags ++ [ newVal ]
       else "${oldFlags} ${newVal}";
  });

  libgit2Patched = pkgs.libgit2.overrideAttrs (drv: {
    postPatch = (drv.postPatch or "") + ''
      substituteInPlace src/libgit2/sysdir.c \
        --replace '"/etc"' ${lib.escapeShellArg "\"${cfg.configFile}\""}
    '';
  });

  deltaPatched = pkgs.delta.overrideAttrs (drv: {
    buildInputs = (drv.buildInputs or []) ++ lib.singleton libgit2Patched;
  });

in {
  options.vuizvui.user.aszlig.programs.git = {
    enable = lib.mkEnableOption "Git";

    configFile = lib.mkOption {
      type = types.path;
      readOnly = true;
      description = ''
        The path to the system-wide configuration file.
      '';
    };

    delta.enable = lib.mkEnableOption "using delta for Git diff";

    settings = lib.mkOption {
      type = let
        scalar = types.oneOf [ types.bool types.int types.str ];
        section = types.either (types.attrsOf scalar) scalar;
      in types.attrsOf (types.attrsOf section);

      default = {};
      example = {
        color.ui = "auto";
        merge.tool = "vimdiff";
        guitool.foobar.noconsole = true;
      };

      description = ''
        System-wide Git options as described in <citerefentry>
          <refentrytitle>git-config</refentrytitle>
          <manvolnum>1</manvolnum>
        </citerefentry>.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    vuizvui.user.aszlig.programs.git.configFile = let
      escStr = s: "\"${lib.escape [ "\"" "\\" ] s}\"";
      mkVal = v: if lib.isBool v && v  then "true"
            else if lib.isBool v && !v then "false"
            else escStr (toString v);
      mkLine = key: val: "${key} = ${mkVal val}";

      joinLines = lib.concatStringsSep "\n";
      filterNull = lib.filterAttrs (lib.const (v: !(isNull v)));

      mkSection = sect: subsect: vals: ''
        [${sect}${lib.optionalString (subsect != null) " ${escStr subsect}"}]
        ${joinLines (lib.mapAttrsToList mkLine (filterNull vals))}
      '';

      mkConf = sect: content: let
        subs = lib.filterAttrs (lib.const lib.isAttrs) content;
        nonSubs = lib.filterAttrs (lib.const (s: !lib.isAttrs s)) content;
        maybePlain = lib.optional (nonSubs != {});
        plainSects = lib.singleton (mkSection sect null nonSubs);
      in lib.mapAttrsToList (mkSection sect) subs ++ maybePlain plainSects;

      lines = lib.flatten (lib.mapAttrsToList mkConf cfg.settings);
    in pkgs.writeText "gitconfig" (joinLines lines);

    vuizvui.user.aszlig.programs.git.settings = lib.mkIf cfg.delta.enable {
      interactive.diffFilter = "delta --color-only";
      pager = lib.flip lib.genAttrs (lib.const "delta") [
        "diff" "log" "reflog" "show"
      ];
    };

    environment.systemPackages = [
      gitPatched
      pkgs.gitAndTools.git-remote-hg
      pkgs.gitAndTools.hub
    ] ++ lib.optional cfg.delta.enable deltaPatched;
  };
}

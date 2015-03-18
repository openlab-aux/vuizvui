{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.vuizvui.user.aszlig.programs.git;

  genConf = attrs: let
    escStr = s: "\"${escape [ "\"" "\\" ] s}\"";
    mkVal = v: if isBool v && v  then "true"
          else if isBool v && !v then "false"
          else escStr (toString v);
    mkLine = key: val: "${key} = ${mkVal val}";

    filterNull = filterAttrs (_: v: !(isNull v));

    mkSection = sect: subsect: vals: ''
      [${sect}${optionalString (subsect != null) " ${escStr subsect}"}]
      ${concatStringsSep "\n" (mapAttrsToList mkLine (filterNull vals))}
    '';

    mkConf = sect: content: let
      subs = filterAttrs (_: isAttrs) content;
      nonSubs = filterAttrs (_: s: !isAttrs s) content;
      hasPlain = (attrNames nonSubs) != [];
      plainSects = singleton (mkSection sect null nonSubs);
    in mapAttrsToList (mkSection sect) subs ++ optional hasPlain plainSects;

    text = concatStringsSep "\n" (flatten (mapAttrsToList mkConf attrs));
  in pkgs.writeText "gitconfig" text;

  gitPatched = overrideDerivation pkgs.gitFull (git: {
    makeFlags = let
      oldFlags = git.makeFlags or [];
      newVal = "ETC_GITCONFIG=${cfg.config}";
    in if isList oldFlags
       then oldFlags ++ [ newVal ]
       else "${oldFlags} ${newVal}";
  });
in {
  options.vuizvui.user.aszlig.programs.git = {
    enable = mkEnableOption "Git";

    config = mkOption {
      description = "System-wide default config for Git";

      type = let
        superType = types.attrsOf types.unspecified;
      in mkOptionType {
        name = "attribute set of either plain values or "
             + "attribute sets of values (if it is a subsection)";
        inherit (superType) check merge;
        inherit (superType) getSubOptions getSubModules substSubModules;
      };

      default = {};
      example = {
        color.ui = "auto";
        merge.tool = "vimdiff";
        guitool.foobar.noconsole = true;
      };

      apply = genConf;
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [
      gitPatched
      pkgs.gitAndTools.git-remote-hg
      pkgs.gitAndTools.hub
    ];
  };
}

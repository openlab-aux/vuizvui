{ config, lib, pkgs, ... }:

let
  inherit (pkgs.vuizvui.sternenseemann.lib)
    mapAttrsByAttrs
    ;

  inherit (lib.generators)
    toINI
    toKeyValue
    ;

  cfg = config.vuizvui.programs.foot;

  # We don't allow null, since we use null as
  # a “fall back to foot's defaults” value for defined
  # options in the freeform module so no null may be
  # present in the resulting ini file.
  iniAtom = with lib.types; (oneOf [
    bool
    int
    float
    str
  ]) // {
    description = "INI atom (bool, int, float or string)";
  };

  # TODO(sterni): multiple binds
  format = pkgs.formats.ini {};

  prettyPrint = lib.generators.toPretty {};

  fontOptions = [
    "font"
    "font-bold"
    "font-italic"
    "font-bold-italic"
  ];

  exampleFontSet = {
    font = "Dina";
    options = {
      slant = "italic";
      weight = "bold";
    };
  };

  # check if the given set is formed like we expect it to be
  # and print a nice error message if not.
  wellFormedFontSet = set:
    let
      attrCount = builtins.length (builtins.attrNames set);
    in
      lib.assertMsg (set ? font && attrCount <= 2
          && (attrCount > 1 -> set ? options))
        "font set must be of the form ${prettyPrint exampleFontSet} where the options attr is optional.";

  # Convert a mixed list of font sets and strings into a comma
  # separated string list of properly rendered fontconfig font strings
  buildIniFontList = fonts:
    let
      formatOptions = opts: lib.concatStrings
        (lib.mapAttrsToList (o: v: ":${o}=${builtins.toString v}") opts);
      fontconfigFont = font:
        if builtins.isString font
        then font
        else assert (wellFormedFontSet font);
          "${font.font}${formatOptions (font.options or {})}";
    in
      lib.concatMapStringsSep "," fontconfigFont fonts;

  mkFontOption = name: lib.mkOption {
    type = with lib.types; nullOr (nonEmptyListOf (either str attrs));
    description = ''
      The first font specified is used as foot's
      primary ${name}, all further fonts are
      used as fallbacks in the specified order.
      Fonts must be either specified as strings
      in fontconfig syntax or using a special
      record syntax (see example).
    '';
    example = lib.literalExample (prettyPrint [
      {
        font = "Courier New";
        options = {
          size = 12;
        };
      }
      exampleFontSet
      "monospace"
    ]);
    default = null;
  };

  commandBindOptions = [
    "pipe-visible"
    "pipe-scrollback"
    "pipe-selected"
  ];

  exampleCommandBindSet = {
    cmd = "sh -c 'xurls | bemenu | xargs -r $BROWSER'";
    bind = "Control+Print";
  };

  wellformedCommandBindSet = set:
    lib.assertMsg (set ? cmd && set ? bind)
      "command bind set must contain a cmd and a bind attr: ${prettyPrint exampleCommandBindSet}";

  buildIniCommandBind = bind:
    if builtins.isString bind
    then bind
    else assert wellformedCommandBindSet bind;
      "[${bind.cmd}] ${bind.bind}";

  mkCommandBindOption = name:
    lib.mkOption {
      type = with lib.types; nullOr (either str (attrsOf str));
      description = ''
        Bind a key which pipes the ${lib.removePrefix "pipe-" name}
        region into the given command.
      '';
      example = lib.literalExample (prettyPrint exampleCommandBindSet);
      default = null;
    };

  # convert some fancy options we support to a format formats.ini can deal
  # with and remove all optional options (in this case options which default
  # to null), so we don't have to track upstreams defaults, but foot can
  # decide for itself while we still can treat some options specially.
  iniReady = settings:
    let
      withoutNulls =
        lib.filterAttrsRecursive (_: x: x != null) settings;
      attrTransforms = {
        main =
          lib.genAttrs fontOptions (n: (_: buildIniFontList));
        key-bindings =
          lib.genAttrs commandBindOptions (n: (_: buildIniCommandBind));
        };
    in
      mapAttrsByAttrs withoutNulls attrTransforms;

in {
  options.vuizvui.programs.foot = {
    enable = lib.mkEnableOption "foot";

    settings = lib.mkOption {
      type = lib.types.submodule {
        freeformType = format.type;
        options = {
          main =
            lib.genAttrs fontOptions mkFontOption;
          key-bindings =
            lib.genAttrs commandBindOptions mkCommandBindOption;
        };
      };
      default = {};
      description = ''
        Configuration for foot. A list of all available
        options can be found in
        <citerefentry>
          <refentrytitle>foot</refentrytitle>
          <manvolnum>5</manvolnum>
        </citerefentry>
        or at <link xlink:href="https://codeberg.org/dnkl/foot/src/tag/${pkgs.foot.version}/foot.ini" />.
      '';
      example = lib.literalExample (prettyPrint {
        key-bindings = {
          scrollback-up-page = "Control+Shift+Page_Up";
          scrollback-down-page = "Control+Shift+Page_Down";
          search-start = "Control+Shift+F";
        };

        mouse-bindings = {
          primary-paste = "none";
        };
      });
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.foot pkgs.foot.terminfo ];

    environment.etc."xdg/foot/foot.ini".source =
      format.generate "foot.ini" (iniReady cfg.settings);
  };
}

{ config, lib, ... }:

with lib;

let
  mappingType = (types.addCheck types.str (val: let
    pattern = "[ab][0-9]+|h[0-9]+\.[0-9]+";
  in builtins.match pattern val == [])) // {
    name = "aI (axis), bI (button) or hI.M (hat) where I=index, M=mask";
  };

  mkAssignmentOption = example: name: description: mkOption {
    type = types.nullOr mappingType;
    default = null;
    inherit example;
    description = "Assignment for ${description}.";
  };

  mkAxisOption = mkAssignmentOption "a0";
  mkButtonOption = mkAssignmentOption "b0";

  axes = {
    leftx = "left stick X axis";
    lefty = "left stick Y axis";
    rightx = "right stick X axis";
    righty = "right stick Y axis";
    lefttrigger = "left trigger";
    righttrigger = "right trigger";
  };

  buttons = {
    a = "A button (down)";
    b = "B button (right)";
    x = "X button (left)";
    y = "Y button (up)";
    back = "XBox `back` button";
    guide = "XBox `guide` button";
    start = "`start` button";
    leftstick = "pressing the left stick";
    rightstick = "pressing the right stick";
    leftshoulder = "left shoulder/bumper button";
    rightshoulder = "right shoulder/bumper button";
    dpup = "directional pad up";
    dpdown = "directional pad down";
    dpleft = "directional pad left";
    dpright = "directional pad right";
  };

  gcSubModule = { name, ... }: {
    options = {
      name = mkOption {
        type = types.str;
        default = name;
        description = ''
          The name of this controller, doesn't have special meaning and is only
          there to make it easier to dinguish various mappings.
        '';
      };

      guid = mkOption {
        type = types.uniq types.str;
        default = name;
        description = ''
          The SDL2 GUID to uniquely identify this controller.

          Use {option}`vuizvui.list-gamecontrollers` to list them.
        '';
      };

      mapping = mapAttrs mkAxisOption axes // mapAttrs mkButtonOption buttons;
    };
  };

  mkGCLine = const (cfg: let
    validMappings = attrNames axes ++ attrNames buttons;
    mkMappingVal = name: let
      val = cfg.mapping.${name} or null;
    in if val == null then null else "${name}:${val}";
    attrs = [ cfg.guid cfg.name "platform:Linux" ]
         ++ remove null (map mkMappingVal validMappings);
  in concatStringsSep "," attrs);

  controllers = mapAttrsToList mkGCLine config.vuizvui.hardware.gameController;
  controllerConfig = concatStringsSep "\n" controllers;

in {
  options.vuizvui.hardware.gameController = mkOption {
    type = types.attrsOf (types.submodule gcSubModule);
    default = {};
    description = ''
      A mapping of the game controllers to use with SDL2 games.

      The mapping is always based on the XBox reference controller, so even if
      you don't use an XBox controller, you still have to map your keys according to
      [this layout](https://upload.wikimedia.org/wikipedia/commons/2/2c/360_controller.svg).
    '';
  };

  config = mkIf (config.vuizvui.hardware.gameController != {}) {
    environment.sessionVariables.SDL_GAMECONTROLLERCONFIG = controllerConfig;
  };
}

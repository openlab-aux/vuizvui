{ pkgs, lib, config, ... }:

with lib;

let
  inherit (config.services.xserver) xrandrHeads;

  # The symbols if you press shift and a number key.
  wsNumberSymbols = [
    "exclam" "at" "numbersign" "dollar" "percent"
    "asciicircum" "ampersand" "asterisk" "parenleft" "parenright"
  ];

  wsCount = length wsNumberSymbols;

  headCount = length xrandrHeads;
  wsPerHead = wsCount / headCount;
  excessWs = wsCount - (headCount * wsPerHead);
  headModifier = if config.vuizvui.i3.reverseHeads then reverseList else id;
  getHeadAt = elemAt (headModifier xrandrHeads);

  mkDefaultWorkspace = number: numberSymbol: {
    name = toString number;
    value = mkDefault {
      label = null;
      labelPrefix = "${toString number}: ";
      keys.switchTo = "$mod+${if number == 10 then "0" else toString number}";
      keys.moveTo = "$mod+Shift+${numberSymbol}";
      head = if headCount == 0 then null
             else getHeadAt ((number - (excessWs + 1)) / wsPerHead);
    };
  };

  wsCfgList = mapAttrsToList (_: getAttr "config") config.vuizvui.i3.workspaces;
  wsConfig = concatStrings wsCfgList;
  defaultWorkspaces = listToAttrs (imap mkDefaultWorkspace wsNumberSymbols);

  conky = import ./conky.nix {
    inherit pkgs;
  };

  mkBar = output: statusCmd: ''
    bar {
      ${optionalString (output != null) "output ${output}"}
      ${optionalString (statusCmd != null) "status_command ${statusCmd}"}
      colors {
        focused_workspace  #5c5cff #e5e5e5
        active_workspace   #ffffff #0000ee
        inactive_workspace #00cdcd #0000ee
        urgent_workspace   #ffff00 #cd0000
      }
    }
  '';

  barConfig =
    if headCount == 0 then mkBar null conky.single
    else if headCount == 1 then mkBar (head xrandrHeads) conky.single
    else let inner = take (length xrandrHeads - 2) (tail xrandrHeads);
    in mkBar (head xrandrHeads) conky.left
     + concatMapStrings (flip mkBar null) inner
     + mkBar (last xrandrHeads) conky.right;
in
{
  options.vuizvui.i3 = {
    enable = mkEnableOption "i3";

    workspaces = mkOption {
      type = types.attrsOf (types.submodule ./workspace.nix);
      default = listToAttrs (imap mkDefaultWorkspace wsNumberSymbols);
      description = ''
        Workspace to monitor assignment.

        Workspaces are by default assigned starting from the leftmost monitor
        being workspace 1 and the rightmost monitor being workspace 10. The
        workspaces are divided by the number of available heads, so if you have
        a dual head system, you'll end up having workspace 1 to 5 on the left
        monitor and 6 to 10 on the right.
      '';
    };

    reverseHeads = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Reverse the order of the heads, so if enabled and you have two heads,
        you'll end up having workspaces 1 to 5 on the right head and 6 to 10 on
        the left head.
      '';
    };
  };

  config.vuizvui.i3.workspaces = defaultWorkspaces;

  config.services.xserver.windowManager = mkIf config.vuizvui.i3.enable {
    default = "i3";

    i3.enable = true;
    i3.configFile = pkgs.substituteAll {
      name = "i3.conf";
      src = ./i3.conf;

      inherit (pkgs) dmenu xterm pvolctrl;
      inherit (pkgs.xorg) xsetroot;
      inherit wsConfig barConfig;
    };
  };
}

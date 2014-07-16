{ pkgs, lib, config, ... }:

with lib;

let
  # The symbols if you press shift and a number key.
  wsNumberSymbols = [
    "exclam" "at" "numbersign" "dollar" "percent"
    "asciicircum" "ampersand" "asterisk" "parenleft" "parenright"
  ];

  wsCount = length wsNumberSymbols;

  headCount = length config.services.xserver.xrandrHeads;
  wsPerHead = wsCount / headCount;
  excessWs = wsCount - (headCount * wsPerHead);
  headModifier = if config.aszlig.i3.reverseHeads then reverseList else id;
  getHeadAt = elemAt (headModifier config.services.xserver.xrandrHeads);

  mkDefaultWorkspace = number: numberSymbol: {
    name = toString number;
    value = mkDefault {
      label = null;
      labelPrefix = "${toString number}: ";
      keys.switchTo = "$mod+${if number == 10 then "0" else toString number}";
      keys.moveTo = "$mod+Shift+${numberSymbol}";
      head = getHeadAt ((number - (excessWs + 1)) / wsPerHead);
    };
  };

  wsCfgList = mapAttrsToList (_: getAttr "config") config.aszlig.i3.workspaces;
  wsConfig = concatStrings wsCfgList;
  defaultWorkspaces = listToAttrs (imap mkDefaultWorkspace wsNumberSymbols);
in
{
  options.aszlig.i3 = {
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

  config.aszlig.i3.workspaces = defaultWorkspaces;

  config.services.xserver.windowManager = {
    default = "i3";

    i3.enable = true;
    i3.configFile = let
      conky = import ./conky.nix {
        inherit pkgs;
      };
    in pkgs.substituteAll ({
      name = "i3.conf";
      src = ./i3.conf;

      inherit (pkgs) dmenu xterm pvolctrl;
      inherit (pkgs.xorg) xsetroot;
      inherit wsConfig;

      leftHead = head config.services.xserver.xrandrHeads;
      rightHead = last config.services.xserver.xrandrHeads;

      leftConky = conky.left;
      rightConky = conky.right;
    } // (let
      # Workaround for Synergy: we need to have polarizing heads.
      leftHead = head config.services.xserver.xrandrHeads;
      rightHead = last config.services.xserver.xrandrHeads;
    in if config.networking.hostName == "mmrnmhrm"
       then { inherit leftHead rightHead; }
       else { leftHead = rightHead; rightHead = leftHead; }
    ));
  };
}

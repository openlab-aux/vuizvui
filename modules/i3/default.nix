{ pkgs, config, ... }:

with pkgs.lib;

{
  services.xserver.windowManager = {
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
    ) // (let
      wsConfig = if config.networking.hostName == "mmrnmhrm"
                 then [ "XMPP" null "chromium" null null
                        null   null null       null null ]
                 else [ null       null null null null
                        "chromium" null null null null ];

      mkWsName = num: name: let
        mkPair = nameValuePair "ws${toString num}";
      in if name == null
         then mkPair (toString num)
         else mkPair "${toString num}: ${name}";

    in listToAttrs (imap mkWsName wsConfig)));
  };
}

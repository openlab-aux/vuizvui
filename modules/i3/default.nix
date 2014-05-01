{ pkgs, config, ... }:

{
  services.xserver.windowManager = {
    default = "i3";

    i3.enable = true;
    i3.configFile = with pkgs.lib; pkgs.substituteAll ({
      name = "i3.conf";
      src = ./i3.conf;

      inherit (pkgs) conky dmenu xterm pvolctrl;
      inherit (pkgs.xorg) xsetroot;
      leftHead = head config.services.xserver.xrandrHeads;
      rightHead = last config.services.xserver.xrandrHeads;

      primaryNetInterface = "enp0s25";

      conkyrc = pkgs.writeText "conkyrc" ''
        cpu_avg_samples 2
        net_avg_samples 2
        no_buffers yes
        out_to_console yes
        out_to_ncurses no
        out_to_stderr no
        extra_newline no
        update_interval 1.0
        uppercase no
        use_spacer none
        pad_percents 3
        use_spacer left
        TEXT
      '';
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
                 else [ "chromium" null null null null
                        null       null null null null ];

      mkWsName = num: name: let
        mkPair = nameValuePair "ws${toString num}";
      in if name == null
         then mkPair (toString num)
         else mkPair "${toString num}: ${name}";

    in listToAttrs (imap mkWsName wsConfig)));
  };
}

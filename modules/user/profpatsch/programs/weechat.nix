# somewhat copied from the nixpkgs module (2020-06-15).
# uses tmux instead of screen and gets rid of the strange suid wrapper.

# Usage is `mosh weechat@host` and it will ForceCommand
# you to the weechat tmux session.
# Should you kill the tmux session, it will restart it after
# a few seconds.
{ config, lib, pkgs, ... }:

let
  cfg = config.vuizvui.user.profpatsch.programs.weechat;
  weechatDataDir = "/var/lib/weechat";
  sessionName = "weechat";
  userName = "weechat";

  inherit (pkgs.vuizvui.profpatsch)
    writeExecline
    getBins
    ;

  bins = getBins pkgs.tmux [ "tmux" ]
    // getBins pkgs.weechat [ "weechat" ]
    // getBins pkgs.dash [ "dash" ]
    // getBins pkgs.s6-portable-utils [ "s6-sleep" "s6-test" ]
    // getBins pkgs.mosh [ "mosh-server" ]
    ;

  until = { delaySec }: writeExecline "until" {} [
      "if" "-tn" [ "$@" ]
      bins.s6-sleep (toString delaySec)
        # recurse back into until here
        "$0" "$@"
    ];

  startWeechatTmuxSession = wrapper: writeExecline "start-weechat-tmux-session" {} [
    "if" ([
      bins.tmux
        "new-session"
          # detach immediately
          "-d"
          "-s" sessionName
    ] ++ wrapper ++ [
          bins.weechat
    ])
    (until { delaySec = 3; })
      # negate has-session
      "if" "-n" [
        bins.tmux "has-session"
          "-t" sessionName
      ]
  ];

  attachWeechatTmuxSession = writeExecline "attach-weechat-tmux-session" {} [
    "importas" "-u" "-D" "" "what" "SSH_ORIGINAL_COMMAND"
    # if the user passes "ssh" as argv, it will call tmux directly
    "ifelse" [ bins.s6-test "$what" "=" "ssh" ]
    [ bins.tmux "attach-session" "-t" sessionName ]
    # if not, it uses the mosh-server (default)
    bins.mosh-server "--"
      bins.tmux
        "attach-session"
          "-t" sessionName
  ];
in

{
  options.vuizvui.user.profpatsch.programs.weechat = {
    enable = lib.mkEnableOption "weechat";

    authorizedKeys = lib.mkOption {
      description = "ssh keys that should be able to connect to the weechat tmux session";
      type = lib.types.listOf lib.types.str;
    };

    wrapExecStart = lib.mkOption {
      description = "bernstein-chaining command wrapped around weechat";
      type = lib.types.listOf lib.types.str;
      default = [];
    };
  };

  config = lib.mkIf cfg.enable {
    users = {
      groups.weechat = {};
      users.${userName} = {
        isSystemUser = true;
        createHome = true;
        shell = bins.dash;
        group = "weechat";
        home = weechatDataDir;
        openssh.authorizedKeys.keys = cfg.authorizedKeys;
        # give this user access to the bitlbee group and socket
        # TODO: should not be here I guess.
        extraGroups = [ "bitlbee" ];
      };
    };

    # make sure the only use-case for this account
    # is attaching the tmux session.
    services.openssh.extraConfig = ''
      Match User ${userName}
          ForceCommand ${attachWeechatTmuxSession}
    '';

    systemd.services.weechat = {
      environment.WEECHAT_HOME = weechatDataDir;
      serviceConfig = {
        ExecStart = startWeechatTmuxSession cfg.wrapExecStart;
        Restart = "always";
        RestartSec = "3s";
        User = userName;
        Group = "weechat";
      };
      wantedBy = [ "multi-user.target" ];
      wants = [ "network.target" ];
    };

    # This enables “lingering” for the CI user.
    # Inspired by the discussion (and linked code)
    # in https://github.com/NixOS/nixpkgs/issues/3702
    # This should just be a NixOS option really.
    system.activationScripts = {
      enableLingering = ''
        # remove all existing lingering users
        rm -r /var/lib/systemd/linger
        mkdir /var/lib/systemd/linger
        # enable for the subset of declared users
        touch /var/lib/systemd/linger/${userName}
      '';
    };

  };
}

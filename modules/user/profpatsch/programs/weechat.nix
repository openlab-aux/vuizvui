# somewhat copied from the nixpkgs module (2020-06-15).
# uses tmux instead of screen and gets rid of the strange suid wrapper.

# Usage is `mosh weechat@host` and it will ForceCommand
# you to the weechat tmux session.
# Should you kill the tmux session, it will restart it after
# a few seconds.
{ config, lib, pkgs, ... }:

let
  cfg = config.vuizvui.user.profpatsch.programs.weechat;
  # tmux session name weechat runs in
  sessionName = "weechat";

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

    userName = lib.mkOption {
      description = "user name of the user account that should be created for weechat";
      type = lib.types.str;
    };

    extraGroups = lib.mkOption {
      description = "extra groups to add to the weechat user";
      type = lib.types.listOf lib.types.str;
    };

    weechatDataDir = lib.mkOption {
      description = "the data directory used for keeping configuration, logs and other state";
      type = lib.types.path;
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
      users.${cfg.userName} = {
        isSystemUser = true;
        createHome = true;
        shell = bins.dash;
        group = "weechat";
        home = cfg.weechatDataDir;
        openssh.authorizedKeys.keys = cfg.authorizedKeys;
        extraGroups = cfg.extraGroups;
      };
    };

    # make sure the only use-case for this account
    # is attaching the tmux session.
    services.openssh.extraConfig = ''
      Match User ${cfg.userName}
          ForceCommand ${attachWeechatTmuxSession}
    '';

    systemd.services."weechat-${cfg.userName}" = {
      environment.WEECHAT_HOME = cfg.weechatDataDir;
      serviceConfig = {
        ExecStart = startWeechatTmuxSession cfg.wrapExecStart;
        Restart = "always";
        RestartSec = "3s";
        User = cfg.userName;
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
        touch /var/lib/systemd/linger/${cfg.userName}
      '';
    };

  };
}

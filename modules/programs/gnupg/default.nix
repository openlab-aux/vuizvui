{ config, pkgs, lib, ... }:

let
  cfg = config.vuizvui.programs.gnupg;

  inherit (lib) versionAtLeast getVersion mkIf types mkOption;

  hasXdgSupport = versionAtLeast (getVersion cfg.package) "2.1.13";
  isDefaultHome = cfg.homeDir == ".gnupg";

  hasSupervisorSupport = versionAtLeast (getVersion cfg.package) "2.1.16";

  sockDir = if hasXdgSupport && isDefaultHome
            then "%t/gnupg"
            else "%h/${cfg.homeDir}";
  shellSockDir = if hasXdgSupport && isDefaultHome
                 then "$XDG_RUNTIME_DIR/gnupg"
                 else "$HOME/${cfg.homeDir}";

  pinentryWrapper = pkgs.runCommandCC "pinentry-wrapper" {
    pinentryProgram = cfg.agent.pinentry.program;
  } ''
    cc -Wall -std=gnu11 -DPINENTRY_PROGRAM=\"$pinentryProgram\" \
      "${./pinentry-wrapper.c}" -o "$out"
  '';

  scdaemonRedirector = pkgs.writeScript "scdaemon-redirector" ''
    #!${pkgs.stdenv.shell}
    exec "${pkgs.socat}/bin/socat" - \
      UNIX-CONNECT:"${shellSockDir}/S.scdaemon"
  '';

  agentWrapper = withSupervisor: pkgs.runCommandCC "gpg-agent-wrapper" {
    buildInputs = with pkgs; [ pkg-config systemd ];
    inherit pinentryWrapper;
  } ''
    cc -Wall -shared -std=c11 \
      ${lib.optionalString withSupervisor "-DSUPERVISOR_SUPPORT=1"} \
      -DLIBSYSTEMD=\"${lib.getLib pkgs.systemd}/lib/libsystemd.so\" \
      -DPINENTRY_WRAPPER=\"$pinentryWrapper\" \
      $(pkg-config --cflags libsystemd) -ldl \
      "${./agent-wrapper.c}" -o "$out" -fPIC
  '';

  agentSocketConfig = name: {
    FileDescriptorName = name;
    Service = "gpg-agent.service";
    SocketMode = "0600";
    DirectoryMode = "0700";
  };

in {
  options.vuizvui.programs.gnupg = {
    enable = lib.mkEnableOption "support for GnuPG";

    homeDir = mkOption {
      type = types.addCheck types.str (d: builtins.substring 0 1 d != "/");
      default = ".gnupg";
      description = ''
        The directory where GnuPG keeps its state files and configuration files,
        relative to the user's home directory.
      '';
    };

    package = mkOption {
      type = types.package;
      default = pkgs.gnupg;
      defaultText = "pkgs.gnupg";
      example = lib.literalExample "pkgs.gnupg21";
      description = ''
        The GnuPG package to use for running the agent and make available in
        <option>environment.systemPackages</option>.
      '';
    };

    agent = {
      enable = lib.mkEnableOption "support for the GnuPG agent";

      extraConfig = lib.mkOption {
        type = types.str;
        default = "";
        example = lib.literalExample ''
          default-cache-ttl 34560000
          default-cache-ttl-ssh 34560000
          max-cache-ttl 34560000
          max-cache-ttl-ssh 34560000
        '';
        description = "The content of gpg-agent.conf";
      };

      pinentry.program = mkOption {
        type = types.path;
        default = "${pkgs.pinentry-gtk2}/bin/pinentry";
        defaultText = "\${pkgs.pinentry-gtk2}/bin/pinentry";
        example = lib.literalExample "\${pkgs.pinentry-qt}/bin/pinentry";
        description = "The pinentry program to use to ask for passphrases.";
      };

      sshSupport = lib.mkEnableOption "GnuPG agent support for SSH";

      scdaemon = {
        enable = lib.mkEnableOption "GnuPG agent with Smartcard daemon";

        program = mkOption {
          type = types.path;
          default = "${cfg.package}/libexec/scdaemon";
          defaultText = let
            configPath = "config.vuizvui.programs.gnupg";
          in "\${${configPath}.package}/libexec/scdaemon";
          example = lib.literalExample "\${pkgs.my_scdaemon}/bin/scdaemon";
          description = "The program to use for the Smartcard daemon";
        };
      };
    };
  };

  config = lib.mkMerge [
    (mkIf cfg.enable {
      vuizvui.requiresTests = lib.singleton ["vuizvui" "programs" "gnupg"];
      environment.systemPackages = [ cfg.package ];
    })
    (mkIf (cfg.enable && !isDefaultHome) {
      environment.variables.GNUPGHOME = "~/${cfg.homeDir}";
    })
    (mkIf (cfg.enable && cfg.agent.enable) {
      systemd.user.services.gpg-agent = {
        description = "GnuPG Agent";
        environment.LD_PRELOAD = agentWrapper hasSupervisorSupport;
        environment.GNUPGHOME = "~/${cfg.homeDir}";

        serviceConfig.ExecStart = let
          configFile = pkgs.writeText "gpg-agent.conf" ''
            # module-defined config
            pinentry-program ${pinentryWrapper}
            ${if cfg.agent.scdaemon.enable
              then "scdaemon-program ${scdaemonRedirector}"
              else "disable-scdaemon"}
            ${if hasSupervisorSupport
              then "supervised"
              else "no-detach\ndaemon"}
            ${lib.optionalString cfg.agent.sshSupport "enable-ssh-support"}

            # module user config
            ${cfg.agent.extraConfig}
          '';
        in "${cfg.package}/bin/gpg-agent --options ${configFile}";

        serviceConfig.ExecReload = toString [
          "${cfg.package}/bin/gpg-connect-agent"
          "RELOADAGENT"
          "/bye"
        ];
      };

      systemd.user.sockets.gpg-agent-main = {
        wantedBy = [ "sockets.target" ];
        description = "Main Socket For GnuPG Agent";
        listenStreams = lib.singleton "${sockDir}/S.gpg-agent";
        socketConfig = let
          sockName = if hasSupervisorSupport then "std" else "main";
        in agentSocketConfig sockName;
      };
    })
    (mkIf (cfg.enable && cfg.agent.enable && cfg.agent.scdaemon.enable) {
      systemd.user.sockets.gnupg-scdaemon = {
        wantedBy = [ "sockets.target" ];
        description = "GnuPG Smartcard Daemon Socket";
        listenStreams = lib.singleton "${sockDir}/S.scdaemon";
        socketConfig = {
          FileDescriptorName = "scdaemon";
          SocketMode = "0600";
          DirectoryMode = "0700";
        };
      };

      systemd.user.services.gnupg-scdaemon = {
        description = "GnuPG Smartcard Daemon";
        environment.LD_PRELOAD = agentWrapper false;
        environment.GNUPGHOME = "~/${cfg.homeDir}";

        serviceConfig.ExecStart = toString [
          "${cfg.agent.scdaemon.program}"
          "--no-detach"
          "--daemon"
        ];
      };
    })
    (mkIf (cfg.enable && cfg.agent.enable && cfg.agent.sshSupport) {
      environment.variables.SSH_AUTH_SOCK = "${shellSockDir}/S.gpg-agent.ssh";

      systemd.user.sockets.gpg-agent-ssh = {
        wantedBy = [ "sockets.target" ];
        description = "SSH Socket For GnuPG Agent";
        listenStreams = lib.singleton "${sockDir}/S.gpg-agent.ssh";
        socketConfig = agentSocketConfig "ssh";
      };

      assertions = lib.singleton {
        assertion = !config.programs.ssh.startAgent;
        message = toString [
          "You cannot use the GnuPG agent with SSH support in addition to the"
          "SSH agent, please either disable"
          "`vuizvui.programs.gpg-agent.sshSupport' or disable"
          "`programs.ssh.startAgent'."
        ];
      };
    })
  ];
}

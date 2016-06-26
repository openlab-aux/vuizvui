{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.vuizvui.programs.gnupg;

  hasXdgSupport = versionAtLeast (getVersion cfg.package) "2.1.13";
  isDefaultHome = cfg.homeDir == ".gnupg";

  sockDir = if hasXdgSupport && isDefaultHome
            then "%t/gnupg"
            else "%h/${cfg.homeDir}";
  shellSockDir = if hasXdgSupport && isDefaultHome
                 then "$XDG_RUNTIME_DIR/gnupg"
                 else "$HOME/${cfg.homeDir}";

  pinentryWrapper = pkgs.runCommand "pinentry-wrapper" {
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

  agentWrapper = pkgs.runCommand "gpg-agent-wrapper" {
    buildInputs = with pkgs; [ pkgconfig systemd ];
    inherit pinentryWrapper;
  } ''
    cc -Wall -shared -std=c11 \
      -DLIBSYSTEMD=\"${pkgs.systemd}/lib/libsystemd.so\" \
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
    enable = mkEnableOption "support for GnuPG";

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
      example = literalExample "pkgs.gnupg21";
      description = ''
        The GnuPG package to use for running the agent and make available in
        <option>environment.systemPackages</option>.
      '';
    };

    agent = {
      enable = mkEnableOption "support for the GnuPG agent";

      pinentry.program = mkOption {
        type = types.path;
        default = "${pkgs.pinentry}/bin/pinentry";
        defaultText = "\${pkgs.pinentry}/bin/pinentry";
        example = literalExample "\${pkgs.pinentry_qt5}/bin/pinentry";
        description = "The pinentry program to use to ask for passphrases.";
      };

      sshSupport = mkEnableOption "GnuPG agent support for SSH";

      scdaemon = {
        enable = mkEnableOption "GnuPG agent with Smartcard daemon";

        program = mkOption {
          type = types.path;
          default = "${cfg.package}/libexec/scdaemon";
          defaultText = let
            configPath = "config.vuizvui.programs.gnupg";
          in "\${${configPath}.package}/libexec/scdaemon";
          example = literalExample "\${pkgs.my_shiny_scdaemon}/bin/scdaemon";
          description = "The program to use for the Smartcard daemon";
        };
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      vuizvui.requiresTests = singleton ["vuizvui" "programs" "gnupg"];
      environment.systemPackages = [ cfg.package ];
    })
    (mkIf (cfg.enable && !isDefaultHome) {
      environment.variables.GNUPGHOME = "~/${cfg.homeDir}";
    })
    (mkIf (cfg.enable && cfg.agent.enable) {
      systemd.user.services.gpg-agent = {
        description = "GnuPG Agent";
        environment.LD_PRELOAD = agentWrapper;
        environment.GNUPGHOME = "~/${cfg.homeDir}";

        serviceConfig.ExecStart = toString ([
          "${cfg.package}/bin/gpg-agent"
          "--pinentry-program=${pinentryWrapper}"
          (if cfg.agent.scdaemon.enable
           then "--scdaemon-program=${scdaemonRedirector}"
           else "--disable-scdaemon")
          "--no-detach"
          "--daemon"
        ] ++ optional cfg.agent.sshSupport "--enable-ssh-support");

        serviceConfig.ExecReload = toString [
          "${cfg.package}/bin/gpg-connect-agent"
          "RELOADAGENT"
          "/bye"
        ];
      };

      systemd.user.sockets.gpg-agent-main = {
        wantedBy = [ "sockets.target" ];
        description = "Main Socket For GnuPG Agent";
        listenStreams = singleton "${sockDir}/S.gpg-agent";
        socketConfig = agentSocketConfig "main";
      };
    })
    (mkIf (cfg.enable && cfg.agent.enable && cfg.agent.scdaemon.enable) {
      systemd.user.sockets.gnupg-scdaemon = {
        wantedBy = [ "sockets.target" ];
        description = "GnuPG Smartcard Daemon Socket";
        listenStreams = singleton "${sockDir}/S.scdaemon";
        socketConfig = {
          FileDescriptorName = "scdaemon";
          SocketMode = "0600";
          DirectoryMode = "0700";
        };
      };

      systemd.user.services.gnupg-scdaemon = {
        description = "GnuPG Smartcard Daemon";
        environment.LD_PRELOAD = agentWrapper;
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
        listenStreams = singleton "${sockDir}/S.gpg-agent.ssh";
        socketConfig = agentSocketConfig "ssh";
      };

      assertions = singleton {
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

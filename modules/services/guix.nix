# ATTN: this is a WIP service, use at your own risk!
{ config, lib, pkgs, ... }:
# https://www.gnu.org/software/guix/manual/en/html_node/Binary-Installation.html

let
  guixBinaryTar = pkgs.fetchurl {
    url = "https://alpha.gnu.org/gnu/guix/guix-binary-0.16.0.x86_64-linux.tar.xz";
    sha256 = "049l0zim30cd0gyly2h3jaw4cshdk78h7xdb9ac173h72i13afbj";
  };

  #*/
  guixInstallScriptIdempotent = pkgs.writeScript "guix-install.sh" ''
    #!/bin/sh
    set -euo pipefail

    # extract guix
    if ! test -e /gnu; then
      echo "INFO: installing guix"

      tmp=$(mktemp -d)
      pushd $tmp >/dev/null
      export PATH=${pkgs.xz}/bin:$PATH
      ${pkgs.gnutar}/bin/tar xf ${guixBinaryTar}
      mkdir -p /var
      cp -r ./var/guix /var
      cp -r ./gnu /
      popd >/dev/null

      # XXX
      # change the mtime of all compiled guile files,
      # because tar in this script somehow changes the mtime
      # of extracted files to the current time, and nobody knows
      # why. If the sources are newer than the .go files, guile
      # will try to recompile everything.
      find /gnu/store/ -ipath "*guile*ccache*/*.go" | xargs touch -m
    fi

    # install root user profile
    if ! test -e /root/.config/guix/current; then
      mkdir -p /root/.config/guix
      ln -s /var/guix/profiles/per-user/root/current-guix \
        /root/.config/guix/current
    fi

    echo INFO: finished installing guix!
  '';

  guixBuildGroup = "guixbuilders";

  guixBuildUser = id: {
    name = "guix-build-user-${toString id}";
    createHome = false;
    description = "Guix build user ${toString id}";
    extraGroups = [ guixBuildGroup ];
    isSystemUser = true;
  };

  guixBuildUsers = numberOfUsers:
    builtins.listToAttrs
      (map (user: {
        name = user.name;
        value = user;
      }) (builtins.genList guixBuildUser numberOfUsers));
in
{
  options = {
    vuizvui.services.guix.enable =
      lib.mkEnableOption "the guix daemon and init `/gnu/store`";
  };

  config = lib.mkIf config.vuizvui.services.guix.enable {
    users.users = guixBuildUsers 10;
    users.groups = { "${guixBuildGroup}" = {}; };

    systemd.services.guix-install = {
      serviceConfig = {
        ExecStart = guixInstallScriptIdempotent;
        Type = "oneshot";
      };
    };

    systemd.services.guix-daemon = {
      serviceConfig = {
        ExecStart = "/var/guix/profiles/per-user/root/current-guix/bin/guix-daemon --build-users-group=${guixBuildGroup}";
        Environment = "GUIX_LOCPATH=/var/guix/profiles/per-user/root/guix-profix/lib/locale";
        RemainAfterExit = true;
        StandardOutput = "syslog";
        StandardError = "syslog";
        TasksMax = 8192;
      };
      wantedBy = [ "multi-user.target" ];
      after = [ "guix-install.service" ];
      wants = [ "guix-install.service" ];
    };

    environment.shellInit = ''
      export GUIX_PROFILE="$HOME/.config/guix/current"
      source $GUIX_PROFILE/etc/profile
      export GUIX_LOCPATH="${pkgs.glibcLocales}/lib/locale"
      export INFOPATH="$GUIX_PROFILE/share/info:$INFOPATH"

      guix archive --authorize < \
        /root/.config/guix/current/share/guix/ci.guix.info.pub
    '';
  };
}

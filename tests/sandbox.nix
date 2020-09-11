{
  name = "sandbox";

  machine = { pkgs, lib, ... }: {
    system.activationScripts.inject-link = ''
      ln -svf ${pkgs.hello} /run/foo-test-sandbox
      ln -svf ${pkgs.gnused} /run/bar-test-sandbox
      ln -svf ${pkgs.gnugrep} /run/baz-test-sandbox
    '';
    environment.sessionVariables.COLLECT_ME = [
      "/run/foo-test-sandbox"
      "/run/bar-test-sandbox"
      "/run/baz-test-sandbox"
    ];

    # Only needed so we get the right XDG paths in the system path.
    services.xserver.enable = true;
    systemd.services.display-manager.enable = false;

    systemd.sockets.netnstest = {
      description = "Host Socket for Testing Network Namespaces";
      requiredBy = [ "sockets.target" ];

      socketConfig.ListenStream = "3000";
      socketConfig.Accept = true;
    };

    systemd.services."netnstest@" = {
      description = "Host Service for Testing Network Namespaces";
      serviceConfig.StandardInput = "socket";
      serviceConfig.ExecStart = "${pkgs.coreutils}/bin/tee /tmp/netns.log";
    };

    environment.systemPackages = let
      mkNestedLinksTo = drv: let
        mkLink = name: to: pkgs.runCommandLocal name { inherit to; } ''
          ln -s "$to" "$out"
        '';
      in mkLink "nested-1" (mkLink "nested-2" (mkLink "nested-3" drv));

      testPackage = pkgs.runCommand "test-sandbox" {
        program = ''
          #!${pkgs.stdenv.shell} -ex

          if [ "$1" != canary ]; then
            echo 'Canary check failed, so the test program probably' \
                 'was not executed via the XDG desktop entry.' >&2
            exit 1
          fi

          # Should fail because we can't access the host's PATH
          ! echo foo | grep -qF foo

          # No /bin/sh by default
          test ! -e /bin
          test ! -e /bin/sh

          # Write PID information to files, so that we can later verify whether
          # we were in a PID namespace.
          echo $$ > /home/foo/.cache/xdg/ownpid
          ls -d1 /proc/[0-9]* > /home/foo/.cache/xdg/procpids

          # Check whether we can access files behind nested storepaths that are
          # symlinks.
          lfile="$(< ${mkNestedLinksTo (pkgs.writeText "target" "file")})"
          test "$lfile" = file
          ldir="$(< ${mkNestedLinksTo (pkgs.runCommandLocal "target" {} ''
            mkdir -p "$out"
            echo dir > "$out/canary"
          '')}/canary)"
          test "$ldir" = dir

          export PATH=/run/baz-test-sandbox/bin
          echo foo > /home/foo/existing/bar
          test ! -d /home/foo/nonexisting
          /run/foo-test-sandbox/bin/hello
          echo aaa | /run/bar-test-sandbox/bin/sed -e 's/a/b/g'

          echo XDG1 > /home/foo/.local/share/xdg/1
          echo XDG2 > /home/foo/.config/xdg/2
          echo XDG3 > /home/foo/.cache/xdg/3
          echo > /home/foo/.cache/xdg/done
        '';
      } ''
        mkdir -p "$out/bin" "$out/share/applications" "$out/share/test-sandbox"

        echo -n "$program" > "$out/bin/test-sandbox"
        chmod +x "$out/bin/test-sandbox"

        echo '<svg xmlns="http://www.w3.org/2000/svg"/>' \
          > "$out/share/test-sandbox/icon.svg"

        cat > "$out/share/applications/test.desktop" <<EOF
        [Desktop Entry]
        Name=$fullName
        Type=Application
        Version=1.1
        Exec=$out/bin/test-sandbox canary
        Icon=$out/share/test-sandbox/icon.svg
        Categories=Utility
        EOF
      '';

    in [
      # Unfortunately, "xdg-open test-sandbox.desktop" doesn't work, so let's
      # use gtk-launch instead. We also need xvfb_run so that we can avoid to
      # start a full-blown X server.
      #
      # See also:
      #
      #   https://askubuntu.com/questions/5172
      #   https://bugs.launchpad.net/ubuntu/+source/gvfs/+bug/378783
      #
      (lib.getBin pkgs.gtk3) pkgs.xvfb_run

      (pkgs.vuizvui.buildSandbox testPackage {
        paths.required = [
          "/home/foo/existing"
          "$XDG_DATA_HOME/xdg"
          "$XDG_CONFIG_HOME/xdg"
          "$XDG_CACHE_HOME/xdg"
        ];
        paths.wanted = [ "/home/foo/nonexisting" ];
        paths.runtimeVars = [ "COLLECT_ME" ];
      })

      (pkgs.vuizvui.buildSandbox (pkgs.writeScriptBin "test-sandbox2" ''
        #!/bin/sh
        # Another /bin/sh just to be sure :-)
        /bin/sh -c 'echo /bin/sh works'
      '') { allowBinSh = true; })

      (pkgs.vuizvui.buildSandbox (pkgs.writeScriptBin "test-sandbox3" ''
        #!${pkgs.stdenv.shell}
        echo hello network | ${pkgs.netcat-openbsd}/bin/nc -N 127.0.0.1 3000 \
          || echo netcat has failed
      '') { namespaces.net = true; })

      (pkgs.vuizvui.buildSandbox (pkgs.writeScriptBin "test-sandbox4" ''
        #!${pkgs.stdenv.shell}
        test $$ -gt 5 && echo no pid namespace
      '') { namespaces.pid = false; })
    ];
    users.users.foo.isNormalUser = true;
  };

  testScript = ''
    # fmt: off
    machine.wait_for_unit('multi-user.target')
    machine.succeed('su - -c "xvfb-run gtk-launch test" foo >&2')
    machine.wait_for_file('/home/foo/.cache/xdg/done')

    machine.succeed('test -d /home/foo/existing')
    machine.succeed('grep -qF foo /home/foo/existing/bar')
    machine.fail('test -d /home/foo/nonexisting')

    machine.succeed('grep -qF XDG1 /home/foo/.local/share/xdg/1')
    machine.succeed('grep -qF XDG2 /home/foo/.config/xdg/2')
    machine.succeed('grep -qF XDG3 /home/foo/.cache/xdg/3')

    machine.succeed('test "$(< /home/foo/.cache/xdg/procpids)" = /proc/1')
    machine.succeed('test "$(< /home/foo/.cache/xdg/ownpid)" = 1')

    machine.succeed('test "$(su -c test-sandbox2 foo)" = "/bin/sh works"')

    machine.succeed('su -c "echo root netns | nc -N 127.0.0.1 3000" foo')
    machine.succeed('test "$(su -c test-sandbox3 foo)" = "netcat has failed"')
    machine.fail('grep -F "hello network" /tmp/netns.log')
    machine.succeed('grep -F "root netns" /tmp/netns.log')

    machine.succeed('test "$(su -c test-sandbox4 foo)" = "no pid namespace"')
  '';
}

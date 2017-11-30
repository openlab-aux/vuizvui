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
    environment.systemPackages = let
      testProgram = pkgs.writeScriptBin "test-sandbox" ''
        #!${pkgs.stdenv.shell} -ex

        # Should fail because we can't access the host's PATH
        ! echo foo | grep -qF foo

        export PATH=/run/baz-test-sandbox/bin
        echo foo > /home/foo/existing/bar
        test ! -d /home/foo/nonexisting
        /run/foo-test-sandbox/bin/hello
        echo aaa | /run/bar-test-sandbox/bin/sed -e 's/a/b/g'

        echo XDG1 > /home/foo/.local/share/xdg/1
        echo XDG2 > /home/foo/.config/xdg/2
        echo XDG3 > /home/foo/.cache/xdg/3
      '';
    in lib.singleton (pkgs.vuizvui.buildSandbox testProgram {
      paths.required = [
        "/home/foo/existing"
        "$XDG_DATA_HOME/xdg"
        "$XDG_CONFIG_HOME/xdg"
        "$XDG_CACHE_HOME/xdg"
      ];
      paths.wanted = [ "/home/foo/nonexisting" ];
      paths.runtimeVars = [ "COLLECT_ME" ];
    });
    users.users.foo.isNormalUser = true;
  };

  testScript = ''
    $machine->waitForUnit('multi-user.target');
    $machine->succeed('su - -c test-sandbox foo >&2');

    $machine->succeed('test -d /home/foo/existing');
    $machine->succeed('grep -qF foo /home/foo/existing/bar');
    $machine->fail('test -d /home/foo/nonexisting');

    $machine->succeed('grep -qF XDG1 /home/foo/.local/share/xdg/1');
    $machine->succeed('grep -qF XDG2 /home/foo/.config/xdg/2');
    $machine->succeed('grep -qF XDG3 /home/foo/.cache/xdg/3');
  '';
}

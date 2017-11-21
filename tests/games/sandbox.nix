{
  name = "sandbox";

  machine = { pkgs, lib, ... }: {
    nixpkgs.config.vuizvui.games = {};
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
        ! echo foo | grep -qF foo
        export PATH=/run/baz-test-sandbox/bin
        echo foo > /home/foo/existing/bar
        test ! -d /home/foo/nonexisting
        /run/foo-test-sandbox/bin/hello
        echo aaa | /run/bar-test-sandbox/bin/sed -e 's/a/b/g'
      '';
    in lib.singleton (pkgs.vuizvui.games.buildSandbox testProgram {
      paths.required = [ "/home/foo/existing" ];
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
  '';
}

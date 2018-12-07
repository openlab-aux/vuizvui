{ pkgs, nixpkgsPath, ... }:

let
  mkExpect = expectScript: script: pkgs.writeScript "test-gnupg-cli" ''
    #!${pkgs.expect}/bin/expect -f
    set timeout 20
    spawn ${pkgs.writeScript "cli-testscript.sh" ''
      #!${pkgs.stdenv.shell} -e
      ${script}
    ''}
    ${expectScript}
    set ret [wait]
    exit [lindex $ret 3]
  '';

  cliTestWithPassphrase = mkExpect ''
    expect -regexp ---+.*Please.enter
    send supersecret\r
  '';

  cliTest = mkExpect "";

in {
  name = "gnupg";

  enableOCR = true;

  machine = { lib, ... }: {
    imports = map (what:
      "${nixpkgsPath}/nixos/tests/common/${what}.nix"
    ) [ "user-account" "x11" ];

    services.openssh.enable = true;
    services.xserver.displayManager.auto.user = "alice";

    vuizvui.programs.gnupg.enable = true;
    vuizvui.programs.gnupg.agent.enable = true;
    vuizvui.programs.gnupg.agent.sshSupport = true;
    vuizvui.programs.gnupg.agent.scdaemon.enable = true;

    programs.ssh.startAgent = false;
  };

  testScript = ''
    $machine->waitForUnit("sshd.service");
    $machine->succeed("ssh-keygen -t ed25519 -f /root/id_ed25519 -N '''");
    my $cmd = 'mkdir -p ~/.ssh && cat > ~/.ssh/authorized_keys';
    $machine->succeed("su -c 'umask 0077; $cmd' alice < /root/id_ed25519.pub");

    $machine->waitForX;

    sub ssh ($) {
      my $esc = $_[0] =~ s/'/'\\${"'"}'/gr;
      return "ssh -q -i /root/id_ed25519".
             " -o StrictHostKeyChecking=no".
             " alice\@127.0.0.1 -- '$esc'";
    }

    sub xsu ($) {
      my $esc = $_[0] =~ s/'/'\\${"'"}'/gr;
      return "DISPLAY=:0 su alice -c '$esc'";
    }

    $machine->nest("import snakeoil key", sub {
      $machine->succeed(ssh "${cliTestWithPassphrase ''
        gpg --import ${./snakeoil.asc}
      ''}");
      $machine->succeed(ssh "${mkExpect ''
        expect gpg>
        send trust\r
        expect decision?
        send 5\r
        expect "Do you really want"
        send y\r
        expect gpg>
        send save\r
      '' "gpg --edit-key ECC15FE1"}");
    });

    subtest "test SSH agent support", sub {
      $machine->succeed(ssh 'ssh-keygen -t ed25519 -f ~/testkey -N ""');
      $machine->succeed(ssh '${mkExpect ''
        expect -regexp ---+.*Please.enter
        send supersecret\r
        expect -regexp ---+.*Please.re-en
        send supersecret\r
      '' "ssh-add ~/testkey"}');

      $machine->succeed("umask 0077; $cmd < ~alice/testkey.pub");
      $machine->succeed(ssh 'rm ~/testkey*');

      $machine->succeed(ssh 'ssh -o StrictHostKeyChecking=no root@127.0.0.1'.
                            ' touch /i_have_thu_powarr');
      $machine->succeed("test -e /i_have_thu_powarr");

      $machine->succeed(ssh "systemctl --user reload gpg-agent");

      $machine->succeed(ssh "${cliTestWithPassphrase ''
        ssh -o StrictHostKeyChecking=no root@127.0.0.1 \
          touch /i_still_have_thu_powarr
      ''}");
      $machine->succeed("test -e /i_still_have_thu_powarr");
    };

    subtest "socket persists after restart", sub {
      $machine->succeed(ssh 'test -e "$SSH_AUTH_SOCK"');
      $machine->succeed(ssh 'systemctl --user stop gpg-agent.service');
      $machine->succeed(ssh 'test -e "$SSH_AUTH_SOCK"');
    };

    subtest "test from SSH", sub {
      $machine->execute(ssh "systemctl --user reload gpg-agent");
      $machine->succeed(ssh "${cliTestWithPassphrase ''
        echo encrypt me > to_encrypt
        gpg -sea -r ECC15FE1 to_encrypt
        rm to_encrypt
      ''}");
      $machine->succeed(ssh "${cliTest ''
        [ "$(gpg -d to_encrypt.asc)" = "encrypt me" ]
      ''}");
    };

    subtest "test from X", sub {
      $machine->execute(ssh "systemctl --user reload gpg-agent");
      my $pid = $machine->succeed(xsu
        'echo encrypt me | gpg -sea -r ECC15FE1 > encrypted_x.asc & echo $!'
      );
      chomp $pid;
      $machine->waitForText(qr/[Pp]assphrase/);
      $machine->screenshot("passphrase_dialog");
      $machine->sendChars("supersecret\n");
      $machine->waitUntilFails("kill -0 $pid");
      $machine->succeed(xsu '[ "$(gpg -d encrypted_x.asc)" = "encrypt me" ]');
    };
  '';
}

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
    test-support.displayManager.auto.user = "alice";

    vuizvui.programs.gnupg.enable = true;
    vuizvui.programs.gnupg.agent.enable = true;
    vuizvui.programs.gnupg.agent.sshSupport = true;
    vuizvui.programs.gnupg.agent.scdaemon.enable = true;

    programs.ssh.startAgent = false;
  };

  testScript = ''
    # fmt: off
    from shlex import quote

    machine.wait_for_unit("sshd.service")
    machine.succeed("ssh-keygen -t ed25519 -f /root/id_ed25519 -N '''")
    cmd = 'mkdir -p ~/.ssh && cat > ~/.ssh/authorized_keys'
    machine.succeed(f"su -c 'umask 0077; {cmd}' alice < /root/id_ed25519.pub")

    machine.wait_for_x()

    def ssh(cmd: str) -> str:
      # XXX: Redirecting stdout to /dev/null is a workaround and we ideally
      #      should *ONLY* get stdout on error.
      return "ssh -q -i /root/id_ed25519 -o StrictHostKeyChecking=no" \
             f" alice@127.0.0.1 -- {quote(cmd)} > /dev/null"

    def xsu(cmd: str) -> str:
      return f"DISPLAY=:0 su alice -c {quote(cmd)}"

    with machine.nested("import snakeoil key"):
      machine.succeed(ssh("${cliTestWithPassphrase ''
        gpg --import ${./snakeoil.asc}
      ''}"))
      machine.succeed(ssh("${mkExpect ''
        expect gpg>
        send trust\r
        expect decision?
        send 5\r
        expect "Do you really want"
        send y\r
        expect gpg>
        send save\r
      '' "gpg --edit-key ECC15FE1"}"))

    with subtest("test SSH agent support"):
      machine.succeed(ssh('ssh-keygen -t ed25519 -f ~/testkey -N ""'))
      machine.succeed(ssh('${mkExpect ''
        expect -regexp ---+.*Please.enter
        send supersecret\t
        send supersecret\r
      '' "ssh-add ~/testkey"}'))

      machine.succeed(f"umask 0077; {cmd} < ~alice/testkey.pub")
      machine.succeed(ssh('rm ~/testkey*'))

      machine.succeed(ssh(
        'ssh -o StrictHostKeyChecking=no root@127.0.0.1'
        ' touch /i_have_thu_powarr'
      ))
      machine.succeed("test -e /i_have_thu_powarr")

      machine.succeed(ssh("systemctl --user reload gpg-agent"))

      machine.succeed(ssh("${cliTestWithPassphrase ''
        ssh -o StrictHostKeyChecking=no root@127.0.0.1 \
          touch /i_still_have_thu_powarr
      ''}"))
      machine.succeed("test -e /i_still_have_thu_powarr")

    with subtest("socket persists after restart"):
      machine.succeed(ssh('test -e "$SSH_AUTH_SOCK"'))
      machine.succeed(ssh('systemctl --user stop gpg-agent.service'))
      machine.succeed(ssh('test -e "$SSH_AUTH_SOCK"'))

    with subtest("test from SSH"):
      machine.execute(ssh("systemctl --user reload gpg-agent"))
      machine.succeed(ssh("${cliTestWithPassphrase ''
        echo encrypt me > to_encrypt
        gpg -sea -r ECC15FE1 to_encrypt
        rm to_encrypt
      ''}"))
      machine.succeed(ssh("${cliTest ''
        [ "$(gpg -d to_encrypt.asc)" = "encrypt me" ]
      ''}"))

    with subtest("test from X"):
      machine.execute(ssh("systemctl --user reload gpg-agent"))
      pid = machine.succeed(xsu(
        'echo encrypt me | gpg -sea -r ECC15FE1 > encrypted_x.asc & echo $!'
      )).strip()
      machine.wait_for_text('(?i)[Pp]assphrase')
      machine.screenshot("passphrase_dialog")
      machine.send_chars("supersecret\n")
      machine.wait_until_fails(f"kill -0 {pid}")
      machine.succeed(xsu('[ "$(gpg -d encrypted_x.asc)" = "encrypt me" ]'))
  '';
}

{ nixpkgsPath, ... }:

{
  name = "flameshot";

  machine = { lib, pkgs, ... }: {
    imports = [
      "${nixpkgsPath}/nixos/tests/common/user-account.nix"
      "${nixpkgsPath}/nixos/tests/common/x11.nix"
    ];
    test-support.displayManager.auto.user = "alice";
    vuizvui.user.aszlig.programs.flameshot.enable = true;

    # Use "icewm" instead of "icewm-session" because the latter also starts
    # icewmbg, which includes a default background image that is so noisy that
    # we get bad OCR results.
    services.xserver.windowManager.session = lib.mkForce (lib.singleton {
      name = "icewm";
      start = "${pkgs.icewm}/bin/icewm & waitPID=$!";
    });
  };

  enableOCR = true;

  testScript = ''
    # fmt: off
    machine.wait_for_x()
    machine.wait_for_file("/home/alice/.Xauthority")
    machine.succeed("xauth merge ~alice/.Xauthority")
    machine.succeed('su -c "DISPLAY=:0.0 flameshot gui" - alice >&2 &')
    machine.wait_for_text('(?i)save screenshot')
    machine.screenshot('flameshot')
    machine.send_key("ctrl-s")
    machine.wait_until_succeeds('ls -1 ~alice/screenshots/ | grep -q png')
    machine.screenshot('flameshot_done')
  '';
}

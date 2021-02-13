{ nixpkgsPath, ... }:

{
  name = "flameshot";

  machine = { pkgs, ... }: {
    imports = [
      "${nixpkgsPath}/nixos/tests/common/user-account.nix"
      "${nixpkgsPath}/nixos/tests/common/x11.nix"
    ];
    test-support.displayManager.auto.user = "alice";
    vuizvui.user.aszlig.programs.flameshot.enable = true;
  };

  enableOCR = true;

  testScript = ''
    # fmt: off
    machine.wait_for_x()
    machine.wait_for_file("/home/alice/.Xauthority")
    machine.succeed("xauth merge ~alice/.Xauthority")
    machine.succeed('su -c "DISPLAY=:0.0 flameshot gui" - alice &')
    machine.wait_for_text('(?i)capture the screen')
    machine.screenshot('flameshot')
  '';
}

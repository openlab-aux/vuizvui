{ nixpkgsPath, ... }:

{
  name = "psi";

  machine = { pkgs, ... }: {
    imports = [
      "${nixpkgsPath}/nixos/tests/common/user-account.nix"
      "${nixpkgsPath}/nixos/tests/common/auto.nix"
    ];
    services.xserver.enable = true;
    test-support.displayManager.auto.enable = true;
    test-support.displayManager.auto.user = "alice";
    environment.systemPackages = [ pkgs.vuizvui.aszlig.psi ];
    services.displayManager.defaultSession = "none+evilwm";
    services.xserver.windowManager.evilwm.enable = true;
  };

  enableOCR = true;

  testScript = ''
    # fmt: off
    machine.wait_for_x()
    machine.wait_for_file("/home/alice/.Xauthority")
    machine.succeed("xauth merge ~alice/.Xauthority")
    machine.succeed('su -c "DISPLAY=:0.0 psi" - alice >&2 &')
    machine.wait_for_text('(?i)Reg.ster new account')
    machine.screenshot('psi')
  '';
}

{ nixpkgsPath, ... }:

{
  name = "psi-test";

  machine = { pkgs, ... }: {
    imports = [
      "${nixpkgsPath}/nixos/tests/common/user-account.nix"
      "${nixpkgsPath}/nixos/tests/common/x11.nix"
    ];
    test-support.displayManager.auto.user = "alice";
    environment.systemPackages = [ pkgs.vuizvui.aszlig.psi ];
  };

  enableOCR = true;

  testScript = ''
    # fmt: off
    machine.wait_for_x()
    machine.wait_for_file("/home/alice/.Xauthority")
    machine.succeed("xauth merge ~alice/.Xauthority")
    machine.succeed('su -c "DISPLAY=:0.0 psi" - alice &')
    machine.wait_for_text('(?i)Register new account')
    machine.screenshot('psi')
  '';
}

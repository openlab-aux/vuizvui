{ nixpkgsPath, ... }:

{
  name = "psi-test";

  machine = { pkgs, ... }: {
    imports = [
      "${nixpkgsPath}/nixos/tests/common/user-account.nix"
      "${nixpkgsPath}/nixos/tests/common/x11.nix"
    ];
    services.xserver.displayManager.auto.enable = true;
    services.xserver.displayManager.auto.user = "alice";
    environment.systemPackages = [ pkgs.vuizvui.aszlig.psi ];
  };

  enableOCR = true;

  testScript = ''
    $machine->waitForX;
    $machine->waitForFile("/home/alice/.Xauthority");
    $machine->succeed("xauth merge ~alice/.Xauthority");
    $machine->succeed('su -c "DISPLAY=:0.0 psi" - alice &');
    $machine->waitForText(qr/Register new account/i);
    $machine->screenshot('psi');
  '';
}

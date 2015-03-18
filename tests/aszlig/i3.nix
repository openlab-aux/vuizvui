{ pkgs, ... }:

{
  name = "i3";

  machine = { lib, ... }: {
    imports = [ <nixpkgs/nixos/tests/common/x11.nix> ];

    vuizvui.user.aszlig.profiles.base.enable = true;
    vuizvui.user.aszlig.services.i3.enable = true;

    services.xserver.windowManager.default = lib.mkForce "i3";
    /* XXX */
    fonts = {
      enableCoreFonts = true;
      enableFontDir = true;
      enableGhostscriptFonts = true;
      fonts = [
        pkgs.dosemu_fonts
        pkgs.liberation_ttf
      ];
    };
    /* !XXX */
  };

  testScript = { nodes, ... }: ''
    $machine->waitForX;
    $machine->sleep(20);
    $machine->screenshot("i3");
  '';
}

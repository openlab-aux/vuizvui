import <nixpkgs/nixos/tests/make-test.nix> ({ pkgs, ... }: {
  name = "i3";

  machine = { lib, ... }: {
    imports = [
      ../common.nix
      <nixpkgs/nixos/tests/common/x11.nix>
    ];
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
    vuizvui = {
      i3.enable = true;
    };
  };

  testScript = { nodes, ... }: ''
    $machine->waitForX;
    $machine->sleep(20);
    $machine->screenshot("i3");
  '';
})

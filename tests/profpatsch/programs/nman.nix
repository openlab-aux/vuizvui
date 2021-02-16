{ nixpkgsPath, ... }:

{
  name = "nman-test";

  machine = { pkgs, lib, ... }: {
    nix.nixPath = lib.mkForce [
      "nixpkgs=${pkgs.path}"
    ];

    documentation.man.enable = false;

    environment.systemPackages = [
      pkgs.mandoc
      pkgs.vuizvui.profpatsch.nman
    ];

    # add all outputs of packages which are later requested
    # by nman in tests. note that only a single output is
    # added which allows us to test that nman accesses the
    # right outputs first and is as lazy as possible
    # (any other outputs need network to be realised).
    system.extraDependencies = [
      pkgs.lowdown.man
      pkgs.man-pages.out
      pkgs.libunwind.devman
      pkgs.w3m.out
    ];

    environment.variables = {
      PAGER = "cat";
    };
  };

  testScript = ''
    # fmt: off
    machine.start()

    # man pages which exist
    machine.succeed("nman lowdown | grep LOWDOWN.1.")
    machine.succeed("nman w3m | grep W3M.1.")
    machine.succeed("nman lowdown 3 | grep LOWDOWN.3.")
    machine.succeed("nman lowdown lowdown_html_rndr | grep LOWDOWN_HTML_RNDR.3.")
    machine.succeed("nman mandoc 7 man | grep MAN.7.")
    machine.succeed("nman libunwind unw_init_local | grep UNW_INIT_LOCAL.3.")

    # man pages which should not be found
    machine.fail("nman aewukaishenaiugenaifesphg")
    machine.fail("nman man-pages 50 realpath")
    machine.fail("nman man-pages does-not-exist")
    machine.fail("nman man-pages 3 does-not-exist")
  '';
}

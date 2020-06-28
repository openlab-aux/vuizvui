{ buildRustCrate }:

let
  libc = buildRustCrate {
    pname = "libc";
    version = "0.2.69";
    crateName = "libc";
    sha256 = "0fwi6rxklsaqcig432fg3cjamiilvv2c4jz0i3dxw7c33ipprhsz";
  };

  errno = buildRustCrate {
    pname = "errno";
    version = "0.2.5";
    crateName = "errno";
    sha256 = "0gd36jijlb17df3ffxqxqczlwdawicbbzqjwfjc4b5lzqgizm0bz";
    dependencies = [ libc ];
  };

in {
  inherit
    libc
    errno
    ;
}

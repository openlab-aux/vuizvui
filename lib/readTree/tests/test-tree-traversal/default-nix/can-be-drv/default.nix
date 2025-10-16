{ ... }:
derivation {
  name = "im-a-drv";
  system = "x86_64-linux";
  builder = "/bin/sh";
  args = [ "-c" ''echo "" > $out'' ];
}

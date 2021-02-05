# The only deployment tool that anybody should take seriously
{ pkgs, getBins }:

let
  bins = getBins pkgs.coreutils [ "realpath" ]
      // getBins pkgs.openssh [ "ssh" ]
      // getBins pkgs.nix [ "nix-build" "nix-copy-closure" ]
      ;

  deploy = pkgs.writers.writeDash "deploy-machine-profpatsch" ''
    set -e
    MACHINE="''${1?please set machine as first argument}"
    OUT_LINK="system-$MACHINE"

    ${bins.nix-build} \
      --show-trace \
      --out-link "$OUT_LINK" \
      -I "nixpkgs=$HOME/nixpkgs" \
      -A "machines.profpatsch.$MACHINE.build" \
      "$HOME/vuizvui"

    ${bins.nix-copy-closure} \
      --to "ssh://$MACHINE?compress=true" \
      "$OUT_LINK"

    ${bins.ssh} \
      "root@$MACHINE" \
      "$(${bins.realpath} $OUT_LINK)/bin/switch-to-configuration" \
      "switch"
  '';

in {
  inherit
    deploy
    ;
}

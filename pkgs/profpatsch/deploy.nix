# The only deployment tool that anybody should take seriously
{ pkgs, getBins }:

let
  bins = getBins pkgs.coreutils [ "realpath" ]
      // getBins pkgs.openssh [ "ssh" ]
      // getBins pkgs.nix [ "nix-build" "nix-copy-closure" "nix-env" ]
      ;

  deploy = pkgs.writers.writeDash "deploy-machine-profpatsch" ''
    set -e
    MACHINE="''${1?please set machine as first argument}"
    HOME="''${HOME?please make sure HOME is set}"
    VUIZVUI="$HOME/vuizvui"
    OUT_LINK="$VUIZVUI/machines/profpatsch/system-$MACHINE"

    cmd() {
      echo "$" "$@" 1>&2
      "$@"
    }

    cmd ${bins.nix-build} \
      --show-trace \
      --out-link "$OUT_LINK" \
      -I "nixpkgs=$HOME/nixpkgs" \
      -A "machines.profpatsch.$MACHINE.build" \
      "$VUIZVUI"

    # copy all required paths to the machine
    cmd ${bins.nix-copy-closure} \
      --to "$MACHINE?compress=true" \
      --use-substitutes \
      ${create-system-profile-and-switch} \
      "$OUT_LINK"

    # activate the system
    cmd ${bins.ssh} \
      "root@$MACHINE" \
      ${create-system-profile-and-switch} \
      "$(${bins.realpath} $OUT_LINK)"
  '';

  create-system-profile-and-switch = pkgs.writers.writeDash "create-system-profile-and-switch" ''
    set -e
    system="$1"
    ${bins.nix-env} -p /nix/var/nix/profiles/system --set $system
    $system/bin/switch-to-configuration switch
  '';

in {
  inherit
    deploy
    ;
}

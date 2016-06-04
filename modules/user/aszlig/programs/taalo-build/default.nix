{ config, pkgs, lib, ... }:

let
  backend = pkgs.writeScript "taalo-realize-backend" ''
    #!${pkgs.perl}/bin/perl -I${pkgs.nix}/lib/perl5/site_perl
    use strict;
    use Nix::CopyClosure;
    use Nix::SSH;
    use IPC::Open2;

    binmode STDERR, ":encoding(utf8)";

    my ($from, $to);
    my $dest = 'nix-remote-build@taalo.headcounter.org';
    my $cmd = "exec ssh $dest -C -- nix-store --serve --write";
    my $pid = open2($from, $to, $cmd);

    # Do the handshake.
    my $magic;
    eval {
        my $SERVE_MAGIC_1 = 0x390c9deb; # FIXME
        my $clientVersion = 0x200;
        syswrite($to, pack("L<x4L<x4", $SERVE_MAGIC_1, $clientVersion))
          or die;
        $magic = readInt($from);
    };

    die "unable to connect to taalo\n" if $@;
    die "did not get valid handshake from taalo\n" if $magic != 0x5452eecb;

    my $serverVersion = readInt($from);
    die "unsupported server version\n"
      if $serverVersion < 0x200 || $serverVersion >= 0x300;

    Nix::CopyClosure::copyToOpen(
      $from, $to, "taalo", \@ARGV, 0, 0, 0, 1
    );

    writeInt(6, $to) or die;
    writeStrings(\@ARGV, $to);
    writeInt(0, $to);
    writeInt(0, $to);

    my $res = readInt($from);

    close $to;

    waitpid($pid, 0);
    exit $res;
  '';

  taalo-realize = pkgs.writeScriptBin "taalo-realize" ''
    #!${pkgs.stdenv.shell}
    if [ $# -le 0 -o "$1" = "--help" -o "$1" = "-h" ]; then
      echo "Usage: $0 DERIVATION..." >&2
      exit 1
    fi

    exec ${backend} "$@"
  '';

  taalo-build = pkgs.writeScriptBin "taalo-build" ''
    #!${pkgs.stdenv.shell}
    drvs="$(nix-instantiate "$@" | cut -d'!' -f1)" || exit 1
    exec ${backend} $drvs
  '';

in {
  options.vuizvui.user.aszlig.programs.taalo-build = {
    enable = lib.mkEnableOption "aszlig's build helpers for remote builds";
  };
  config = lib.mkIf config.vuizvui.user.aszlig.programs.taalo-build.enable {
    environment.systemPackages = [ taalo-realize taalo-build ];
  };
}

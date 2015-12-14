{ config, pkgs, lib, ... }:

with lib;

{
  options.vuizvui.user.aszlig.programs.taalo-build = {
    enable = mkEnableOption "aszlig's build helper for remote builds";
  };
  config = mkIf config.vuizvui.user.aszlig.programs.taalo-build.enable {
    environment.systemPackages = singleton (pkgs.writeScriptBin "taalo-build" ''
      #!${pkgs.perl}/bin/perl -I${pkgs.nix}/lib/perl5/site_perl
      use strict;
      use Nix::CopyClosure;
      use Nix::SSH;
      use IPC::Open2;

      binmode STDERR, ":encoding(utf8)";

      open my $instantiate, "-|", "nix-instantiate", @ARGV
        or die "Failed to run nix-instantiate";
      my $to_realize = join "", <$instantiate>;
      close $instantiate or exit $? >> 8;

      chomp $to_realize;

      my ($from, $to);
      my $cmd = "nixops ssh -d headcounter taalo -- -C "
              . "nix-store --serve --write";
      my $pid = open2($from, $to, "exec ssh -x -a -C mmrnmhrm '$cmd'");

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
        $from, $to, "taalo", [$to_realize], 0, 0, 0, 1
      );

      writeInt(6, $to) or die;
      writeStrings([$to_realize], $to);
      writeInt(0, $to);
      writeInt(0, $to);

      my $res = readInt($from);

      close $to;

      waitpid($pid, 0);
      exit $res;
    '');
  };
}

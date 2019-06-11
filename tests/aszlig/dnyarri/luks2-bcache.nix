{
  name = "dnyarri-luks2-bcache";

  nodes.machine = { pkgs, ... }: {
    environment.systemPackages = [
      pkgs.cryptsetup pkgs.bcache-tools pkgs.btrfs-progs
    ];
    virtualisation.memorySize = 2048;
    virtualisation.emptyDiskImages = [ 5 2048 2048 512 ];
  };

  nodes.newmachine = { pkgs, lib, ... }: {
    virtualisation.memorySize = 2048;
    virtualisation.emptyDiskImages = [ 5 2048 2048 512 ];
    boot.initrd.luks.devices = lib.mkOverride 0 {
      test1.device = "/dev/disk/by-uuid/07b821b9-0912-4f03-9ebc-89f41704caff";
      test1.keyFile = "/dev/vdb";
      test2.device = "/dev/disk/by-uuid/d140fd40-bb3c-48b5-98e0-b75878dbce66";
      test2.keyFile = "/dev/vdb";
    };
    boot.initrd.availableKernelModules = [ "bcache" ];
    boot.initrd.postMountCommands = ''
      test 'hello test' = "$(cat /mnt-root/test/hi.txt)" || exit 1
    '';
    fileSystems = lib.mkVMOverride {
      "/test" = {
        fsType = "btrfs";
        label = "testfs";
        neededForBoot = true;
      };
    };
  };

  testScript = let
    luksOpts = "--type LUKS2 --pbkdf argon2id -s 512 -h sha512";
    luksFormat = "cryptsetup luksFormat -q ${luksOpts}";
    uuid1 = "07b821b9-0912-4f03-9ebc-89f41704caff";
    uuid2 = "d140fd40-bb3c-48b5-98e0-b75878dbce66";
  in ''
    $machine->waitForUnit('multi-user.target');

    $machine->nest('setting up LUKS2 and bcache backing devices', sub {
      $machine->succeed('dd if=/dev/urandom of=/dev/vdb bs=1 count=200');

      $machine->succeed('make-bcache -B /dev/vdc');
      $machine->succeed('make-bcache -B /dev/vdd');

      $machine->waitUntilSucceeds(
        '[ $(echo /dev/bcache[0-9]* | wc -w) -eq 2 ]'
      );
      my $bcache1 = $machine->succeed('ls -1 /dev/bcache[0-9]* | head -n1');
      chomp $bcache1;
      my $bcache2 = $machine->succeed('ls -1 /dev/bcache[0-9]* | tail -n1');
      chomp $bcache2;

      $machine->succeed(
        "${luksFormat} $bcache1 --uuid ${uuid1} /dev/vdb",
        "cryptsetup open $bcache1 l1 --key-file=/dev/vdb",
      );

      $machine->succeed(
        "${luksFormat} $bcache2 --uuid ${uuid2} /dev/vdb",
        "cryptsetup open $bcache2 l2 --key-file=/dev/vdb",
      );

      $machine->succeed(
        'mkfs.btrfs -L testfs -m raid1 -d raid1 /dev/mapper/l1 /dev/mapper/l2',
        'btrfs dev scan',
        'mkdir /mnt-test',
        'mount /dev/disk/by-label/testfs /mnt-test',
        'echo hello test > /mnt-test/hi.txt',
        'umount /mnt-test',
        'cryptsetup close l1',
        'cryptsetup close l2',
      );
    });

    $machine->nest('rebooting into new configuration', sub {
      $machine->shutdown;
      $newmachine->{stateDir} = $machine->{stateDir};
      $newmachine->waitForUnit('multi-user.target');
    });

    my $bcache1 =
      $newmachine->succeed('cd /dev; ls -1 bcache[0-9]* | head -n1');
    chomp $bcache1;
    my $bcache2 =
      $newmachine->succeed('cd /dev; ls -1 bcache[0-9]* | tail -n1');
    chomp $bcache2;

    $machine->nest('attaching bcache cache device', sub {
      my $csetuuid = $newmachine->succeed(
        'make-bcache -C /dev/vde | sed -n -e "s/^Set UUID:[^a-f0-9]*//p"'
      );
      chomp $csetuuid;

      $newmachine->nest('wait for cache device to appear', sub {
        $newmachine->waitUntilSucceeds("test -e /sys/fs/bcache/$csetuuid");
      });

      $newmachine->succeed(
        "echo $csetuuid > /sys/block/$bcache1/bcache/attach",
        "echo writeback > /sys/block/$bcache1/bcache/cache_mode",
        "echo $csetuuid > /sys/block/$bcache2/bcache/attach",
        "echo writeback > /sys/block/$bcache2/bcache/cache_mode"
      );
    });

    $machine->nest('write random files to test file system', sub {
      $newmachine->succeed(
        'for i in $(seq 100); do'.
        ' dd if=/dev/urandom of="/test/randfile.$i" bs=1 count=100;'.
        ' sha256sum "/test/randfile.$i" > "/test/randfile.$i.sha256"; '.
        'done'
      );
    });

    $machine->nest('reboot to clear disk buffers', sub {
      $newmachine->shutdown;
      $newmachine->waitForUnit('multi-user.target');
    });

    $machine->nest('verifying contents of random files created earlier', sub {
      $newmachine->succeed(
        'for i in $(seq 100); do'.
        ' sha256sum "/test/randfile.$i" | cmp - "/test/randfile.$i.sha256"'.
        ' || exit 1; '.
        'done'
      );
    });
  '';
}

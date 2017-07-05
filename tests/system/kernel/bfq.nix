{
  name = "bfq-kernel";

  machine = { pkgs, ... }: {
    vuizvui.system.kernel.bfq.enable = true;
    boot.kernelPackages = pkgs.linuxPackages_latest;
  };

  testScript = ''
    $machine->execute('tail /sys/block/*/queue/scheduler >&2');
    $machine->succeed('grep -HF "[bfq]" /sys/block/vda/queue/scheduler');
  '';
}

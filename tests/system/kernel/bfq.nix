{
  name = "bfq-kernel";

  machine = { pkgs, ... }: {
    vuizvui.system.kernel.bfq.enable = true;
    vuizvui.system.kernel.useBleedingEdge = true;
  };

  testScript = ''
    $machine->execute('tail /sys/block/*/queue/scheduler >&2');
    $machine->succeed('grep -HF "[bfq]" /sys/block/vda/queue/scheduler');
  '';
}

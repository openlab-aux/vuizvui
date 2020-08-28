{
  name = "bfq-kernel";

  machine = { pkgs, ... }: {
    boot.kernelPackages = pkgs.linuxPackages_latest;
    vuizvui.system.kernel.bfq.enable = true;
    virtualisation.qemu.diskInterface = "scsi";
  };

  testScript = ''
    # fmt: off
    machine.execute('tail /sys/block/*/queue/scheduler >&2')
    machine.succeed('grep -HF "[bfq]" /sys/block/sda/queue/scheduler')
  '';
}

{ pkgs, writeExecline, getBins }:

let
# split

  bins = getBins pkgs.coreutils [ "split" "mktemp" "rm" "rmdir" ]
      // getBins pkgs.lr [ "lr" ]
      // getBins pkgs.xe [ "xe" ]
      // getBins pkgs.qrencode [ "qrencode" ]
      // getBins pkgs.zbar [ "zbarimg" ]
      // getBins pkgs.sane-backends [ "scanimg" ];

  qr-code-props = {
    # second highest redundancy level
    level = "Q";
    # max amount of bytes that level Q can encode
    bytes = "1700";
  };

  # Takes a private GPG key encoded with paperkey on stdin
  # and execs into the argv for each qr code image (on stdin).
  print-qr-codes = writeExecline "print-qr-codes" {} [
    split-stdin
    "pipeline" [
      bins.qrencode
      "--level=${qr-code-props.level}"
      "--dpi=300"
      "-o-"
    ]
    "$@"
  ];

  split-stdin = pkgs.writers.writePython3 "split-stdin" {} ''
    import sys
    import subprocess

    while True:
        str = sys.stdin.buffer.read(${qr-code-props.bytes})
        if str == "":
            break
        ret = subprocess.run(sys.argv[1:], input=str)
        code = ret.returncode
        if code != 0:
            sys.exit(code if code > 0 else 128 - code)
  '';

  test = writeExecline "test12" {} [
    "foreground" [ print-qr-codes "feh" "-" ]
  ];

  
# scanning:
# for i in (seq 0 9); scanimage --device=\'fujitsu:ScanSnap iX500:1527308\' --mode=gray --resolution=100 --format=png | zbarimg -Sdisable -Sqrcode.enable --raw - | head --bytes=-1; end > out

in {
  inherit print-qr-codes;
}

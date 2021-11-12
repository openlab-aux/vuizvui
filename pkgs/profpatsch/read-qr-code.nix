{ stdenv, writeExecline, getBins, zbar, libnotify, imagemagick }:

let
  bins = getBins zbar [ "zbarimg" ]
      // getBins imagemagick [ "import" ]
      // getBins libnotify [ "notify-send" ];

  script = writeExecline "read-qr-code" {} [
    "pipeline" [
      bins.import "png:-"
    ]
    bins.zbarimg
      "-Sdisable"
      "-Sqrcode.enable"
      "--raw"
      "-"
  ];

in script // {
  meta = {
    description = "Capture a screenshot, then display the content of the QR code, if any";
  };
}

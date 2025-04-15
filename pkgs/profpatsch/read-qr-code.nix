{ stdenv, writeExecline, getBins, zbar, libnotify, imagemagick }:

let
  bins = getBins zbar [ "zbarimg" ]
      // getBins imagemagick [ "import" ];

  script = writeExecline "read-qr-code" {} [
    "pipeline" [
      bins.import "png:-"
    ]
    bins.zbarimg
      "-Sbinary"
      "-Sdisable"
      "-Sqrcode.enable"
      "--raw"
      "-"
  ];

in script // {
  meta = {
    description = "Capture a screenshot, then return the content of the QR code, if any";
  };
}

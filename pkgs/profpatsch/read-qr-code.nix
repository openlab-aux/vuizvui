{ stdenv, writeExecline, profpatsch, zbar, libnotify, imagemagick, ... }:

let
  bins = profpatsch.utils.getBins zbar [ "zbarimg" ]
      // profpatsch.utils.getBins imagemagick [ "import" ];

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

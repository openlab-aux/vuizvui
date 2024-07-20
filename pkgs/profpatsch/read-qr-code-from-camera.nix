{ stdenv, writeExecline, getBins, zbar, libnotify, imagemagick }:

let
  bins = getBins zbar [ "zbarcam" ];

  # webcam device, by trial and error
  webcam = "/dev/video1";

  script = writeExecline "read-qr-code-from-camera" {} [
    bins.zbarcam
      "--oneshot"
      "--raw"
      webcam
  ];

in script // {
  meta = {
    description = "Open an input window from the webcam, wait until a QR code is in frame, then return the content of the QR code, if any";
  };
}

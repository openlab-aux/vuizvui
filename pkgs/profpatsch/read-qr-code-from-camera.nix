{ stdenv, writeExecline, getBins, zbar, libnotify, imagemagick }:
{
  # videoDevice is a string, e.g. "/dev/video0"
  videoDevice
}:

let
  bins = getBins zbar [ "zbarcam" ];

  script = writeExecline "read-qr-code-from-camera" {} [
    bins.zbarcam
      "--oneshot"
      "--raw"
      videoDevice
  ];

in script // {
  meta = {
    description = "Open an input window from the webcam, wait until a QR code is in frame, then return the content of the QR code, if any";
  };
}

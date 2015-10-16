{ stdenv }:

{
  name = "dragonfly";
  patch = stdenv.mkDerivation {
    name = "dragonfly.patch";
    patches = [
      ./0001-ALSA-usb-audio-Add-a-volume-scale-quirk-for-AudioQue.patch
      ./0002-ALSA-usb-audio-Add-sample-rate-inquiry-quirk-for-Aud.patch
      ./0003-ALSA-Revert-add-dB-range-mapping-for-Dragonfly.patch
    ];
    buildCommand = ''
      cat $patches > "$out"
    '';
  };
}

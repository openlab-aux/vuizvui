{
  pulseaudio = true;
  chromium.enablePepperFlash = true;
  firefox.jre = true;

  # Needed for CPU microcode
  allowUnfree = true;

  packageOverrides = import ../overrides;
}

{
  pulseaudio = true;
  chromium.enablePepperFlash = true;
  firefox.jre = true;

  # Needed for CPU microcode
  allowUnfree = true;

  allowBroken = true;

  packageOverrides = import ../overrides;
}

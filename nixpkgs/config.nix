{
  pulseaudio = true;
  chromium.enableGoogleTalkPlugin = true;
  chromium.jre = true;

  firefox.jre = true;

  packageOverrides = import ../overrides;
}

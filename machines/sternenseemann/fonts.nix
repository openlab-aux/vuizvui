{ config, pkgs, ... }:

{
  config = {
    fonts = {
      fonts = with pkgs; [
        corefonts            # microsoft fonts
        ttf_bitstream_vera   # dejavu without b&w emojis
        libertine
        liberation_ttf       # free replacements for times …
        google-fonts         # vollkorn, source * pro, inconsolata
        freefont_ttf
        noto-fonts           # noto fonts: great for fallbacks
        noto-fonts-extra
        noto-fonts-cjk
        noto-fonts-emoji     # emoji primary
        twemoji-color-font   # emoji fallback
        unifont              # bitmap font, good fallback
        unifont_upper        # upper unicode ranges of unifont
      ];

      fontDir.enable = true;
      enableGhostscriptFonts = true;
      enableDefaultFonts = true;

      fontconfig = {
        enable = true;
        antialias = true;
        hinting.enable = true;
        defaultFonts = {
          monospace = [ "Bitstream Vera Sans Mono" "Noto Mono" ];
          serif = [ "Vollkorn" "Noto Serif" ];
          sansSerif = [ "Open Sans" "Noto Sans" ];
          emoji = [ "Noto Color Emoji" "Twitter Color Emoji" "Unifont" "Unifont Upper" ];
        };
      };
    };
  };
}

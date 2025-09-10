{ config, pkgs, lib, ... }:

let
  google-fonts-subset = fonts:
    pkgs.runCommandNoCC "google-fonts-subset-${pkgs.google-fonts.version}" { } ''
      # google-fonts in nixpkgs is all TTF at the moment
      source="${pkgs.google-fonts}/share/fonts/truetype"
      target="$out/share/fonts/truetype"
      mkdir -p "$target"

      for font in ${lib.escapeShellArgs fonts}; do
        cp "$source/$font"*.ttf "$target/"
      done
    '';
in

{
  config = {
    fonts = {
      enableDefaultPackages = false;
      enableGhostscriptFonts = false;

      packages = with pkgs; [
        atkinson-hyperlegible  # Sans serif for accessibility
        b612                   # highly legible font family for aircraft cockpits
        corefonts              # microsoft fonts
        eb-garamond            # free garamond port
        ibm-plex               # Striking Fonts from IBM
        jetbrains-mono         # monospace
        lmodern                # TeX font
        noto-fonts-color-emoji # emoji primary
        open-sans              # nice sans
        unifont                # bitmap font, good fallback
        unifont_upper          # upper unicode ranges of unifont
        vollkorn               # weighty serif
        (google-fonts-subset [ "InclusiveSans" ])

        noto-fonts             # noto fonts: great for fallbacks
        noto-fonts-extra
        noto-fonts-cjk-sans
      ];

      fontconfig = {
        enable = true;
        antialias = true;
        hinting.enable = true;
        defaultFonts = {
          # TODO(sterni): explicitly specify the language Noto fonts as fallback?
          monospace = [ "Jetbrains Mono NL" "Noto Sans Mono" ];
          serif = [ "Noto Serif" "Vollkorn" ];
          sansSerif = [ "Inclusive Sans" "Open Sans" "Noto Sans" ];
          emoji = [ "Noto Color Emoji" "Unifont" "Unifont Upper" ];
        };
      };
    };
  };
}

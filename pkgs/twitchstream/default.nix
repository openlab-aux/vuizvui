{ stdenv, fetchurl, writeScriptBin, ffmpeg_2, pulseaudio }:

# FIXME: Clean up this whole file!

with stdenv.lib;

let
  streams = {
    dnyarri = {
      width = 1920;
      height = 1080;
      monitor = 1;
    };
    mmrnmhrm = {
      width = 1600;
      height = 1280;
      monitor = 1;
    };
  };

  sumAttr = name: attrs: acc: acc + (getAttr name attrs);
  maxAttr = name: attrs: acc: let
    current = getAttr name attrs;
  in if acc > current then acc else current;

  fullwidth = fold (sumAttr "width") 0 (attrValues streams);
  maxheight = fold (maxAttr "height") 0 (attrValues streams);

  resolution = "1920x1080";
  fps = 15;
  quality = "slow";

  encoder = let
    aacenc = stdenv.mkDerivation rec {
      name = "vo-aacenc-0.1.3";
      src = fetchurl {
        url = "mirror://sourceforge/opencore-amr/${name}.tar.gz";
        sha256 = "0dhghm3c8pqrriwwyj5x9i0yf52fmdfijbgqqkvqvwarldvp86p5";
      };
    };
    base = ffmpeg_2.override { x11grabSupport = true; };
  in stdenv.lib.overrideDerivation base (attrs: {
    configureFlags = attrs.configureFlags ++ [
      "--enable-libpulse"
      "--enable-version3"
      "--enable-libvo-aacenc"
    ];
    preConfigure = ''
      addPkgConfigPath "${pulseaudio}"
      addPkgConfigPath "${aacenc}"
    '';
    NIX_CFLAGS_COMPILE = "-I${aacenc}/include -L${aacenc}/lib";
    buildInputs = attrs.buildInputs ++ [ pulseaudio aacenc ];
  });

  script = let
    combine = [
      "color=c=black:s=1248x640 [surface]"
      "[0:v] setpts=PTS-STARTPTS, scale=680x540 [left]"
      "[1:v] setpts=PTS-STARTPTS, scale=568x640 [right]"
      "[surface][left] overlay=0:0 [leftonly]"
      "[leftonly][right] overlay=680:0 [out]"
    ];
  /*
    combine = [
      "color=c=black:s=${toString fullwidth}x${toString maxheight} [surface]"
      "[surface][0:v] overlay=0:0 [leftonly]"
      "[leftonly][1:v] overlay=${toString streams.dnyarri.width}:0 [out]"
    ];
  */
    nop = x: "\\";
  in ''
    #!${stdenv.shell}
    keyfile="$HOME/.twitch.key"
    if [ ! -e "$keyfile" ]; then
      echo "You need to put your streaming key into $keyfile!" >&2
      echo "To obtain the key, please visit the following URL:" >&2
      echo "http://www.twitch.tv/broadcast/dashboard/streamkey" >&2
      exit 1
    fi

    ${encoder}/bin/ffmpeg -loglevel warning \
      -f x11grab -s "${resolution}" -r "${toString fps}" -i "$DISPLAY+1920,0" \
      -f pulse -ac 2 -i default \
      -codec:v libx264 -s 1280x720 -preset:v fast -crf 24 -pix_fmt yuv420p \
      -codec:a libvo_aacenc -ar 44100 -threads auto -b:a 128k -bufsize 8k \
      -f flv "rtmp://live-fra.twitch.tv/app/$(< "$keyfile")" "$@"
  '';

  disabled = ''
    ${encoder}/bin/ffmpeg \
      -f x11grab -s "${resolution}" -r "${toString fps}" -i "$DISPLAY+1920,0" \
      ${nop ''
      -i 'tcp://dnyarri:7891?listen' \
      ''}
      -f pulse -ac 2 -i default \
      ${nop ''
      -filter_complex "${concatStringsSep "; " combine}" \
      -map "[out]" -map 2:a,0:v \
      -c:v libx264 -preset "${quality}" -s 1280x720 \
                   -b 2500k -minrate 2500k -maxrate 2500k \
                   -tune film -qscale:v 1 -threads:v 4 -crf 1 -tune animation \
      -c:a libmp3lame -ar 44100 -qscale:a 1 -bufsize 512k -threads 4 \
      -framerate "${toString fps}" \
      -force_key_frames 2 -b 2500k -minrate 2500k -maxrate 2500k \
      -g 2 -keyint_min 2 \
      ''}
      -c:v libx264 -preset fast -pix_fmt yuv420p -s 1280x800 -threads 0 \
      -c:a libmp3lame -ab 128k -ar 44100 -threads 0 \
      -f flv "rtmp://live-fra.twitch.tv/app/$(< "$keyfile")" "$@"
  '';
in writeScriptBin "twitchstream" script

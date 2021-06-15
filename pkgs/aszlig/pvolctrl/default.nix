{ stdenv, fetchurl, pkg-config, libpulseaudio }:

stdenv.mkDerivation rec {
  name = "pvolctrl-0.23";

  unpackPhase = let
    baseurl = "https://sites.google.com/site/guenterbartsch/blog/"
            + "volumecontrolutilityforpulseaudio/";
    makefile = fetchurl {
      url = baseurl + "Makefile";
      sha256 = "0l2ffvb617csk6h29y64v6ywhpcp7la6vvcip1w4nq0yry6jhrqz";
    };
    source = fetchurl {
      url = baseurl + "pvolctrl.c";
      sha256 = "0vcd5dlw9l47jpabwmmzdvlkn67fz55dr3sryyh56sl263mibjda";
    };
  in ''
    mkdir -p "${name}"
    sed -e 's|/usr/bin/||' "${makefile}" > "${name}/Makefile"
    sed -e 's/PA_VOLUME_MAX/PA_VOLUME_NORM/
    /avg_vol += (avg_vol \* vol_mod) \/ 100;/ {
      s/(avg_vol/((int)PA_VOLUME_NORM/
    }
    /if (vol_mod)/i \
      if (info->name == NULL || strncmp(info->name, "combined", 8) != 0) \
        return;' "${source}" > "${name}/pvolctrl.c"
    sourceRoot="${name}"
  '';

  installPhase = ''
    install -D -T pvolctrl "$out/bin/pvolctrl"
  '';

  buildInputs = [ pkg-config libpulseaudio ];
}

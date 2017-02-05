{ lib, fetchFromGitHub, writeScriptBin, curl, bash, gawk
, haskellPackages, mpg321 }:

let
  repo = fetchFromGitHub {
    owner = "openlab-aux";
    repo = "stackenblocken";
    rev = "labpingbot";
    sha256 = "1x319sbkk8hl3lad2zapkdv6ihqqsl8f5l0a2n9fvppcm5c7pz0d";
 };

 bot = haskellPackages.callPackage "${repo}/stackenblocken.nix" {};
 jingle = "${repo}/stackenblocken_jingle.mp3";

 script = ''
    #!${lib.getBin bash}/bin/bash
    percent=10
    no_stackenblocken="no STACKENBLOCKEN today"
    tmpd=$(mktemp -d)

    # kill everything on SIGINT
    trap exit SIGINT
    # also running background processes
    trap "kill 0" EXIT

    function icsfile {
      ${lib.getBin gawk}/bin/awk -v date=''${1:-nodate} '
        /BEGIN:VEVENT/ { cache = 1; }
        /DTSTART:/ {
          if( index( $0, date ) )
            printf( "%s", cached_lines );
          else
            drop = 1;
          cached_lines = "";
          cache = 0;
        }
        cache  {
          cached_lines = cached_lines $0 "\n";
          next;
        };
        !drop { print; }
        /END:VEVENT/ { drop = 0; }
      '
    }

    function check_events {
      ${lib.getBin curl}/bin/curl -s https://openlab-augsburg.de/veranstaltungen/events.ics \
        | icsfile `date --utc +%Y%m%d` \
        > "$tmpd/events-today"

      # filter out events that have the no-stackenblocken tag
      # and skip it on those days
      if <"$tmpd/events-today" grep -q "CATEGORIES.*no-stackenblocken"; then
        events=$(<$tmpd/events-today sed -ne 's/SUMMARY:\(.*\)$/\1/p')
        echo "$no_stackenblocken because of event(s):"
        echo "$events"
        exit 0
      fi
    }

    function check_random {
      rnumber=$RANDOM
      ((rnumber %= 100))
      # lt for percent (numbers begin from 0)
      if [ $rnumber -lt $percent ]; then
        echo "$no_stackenblocken because lucks says so! ($percent% chance)"
        exit 0
      fi
    }

    check_events
    check_random

    for i in $(seq 2); do
      echo "starting .labping bot"
      ${lib.getBin bot}/bin/stackenblocken &
      echo "DOING STACKENBLOCKEN"
      ${lib.getBin mpg321}/bin/mpg321 --gain 40 -q ${jingle}
    done
  '';


in
  writeScriptBin "stackenblocken" script


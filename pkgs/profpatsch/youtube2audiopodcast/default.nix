{ pkgs, lib, writeExecline, writeHaskellInterpret, getBins, runInEmptyEnv, sandbox }:

config@{ url, internalPort }:

let
  bins = getBins pkgs.hello [ "hello" ]
    // getBins pkgs.coreutils [ "printf" "wc" "tr" "cut" "mktemp" "mkdir" "ls" ]
    // getBins pkgs.youtube-dl [ "youtube-dl" ]
    // getBins pkgs.s6-networking [ "s6-tcpserver" ]
    // getBins pkgs.execline [ "fdmove" "backtick" "importas" "if" "redirfd" "pipeline" ]
    // getBins pkgs.s6-portable-utils [
         { use = "s6-cat"; as = "cat"; }
       ]
    // getBins pkgs.jl [ "jl" ];

  # fetch the audio of a youtube video to ./audio.opus, given video ID
  youtube-dl-audio = writeExecline "youtube-dl-audio" { readNArgs = 1; } [
    bins.youtube-dl
      "--verbose"
      "--extract-audio"
      "--audio-format" "opus"
      # We have to give a specific filename (with the right extension).
      # youtube-dl is really finicky with output filenames.
      "--output" "./audio.opus"
      "https://www.youtube.com/watch?v=\${1}"
  ];

  # print youtube playlist information to stdout, given playlist ID
  youtube-playlist-info = writeExecline "youtube-playlist-info" { readNArgs = 1; } [
    bins.youtube-dl
      "--verbose"
      # don’t query detailed info of every video,
      # which takes a lot of time
      "--flat-playlist"
      # print a single line of json to stdout
      "--dump-single-json"
      "--yes-playlist"
      "https://www.youtube.com/playlist?list=\${1}"
  ];

  printFeed = writeHaskellInterpret "print-feed" {
    withPackages = hps: [ hps.feed hps.aeson ];
  } ./Main.hs;

  # minimal CGI request parser for use as UCSPI middleware
  yolo-cgi = pkgs.writers.writePython3 "yolo-cgi" {} ''
    import sys
    import os


    def parse_ass(bool):
        if not bool:
            sys.exit(1)


    inbuf = sys.stdin.buffer

    first_line = inbuf.readline().rstrip(b"\n").split(sep=b" ")
    parse_ass(len(first_line) == 3)
    parse_ass(first_line[2].startswith(b"HTTP/"))

    os.environb[b"REQUEST_METHOD"] = first_line[0]
    os.environb[b"REQUEST_URI"] = first_line[1]

    cmd = sys.argv[1]
    args = sys.argv[2:] if len(sys.argv) > 2 else []
    os.execlp(cmd, cmd, *args)
  '';

  # print the contents of an envar to the stdin of $@
  envvar-to-stdin = writeExecline "envvar-to-stdin" { readNArgs = 1; } [
    "importas" "VAR" "$1"
    "pipeline" [ bins.printf "%s" "$VAR" ] "$@"
  ];

  # serve an opus file as HTTP on stdout
  serve-http-file = content-type:
    writeExecline "serve-http-file" { readNArgs = 1; } [
      # determine file size
      bins.backtick "-i" "-n" "filesize" [
        bins.redirfd "-r" "0" "$1"
        bins.wc "--bytes"
      ]
      bins.importas "filesize" "filesize"
      # yolo html
      bins.${"if"} [ bins.printf ''
        HTTP/1.1 200 OK
        Content-Type: ${content-type}
        Content-Length: %u

      '' "$filesize" ]
      # the payload is our file
      bins.redirfd "-r" "0" "$1" bins.cat
    ];

  dispatch-request = pkgs.writers.writeDash "dispatch-request" ''
    case "$REQUEST_URI" in
      /playlist/*)
        ${bins.mkdir} /tmp >&2
        ${bins.mkdir} /work >&2
        ${print-feed-xml} "''${REQUEST_URI#/playlist/}" >/work/feed \
          && ${serve-http-file "text/xml"} /work/feed
        ;;
      /video/*)
        ${youtube-dl-audio} "''${REQUEST_URI#/video/}" 1>&2 \
          && ${serve-http-file "audio/ogg"} "./audio.opus"
        ;;
      *) return 1 ;;
    esac
  '';

  http-server = writeExecline "http-server" {} [
    (runInEmptyEnv [])
    bins.s6-tcpserver "127.0.0.1" config.internalPort
    (sandbox { extraMounts = [ "/etc" ]; })
    yolo-cgi
    dispatch-request
  ];

  example-config = pkgs.writeText "example-config.json" (lib.generators.toJSON {} {
    channelName = "Lonely Rolling Star";
    channelURL = "https://www.youtube.com/playlist?list=PLV9hywkogVcOuHJ8O121ulSfFDKUhJw66";
  });

  transform-flat-playlist-to-rss = { videoUrl }:
    let
      playlist-item-info-jl = ''
        (\o ->
          { itemTitle: o.title
          , itemYoutubeLink: append "https://youtube.com/watch?v=" o.id
          ${/*TODO how to add the url here nicely?*/""}
          , itemURL: append "${videoUrl}/" o.id

          ${/*# TODO*/""}
          , itemDescription: ""
          , itemCategory: ""
          , itemTags: []
          , itemSizeBytes: 0
          , itemHash: ""
          })
     '';
     playlist-info-jl = ''
       \pl ->
         { channelInfo:
           { channelDescription: pl.title

           ${/*# TODO*/""}
           , channelLastUpdate: "000"
           , channelImage: null
           }
         , channelItems:
           map
             ${playlist-item-info-jl}
             pl.entries
         }
     '';
   in writeExecline "youtube-dl-playlist-json-to-rss-json" {} [
    bins.jl playlist-info-jl
  ];

  ex = "PLV9hywkogVcOuHJ8O121ulSfFDKUhJw66";

  print-feed-xml = writeExecline "print-feed-xml" { readNArgs = 1; } [
    "pipeline" [
      youtube-playlist-info "$1"
    ]
    "pipeline" [
      (transform-flat-playlist-to-rss { videoUrl = "${config.url}/video"; })
    ]
    printFeed
  ];

in http-server

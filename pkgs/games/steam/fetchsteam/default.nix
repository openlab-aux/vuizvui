{ stdenv, runCommand, writeText, fetchFromGitHub, buildDotnetPackage
, username, password
}:

{ name, appId, depotId, manifestId, branch ? null, sha256, fileList ? [] }:

let
  protobuf-net = buildDotnetPackage rec {
    baseName = "protobuf-net";
    version = "2.0.0.668";

    src = fetchFromGitHub {
      owner = "mgravell";
      repo = "protobuf-net";
      rev = "r668";
      sha256 = "1060pihqkbr9pd2z6m01d6fsbc9nj56m6y5a0pch9mqdmviv4896";
    };

    sourceRoot = "${src.name}/${baseName}";
  };

  SteamKit2 = buildDotnetPackage rec {
    baseName = "SteamKit2";
    version = "1.6.4";

    src = fetchFromGitHub {
      owner = "SteamRE";
      repo = "SteamKit";
      rev = "SteamKit_${version}";
      sha256 = "17d7wi2f396qhp4w9sf37lazvsaqws8x071hfis9gv5llv6s7q46";
    };

    buildInputs = [ protobuf-net ];

    xBuildFiles = [ "SteamKit2/SteamKit2.sln" ];
    outputFiles = [ "SteamKit2/SteamKit2/bin/Release/*" ];
  };

  DepotDownloader = buildDotnetPackage rec {
    baseName = "DepotDownloader";
    version = "2.1.1git20160207";

    src = fetchFromGitHub {
      owner = "SteamRE";
      repo = baseName;
      rev = "5fa6621d9f9448fcd20c974b427a8bd2cb044cb4";
      sha256 = "0vb566d7x1scd96c8ybq6gdbc2cv5jjq453ld458qcvfy587amfn";
    };

    patches = [ ./downloader.patch ];

    postPatch = ''
      sed -i \
        -e 's/\(<[Rr]eference *[Ii]nclude="[^", ]\+\)[^"]*/\1/g' \
        -e 's,<[Ss]pecific[Vv]ersion>[Tt]rue</[Ss]pecific[Vv]ersion>,,g' \
        DepotDownloader/DepotDownloader.csproj
      sed -i -e 's/ version="[^"]*"//g' DepotDownloader/packages.config
    '';

    buildInputs = [ SteamKit2 protobuf-net ];

    outputFiles = [ "${baseName}/bin/Release/*" ];

    # UUUGLY, but I don't want to spend a week trying to get this working
    # without that nasty wrapper.
    makeWrapperArgs = let
      mkMono = name: path: "${path}/lib/dotnet/${name}";
      paths = stdenv.lib.mapAttrsToList mkMono {
        inherit SteamKit2 protobuf-net;
      };
      monoPath = stdenv.lib.concatStringsSep ":" paths;
    in [ "--prefix MONO_PATH : \"${monoPath}\"" ];
  };

  fileListFile = let
    content = stdenv.lib.concatStringsSep "\n" fileList;
  in writeText "steam-file-list-${name}.txt" content;

in with stdenv.lib; runCommand "${name}-src" {
  buildInputs = [ DepotDownloader ];
  inherit username password appId depotId manifestId;
  outputHashAlgo = "sha256";
  outputHash = sha256;
  outputHashMode = "recursive";
} ''
  depotdownloader -app "$appId" -depot "$depotId" -manifest "$manifestId" \
    ${optionalString (fileList != []) "-filelist \"${fileListFile}\""} \
    ${optionalString (branch != null) "-branch \"${branch}\""} \
    -username "$username" -password "$password" -dir "$out"
  rm -r "$out/.DepotDownloader"
  rm "$out/_steam_depot_manifest_$depotId.csv"
''

{ lib, buildDotnetPackage, fetchNuGet }:

buildDotnetPackage {
  baseName = "monogame-patcher";
  version = "0.1.0";

  src = lib.cleanSource ./src;

  buildInputs = [
    (fetchNuGet {
      baseName = "Mono.Cecil";
      version = "0.10-beta7";
      sha256 = "1ngjxk3wbmdwgsbdpy9yjwgc0ii8xxa78i0h57dia2rjn0gr7bw0";
      outputFiles = [ "lib/net40/*" ];
    })

    (fetchNuGet {
      baseName = "CommandLineParser";
      version = "2.2.1";
      sha256 = "02zqp98lzjv4rpjf7jl0hvhda41dlh0dc29axaapq9glk0hbmjzg";
      outputFiles = [ "lib/net45/*" ];
    })
  ];

  doInstallCheck = true;
  installCheckPhase = "$SHELL -e test.sh";
}

{ buildDotnetPackage, fetchNuGet }:

buildDotnetPackage {
  baseName = "monogame-patcher";
  version = "0.1.0";

  src = ./src;

  buildInputs = [
    (fetchNuGet {
      baseName = "Mono.Cecil";
      version = "0.10-beta7";
      sha256 = "03bina3llcnylrfrvp5psnwrfn757j7zch5r360rpdn7gmcjjcpl";
      outputFiles = [ "lib/net40/*" ];
    })

    (fetchNuGet {
      baseName = "CommandLineParser";
      version = "2.2.1";
      sha256 = "0wf8mzr16d2ni008m60rrk738v8ypk74llk6g8mlyx7rrlchnxaf";
      outputFiles = [ "lib/net45/*" ];
    })
  ];

  doInstallCheck = true;
  installCheckPhase = "$SHELL -e test.sh";
}

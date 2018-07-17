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

    (fetchNuGet {
      baseName = "NUnit";
      version = "3.10.1";
      sha256 = "159m1wpb9yy2x77x7nl0647jkpzj5j801a2inhdl7hcjys8xrqxi";
      outputFiles = [ "lib/net45/*" ];
    })

    (fetchNuGet {
      baseName = "NUnit.ConsoleRunner";
      version = "3.8.0";
      sha256 = "1gspqzfhvpc8yapni7zcr5h2y025swihv78cw07v048l3myf3pzk";
      outputFiles = [ "tools/*" ];
    })
  ];

  doCheck = true;
  checkPhase = ''
    nunitLibs="$(pkg-config nunit.framework --variable=Libraries)"
    MONO_PATH="$(dirname "$nunitLibs")" HOME="$PWD" \
      nunit3-console bin/Release/monogame-patcher.exe
  '';
}

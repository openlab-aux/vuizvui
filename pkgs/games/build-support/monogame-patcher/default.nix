{ lib, runCommand, makeWrapper, fetchNuGet, mono, dotnetPackages }:

runCommand "monogame-patcher" {
  nativeBuildInputs = [ mono makeWrapper ];

  src = ./patcher.cs;

  cecil = "${fetchNuGet {
    baseName = "Mono.Cecil";
    version = "0.10-beta7";
    sha256 = "03bina3llcnylrfrvp5psnwrfn757j7zch5r360rpdn7gmcjjcpl";
    outputFiles = [ "lib/net40/*" ];
  }}/lib/dotnet/Mono.Cecil";

  cliparser = "${fetchNuGet {
    baseName = "CommandLineParser";
    version = "2.2.1";
    sha256 = "0wf8mzr16d2ni008m60rrk738v8ypk74llk6g8mlyx7rrlchnxaf";
    outputFiles = [ "lib/net45/*" ];
  }}/lib/dotnet/CommandLineParser";

} ''
  mkdir -p "$out/bin" "$out/libexec/monogame-patcher"
  mcs "$src" -out:"$out/libexec/monogame-patcher/patcher.exe" \
    -lib:"$cecil" -lib:"$cliparser" \
    -r:Mono.Cecil -r:Mono.Cecil.Rocks -r:CommandLine

  makeWrapper ${lib.escapeShellArg "${mono}/bin/mono"} "$out/bin/$name" \
    --set MONO_PATH "$cecil:$cliparser" \
    --add-flags "$out/libexec/monogame-patcher/patcher.exe"
''

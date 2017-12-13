{ stdenv, lib, buildGame, fetchHumbleBundle, makeWrapper, mono50
, SDL2, SDL2_image, SDL2_mixer, libvorbis

, fetchNuGet, writeText
}:

let
  cecil = fetchNuGet {
    baseName = "Mono.Cecil";
    version = "0.10-beta7";
    sha256 = "03bina3llcnylrfrvp5psnwrfn757j7zch5r360rpdn7gmcjjcpl";
    outputFiles = [ "lib/net40/*" ];
  };

  patcher = writeText "patcher.cs" ''
    using System.Linq;
    using System.Collections.Generic;
    using Mono.Cecil;
    using Mono.Cecil.Cil;
    using Mono.Cecil.Rocks;

    public class patcher {
      public static void Main() {
        var module = ModuleDefinition.ReadModule("Lightning.exe");
        var typesToPatch = new List<string>() {
          "#=qpBcj_0KQZcJ0ffkrxQcNZQ==",
          "#=qEMEvBoEQXql_zRuLLk1x6w==",
          "#=qZzmwsUfWWbRLjboUcCgm9sc9s0f6yVsIVJ2gWSL_c4g=",
          "PsdImage"
        };

        var filtered = module.Types.Where(p => typesToPatch.Contains(p.Name));
        foreach (var toPatch in filtered) {
          foreach (MethodDefinition md in toPatch.Methods) {
            var il = md.Body.GetILProcessor();
            var fileStreams = md.Body.Instructions
              .Where(i => i.OpCode == OpCodes.Newobj)
              .Where(i => (i.Operand as MethodReference).DeclaringType
                          .FullName == "System.IO.FileStream");

            foreach (Instruction i in fileStreams.ToList()) {
              var fileAccessRead = il.Create(OpCodes.Ldc_I4_1);
              il.InsertBefore(i, fileAccessRead);

              var ctorType = module.AssemblyReferences.Select(
                x => new {
                  type = module.AssemblyResolver.Resolve(x)
                         .MainModule.GetType("System.IO.FileStream")
                }
              ).Where(x => x.type != null).Select(x => x.type).First();

              string wantedCtor = "System.Void System.IO.FileStream"
                                + "::.ctor(System.String,"
                                + "System.IO.FileMode,"
                                + "System.IO.FileAccess)";

              var newCtor = ctorType.GetConstructors()
                .Single(x => x.ToString() == wantedCtor);

              var refCtor = module.ImportReference(newCtor);
              il.Replace(i, il.Create(OpCodes.Newobj, refCtor));
            }
          }
        }
        module.Write("LightningPatched.exe");
      }
    }
  '';

in buildGame rec {
  name = "opus-magnum-${version}";
  version = "1";

  src = fetchHumbleBundle {
    machineName = "opus_magnum_4vffp_linux_Z9zWf";
    suffix = "zip";
    md5 = "aefe8aae8cd3dd3915be8d8cd6f3705d";
  };

  arch = if stdenv.system == "x86_64-linux" then "x86_64" else "x86";

  nativeBuildInputs = [ makeWrapper mono50 cecil ];

  patchPhase = ''
    { export MONO_PATH=${cecil}/lib/dotnet/Mono.Cecil
      mcs ${lib.escapeShellArg patcher} -out:patcher \
        -lib:${cecil}/lib/dotnet/Mono.Cecil \
        -r:Mono.Cecil -r:Mono.Cecil.Rocks
      mono patcher
      mv LightningPatched.exe Lightning.exe
    }
  '';

  buildPhase = let
    dllmap = {
      SDL2 = "${SDL2}/lib/libSDL2.so";
      SDL2_image = "${SDL2_image}/lib/libSDL2_image.so";
      SDL2_mixer = "${SDL2_mixer}/lib/libSDL2_mixer.so";
      libvorbisfile-3 = "${libvorbis}/lib/libvorbisfile.so";
    };
  in lib.concatStrings (lib.mapAttrsToList (dll: target: ''
    sed -i -e '/<dllmap.*dll="${dll}\.dll".*os="linux"/ {
      s!target="[^"]*"!target="${target}"!
    }' Lightning.exe.config
  '') dllmap);

  installPhase = ''
    mkdir -p "$out/bin" "$out/share/opus-magnum" "$out/libexec/opus-magnum"
    cp -rvt "$out/share/opus-magnum" Content PackedContent
    cp -rvt "$out/libexec/opus-magnum" Lightning.exe* Ionic.Zip.Reduced.dll

    makeWrapper ${lib.escapeShellArg mono50}/bin/mono "$out/bin/opus-magnum" \
      --add-flags "$out/libexec/opus-magnum/Lightning.exe" \
      --run "cd '$out/share/opus-magnum'"
  '';

  sandbox.paths.required = [ "$XDG_DATA_HOME/Opus Magnum" "$HOME/Desktop" ];
}

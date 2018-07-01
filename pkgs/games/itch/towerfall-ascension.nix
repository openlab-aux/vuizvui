{ stdenv, lib, buildGame, fetchItch, makeWrapper, p7zip, unzip, mono50
, SDL2, SDL2_image, libGL, libvorbis, openal

, writeScriptBin, coreutils
, fetchNuGet, writeText

, darkWorldExpansion ? true
}:

let
  cecil = fetchNuGet {
    baseName = "Mono.Cecil";
    version = "0.10-beta7";
    sha256 = "03bina3llcnylrfrvp5psnwrfn757j7zch5r360rpdn7gmcjjcpl";
    outputFiles = [ "lib/net40/*" ];
  };

  # We need to patch a few occurences of System.IO.FileStream which are used
  # with the default arguments for FileAccess. The defaults open the file in
  # read-write mode and given that all the game data files are in the read-only
  # Nix store, we'd get an UnauthorizedAccessException.
  patcher = writeText "patcher.cs" ''
    using System.Linq;
    using System.Collections.Generic;
    using Mono.Cecil;
    using Mono.Cecil.Cil;
    using Mono.Cecil.Rocks;

    public class patcher {
      private ModuleDefinition module;

      private patcher(List<string> typesToPatch) {
        this.module = ModuleDefinition.ReadModule("TowerFall.exe");

        var filtered = this.module.Types
          .Where(p => typesToPatch.Contains(p.Name));

        foreach (var toPatch in filtered) {
          patch_class(toPatch);
        }

        this.module.Write("TowerFallPatched.exe");
      }

      private void patch_method(MethodDefinition md) {
        var il = md.Body.GetILProcessor();

        var fileStreams = md.Body.Instructions
          .Where(i => i.OpCode == OpCodes.Newobj)
          .Where(i => (i.Operand as MethodReference).DeclaringType
                      .FullName == "System.IO.FileStream");

        foreach (Instruction i in fileStreams.ToList()) {
          var fileAccessRead = il.Create(OpCodes.Ldc_I4_1);
          il.InsertBefore(i, fileAccessRead);

          var ctorType = this.module.AssemblyReferences.Select(
            x => new {
              type = this.module.AssemblyResolver.Resolve(x)
                .MainModule.GetType("System.IO.FileStream")
            }
          ).Where(x => x.type != null).Select(x => x.type).First();

          string wantedCtor = "System.Void System.IO.FileStream"
                            + "::.ctor(System.String,"
                            + "System.IO.FileMode,"
                            + "System.IO.FileAccess)";

          var newCtor = ctorType.GetConstructors()
            .Single(x => x.ToString() == wantedCtor);

          var refCtor = this.module.ImportReference(newCtor);
          il.Replace(i, il.Create(OpCodes.Newobj, refCtor));
        }
      }

      private void patch_class(TypeDefinition td) {
        foreach (var nested in td.NestedTypes) patch_class(nested);
        foreach (MethodDefinition md in td.Methods) patch_method(md);
      }

      public static void Main() {
        var typesToPatch = new List<string>() {
          "Texture", "IntroScene", "SFX", "SFXVaried"
        };
        new patcher(typesToPatch);
      }
    }
  '';

in buildGame rec {
  name = "towerfall-ascension-${version}";
  version = "20160723";

  srcs = lib.singleton (fetchItch {
    name = "${name}.bin";
    gameId = 22755;
    uploadId = 243755;
    sha256 = "01ipq3z0c2k4h88r7j17nfp43p5sav12a9syangqm0syflvwqxb6";
  }) ++ lib.optional darkWorldExpansion (fetchItch {
    name = "towerfall-darkworld.zip";
    gameId = 24962;
    uploadId = 216070;
    sha256 = "1nb26m2l74rsnlwv9mv33l2s5n873867k9zypc84sm3iljvrdkmg";
  });

  unpackCmd = ''
    case "$curSrc" in
      *.bin) ${p7zip}/bin/7z x "$curSrc" data;;
      *.zip) ${unzip}/bin/unzip -qq "$curSrc" -d data;;
      *) false;;
    esac
  '';

  patchPhase = ''
    { export MONO_PATH=${cecil}/lib/dotnet/Mono.Cecil
      mcs ${lib.escapeShellArg patcher} -out:patcher \
        -lib:${cecil}/lib/dotnet/Mono.Cecil \
        -r:Mono.Cecil -r:Mono.Cecil.Rocks
      mono patcher
      mv TowerFallPatched.exe TowerFall.exe
    }
  '';

  nativeBuildInputs = [ makeWrapper mono50 ];

  libdir = if stdenv.system == "x86_64-linux" then "lib64" else "lib";

  buildPhase = let
    dllmap = {
      SDL2 = "${SDL2}/lib/libSDL2.so";
      SDL2_image = "${SDL2_image}/lib/libSDL2_image.so";
      soft_oal = "${openal}/lib/libopenal.so";
      libvorbisfile-3 = "${libvorbis}/lib/libvorbisfile.so";
      MojoShader = "$out/libexec/towerfall-ascension/libmojoshader.so";
    };
  in lib.concatStrings (lib.mapAttrsToList (dll: target: ''
    sed -i -e '/<dllmap.*dll="${dll}\.dll".*os="linux"/ {
      s!target="[^"]*"!target="'"${target}"'"!
    }' FNA.dll.config
  '') dllmap);

  dummyXdgOpen = writeScriptBin "xdg-open" ''
    #!${stdenv.shell} -e
    if [ "''${1##*.}" = txt ]; then
      exec ${coreutils}/bin/head -v -n 20 "$1"
    else
      echo "Unable to open file $1" >&2
      exit 1
    fi
  '';

  installPhase = ''
    mkdir -p "$out/bin" \
             "$out/share/towerfall-ascension" \
             "$out/libexec/towerfall-ascension"
    cp -rvt "$out/share/towerfall-ascension" Content
    cp -rv mono/config "$out/libexec/towerfall-ascension/TowerFall.exe.config"
    cp -rvt "$out/libexec/towerfall-ascension" TowerFall.exe FNA.dll* \
      "$libdir/libmojoshader.so"
    ln -s "$out/share/towerfall-ascension/Content" \
          "$out/libexec/towerfall-ascension/Content"

    if [ -e "TowerFall Dark World Expansion" ]; then
      cp -rvt "$out/share/towerfall-ascension" \
        "TowerFall Dark World Expansion/DarkWorldContent"
    fi

    makeWrapper ${lib.escapeShellArg mono50}/bin/mono \
      "$out/bin/towerfall-ascension" \
      --set SDL_OPENGL_LIBRARY ${lib.escapeShellArg "${libGL}/lib/libGL.so"} \
      --set PATH "$dummyXdgOpen/bin" \
      --add-flags "$out/libexec/towerfall-ascension/TowerFall.exe" \
      --run "cd '$out/share/towerfall-ascension'"
  '';

  sandbox.paths.required = [ "$XDG_DATA_HOME/TowerFall" ];
}

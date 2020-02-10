{ buildUnity, fetchItch, mono, monogamePatcher, strace }:

buildUnity rec {
  name = "invisigun-heroes";
  fullName = "Invisigun";
  saveDir = "Sombr Studio/Invisigun Reloaded";
  version = "1.7.16";

  src = fetchItch {
    name = "${name}-${version}.zip";
    gameId = 25561;
    uploadId = 208583;
    version = "v${version}";
    sha256 = "1flwc5wvw84s53my8v8n402iz6izjs4d4ppffajdv9cg1vs3nbpl";
  };

  nativeBuildInputs = [ mono monogamePatcher ];

  buildPhase = ''
    cat > nix-support.cs <<EOF
    using UnityEngine;

    public class NixSupport {
      public static string GetFullPathStub(string _ignore) {
        return Application.persistentDataPath;
      }
    }
    EOF

    mcs nix-support.cs -target:library \
      -r:Invisigun_Data/Managed/UnityEngine.CoreModule \
      -out:Invisigun_Data/Managed/NixSupport.dll

    monogame-patcher replace-call \
      -i Invisigun_Data/Managed/Assembly-CSharp.dll \
      -a Invisigun_Data/Managed/NixSupport.dll \
      'System.String System.IO.Path::GetFullPath(System.String)' \
      'System.String NixSupport::GetFullPathStub(System.String)' \
      FileManagerAdapter_Desktop::ApplicationPath
  '';

  sandbox.paths.required = [ "$HOME/Invisigun Heroes" ];
}

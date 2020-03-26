{ buildUnity, fetchItch, mono, monogamePatcher, strace }:

buildUnity rec {
  name = "invisigun-heroes";
  fullName = "Invisigun";
  saveDir = "Sombr Studio/Invisigun Reloaded";
  version = "1.8.1";

  src = fetchItch {
    name = "${name}-${version}.zip";
    gameId = 25561;
    uploadId = 208583;
    version = "v${version}";
    sha256 = "11ik5zi9acpv9gbyra4h6zn223gw18bk56x8rn2vhb291ilmxl5n";
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

  sandbox.paths.required = [ "$XDG_DATA_HOME/Invisigun Reloaded" ];
}

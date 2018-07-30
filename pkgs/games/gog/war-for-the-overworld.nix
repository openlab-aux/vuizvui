{ buildUnity, fetchGog, mono, monogamePatcher }:

buildUnity {
  name = "war-for-the-overworld";
  fullName = "WFTOGame";
  saveDir = "Subterranean Games/War For The Overworld";
  version = "2.0.3f1";

  src = fetchGog {
    productId = 1964276929;
    downloadName = "en3installer0";
    sha256 = "07yj9clf3hmy7z67ck9sqf3gnrazx5rzifg91jas77z774vwdg8k";
  };

  nativeBuildInputs = [ mono monogamePatcher ];

  # The game tries to write stuff to its dataPath and it's even more
  # complicated than for most other games that try to write stuff into their
  # dataPath because the paths overlap with the assets.
  #
  # I've reported this upstream at:
  #
  # https://brightrockgames.userecho.com/communities/1/topics/4720-xdg
  #
  # So let's patch a few stuff so that at least starting the game and
  # loading/saving games will work.
  buildPhase = ''
    cat > nix-support.cs <<EOF
    using UnityEngine;
    using System.IO;

    public class NixSupport {
      public static string GetDataDir() {
        var path = Path.Combine(Application.persistentDataPath, "GameData");
        if (!Directory.Exists(path))
          Directory.CreateDirectory(path);
        return path;
      }

      public static string GetDataDir(string subpath) {
        var path = Path.Combine(NixSupport.GetDataDir(), subpath);
        if (!Directory.Exists(path))
          Directory.CreateDirectory(path);
        return path;
      }

      public static string GetFullPathMkParent(string path) {
        var fullpath = Path.GetFullPath(path);
        var dirname = Path.GetDirectoryName(fullpath);
        if (!Directory.Exists(dirname))
          Directory.CreateDirectory(dirname);
        return fullpath;
      }
    }
    EOF

    mcs nix-support.cs -target:library -r:WFTOGame_Data/Managed/UnityEngine \
      -out:WFTOGame_Data/Managed/NixSupport.dll

    monogame-patcher replace-call \
      -i WFTOGame_Data/Managed/Assembly-CSharp.dll \
      'System.String UnityEngine.Application::get_dataPath()' \
      'System.String UnityEngine.Application::get_persistentDataPath()' \
      IniParser SaveMeta::UpdateMetaDataAndMinimap SaveMeta::GetMinimapPath \
      SaveMeta::GetMinimapPath SaveMeta::GetMinimapAbsolutePath

    monogame-patcher replace-call \
      -i WFTOGame_Data/Managed/Assembly-CSharp.dll \
      -a WFTOGame_Data/Managed/NixSupport.dll \
      'System.String GameDataFolder::Get(System.String)' \
      'System.String NixSupport::GetDataDir(System.String)' \
      Serializer::MapSave DRMFree::FullPath DRMFree::OnEnable \
      SaveStatAchievementLAN::FullPath SaveStatAchievementLAN::GetFolders

    monogame-patcher replace-call \
      -i WFTOGame_Data/Managed/Assembly-CSharp.dll \
      -a WFTOGame_Data/Managed/NixSupport.dll \
      'System.String System.IO.Path::GetFullPath(System.String)' \
      'System.String NixSupport::GetFullPathMkParent(System.String)' \
      SaveMeta::GetMinimapAbsolutePath

    monogame-patcher fix-filestreams \
      -i WFTOGame_Data/Managed/Assembly-CSharp-firstpass.dll \
      UnityGTResourceHandler
  '';
}

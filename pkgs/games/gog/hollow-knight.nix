{ buildUnity, fetchGog, monogamePatcher }:

buildUnity {
  name = "hollow-knight";
  fullName = "hollow_knight";
  saveDir = "Team Cherry/Hollow Knight";
  version = "1.4.3.2";

  src = fetchGog {
    productId = 1308320804;
    downloadName = "en3installer0";
    sha256 = "19g0b6mzjahvj1y3mk25li61wardgk4fnl5cn9v24s9lhq8i8d28";
  };

  nativeBuildInputs = [ monogamePatcher ];

  buildPhase = ''
    monogame-patcher replace-call \
      -i hollow_knight_Data/Managed/Assembly-CSharp.dll \
      'System.String UnityEngine.Application::get_dataPath()' \
      'System.String UnityEngine.Application::get_persistentDataPath()' \
      ConfigManager::Awake
  '';
}

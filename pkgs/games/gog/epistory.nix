{ buildUnity, fetchGog, fixFmodHook }:

buildUnity rec {
  name = "epistory";
  fullName = "Epistory";
  saveDir = "Fishing Cactus/Epistory";
  version = "1.4-gog0";

  src = fetchGog {
    productId = 1986504189;
    sha256 = "05v9i4d7h2id5w6mfpnz3ig62v5dqibl74vahx3gqw9ya4jpgwv8";
  };

  buildInputs = [ fixFmodHook ];

  meta = {
    homepage = [
      https://www.gog.com/game/epistory_typing_chronicles
      http://epistorygame.com
    ];
    downloadPage = https://embed.gog.com/account/gameDetails/1986504189.json;
    description = "a beautiful atmospheric 3D action/adventure typing game";
    longDescription = ''
      Epistory - Typing Chronicles is a beautiful atmospheric 3D
      action/adventure typing game that tells the story of a writer lacking
      inspiration who asks her muse to help write her latest book.

      It features a visually stunning papercraft art style and blends light RPG
      elements with exploration and unique combat mechanics solely using the
      keyboard.
    '';
  };

}

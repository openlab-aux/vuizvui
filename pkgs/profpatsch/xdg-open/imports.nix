{ pkgs }:
{ Prelude =
      { repo =
          pkgs.fetchFromGitHub
            { owner = "dhall-lang";
              repo = "dhall-lang";
              rev = "bbf87ad50626061f20f70f37dae97d3175b92dcf";
              sha256 = "09ldjssgwywm6mm7akry1axhajrbxhia3288rrif5bd3ds8phxac";
            };
        mainFile = "Prelude/package.dhall";
      };
}

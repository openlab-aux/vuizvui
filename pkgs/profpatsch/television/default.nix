{
  lib,
  rustPlatform,
  fetchFromGitHub,
  testers,
  television,
  nix-update-script,
}:
rustPlatform.buildRustPackage (finalAttrs: {
  pname = "television";
  version = "0.13.2";

  src = fetchFromGitHub {
    owner = "alexpasmantier";
    repo = "television";
    tag = finalAttrs.version;
    hash = "sha256-Ur6UTd3XsI2ZyVboQA9r3WDkl7hd1wQ0NCgTlYFF/C0=";
  };

  useFetchCargoVendor = true;
  cargoHash = "sha256-LfaYRrJ4ZXoNVDsI650t+A7mWB9+2+znATp+mqDwTiE=";

  passthru = {
    tests.version = testers.testVersion {
      package = television;
      command = "XDG_DATA_HOME=$TMPDIR tv --version";
    };
    updateScript = nix-update-script { };
  };

  meta = {
    description = "Blazingly fast general purpose fuzzy finder TUI";
    longDescription = ''
      Television is a fast and versatile fuzzy finder TUI.
      It lets you quickly search through any kind of data source (files, git
      repositories, environment variables, docker images, you name it) using a
      fuzzy matching algorithm and is designed to be easily extensible.
    '';
    homepage = "https://github.com/alexpasmantier/television";
    changelog = "https://github.com/alexpasmantier/television/releases/tag/${finalAttrs.version}";
    license = lib.licenses.mit;
    mainProgram = "tv";
    maintainers = with lib.maintainers; [
      louis-thevenet
      getchoo
    ];
  };
})

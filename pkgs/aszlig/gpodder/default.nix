{ gpodder, fetchFromGitHub, python311Packages, buildSandbox }:

buildSandbox (gpodder.overridePythonAttrs (drv: {
  version = "git-2023-07-24";

  src = fetchFromGitHub {
    owner = "gpodder";
    repo = "gpodder";
    rev = "b5f95d9f20508a6cb2b7a1109c8f6bd1c00cfbc1";
    hash = "sha256-wyx8eIyz/cdQHKlC7hV7nNetzPKV0LMD/el5bt/akZ4";
  };

  patches = [ ./disable-autoupdate.patch ];

  propagatedBuildInputs = with python311Packages; [
    dbus-python
    mygpoclient
    pygobject3
    eyeD3
    podcastparser
    requests
    html5lib
    yt-dlp
  ];

  checkInputs = with python311Packages; [
    pytest pytest-httpserver minimock
  ];

  installCheckPhase = ''
    LC_ALL=C PYTHONPATH=./src:$PYTHONPATH pytest --doctest-modules \
    tests src/gpodder/util.py src/gpodder/jsonconfig.py
  '';
})) {
  paths.required = [ "$HOME/gPodder" ];
  fullNixStore = true;
}

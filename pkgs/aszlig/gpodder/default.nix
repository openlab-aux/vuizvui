{ gpodder, fetchFromGitHub, python3Packages, buildSandbox }:

buildSandbox (gpodder.overridePythonAttrs (drv: {
  version = "git-2023-04-29";

  src = fetchFromGitHub {
    owner = "gpodder";
    repo = "gpodder";
    rev = "0267b448eb97e3db3ebfad1bcf8ae348a44ff856";
    hash = "sha256-LVlwmNI1EZOdWdIvc4HuN2zwCn+1ouS/upFAP0sge08";
  };

  patches = [ ./disable-autoupdate.patch ];

  propagatedBuildInputs = with python3Packages; [
    dbus-python
    mygpoclient
    pygobject3
    eyeD3
    podcastparser
    requests
    html5lib
    yt-dlp
  ];

  checkInputs = with python3Packages; [
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

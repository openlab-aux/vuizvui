{ stdenv, curl, cacert, writeText, python3Packages

, apiKey
}:

{ name, gameId, uploadId, sha256 }:

let
  cafile = "${cacert}/etc/ssl/certs/ca-bundle.crt";

  getDownloadURL = writeText "getitch.py" ''
    import os, sys, json

    from urllib.parse import urljoin
    from urllib.request import urlopen

    API_KEY = os.getenv('apiKey')
    API_URL = 'https://itch.io/api/1/'
    API_BASE = urljoin(API_URL, API_KEY) + '/'

    NAME = os.getenv('name')
    GAME_ID = int(os.getenv('gameId'))
    UPLOAD_ID = int(os.getenv('uploadId'))

    def request(path):
      with urlopen(urljoin(API_BASE, path)) as u:
        return json.loads(u.read())

    for key in request('my-owned-keys')['owned_keys']:
      if key['game']['id'] == GAME_ID:
        url = 'download-key/{}/download/{}'.format(key['id'], UPLOAD_ID)
        sys.stdout.write(request(url)['url'] + '\n')
        raise SystemExit(0)

    sys.stderr.write('Unable to find download for game {}!'.format(NAME))
    raise SystemExit(1)
  '';

in stdenv.mkDerivation {
  inherit name apiKey gameId uploadId;
  outputHashAlgo = "sha256";
  outputHash = sha256;

  SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";

  nativeBuildInputs = [ python3Packages.python ];

  buildCommand = ''
    url="$(python "${getDownloadURL}")"
    header "downloading $name from $url"
    "${curl.bin or curl}/bin/curl" --fail --output "$out" "$url"
    stopNest
  '';
}

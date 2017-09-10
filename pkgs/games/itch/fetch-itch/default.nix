{ stdenv, curl, cacert, writeText, python3Packages

, apiKey
}:

{ name, gameId, uploadId, sha256, version ? null }:

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
    VERSION = os.getenv('version', None)

    def request(path):
      with urlopen(urljoin(API_BASE, path)) as u:
        return json.loads(u.read())

    def get_versions(key):
      url = 'upload/{}/builds'.format(UPLOAD_ID)
      return request(url + '?download_key_id=' + str(key))['builds']

    def print_download_url(key):
      if VERSION is not None:
        versions = get_versions(key)
        wanted = [ver for ver in versions if ver['user_version'] == VERSION]
        if len(wanted) == 1:
          url = 'upload/{}/download/builds/{}?download_key_id={}'.format(
            UPLOAD_ID, wanted[0]['id'], key
          )
          sys.stdout.write(request(url)['archive']['url'] + '\n')
          return
        else:
          msg = 'Unknown version {}, recent versions are:\n'.format(VERSION)
          sys.stderr.write(msg)
          for ver in versions[:20]:
            sys.stderr.write('Update date: {}, version: {}\n'.format(
              ver['updated_at'], ver['user_version']
            ))
          raise SystemExit(1)

      url = 'download-key/{}/download/{}'.format(key, UPLOAD_ID)
      sys.stdout.write(request(url)['url'] + '\n')

    for key in request('my-owned-keys')['owned_keys']:
      if key['game']['id'] == GAME_ID:
        print_download_url(key['id'])
        raise SystemExit(0)

    sys.stderr.write('Unable to find download for game {}!'.format(NAME))
    raise SystemExit(1)
  '';

in stdenv.mkDerivation {
  inherit name apiKey gameId uploadId version;
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

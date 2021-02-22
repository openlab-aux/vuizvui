{ writePython3
, getBins
, requests
, nix
, gnutar
, git
}:

let

  bins = (getBins nix [ "nix-hash" ])
    // (getBins gnutar [ "tar" ])
    // (getBins git [ "git" ])
    ;

in

writePython3 "vuizvui-update-programs-sqlite" {
  flakeIgnore = [
    # whitespaces around { }
    "E201" "E202"
    # fuck 4-space indentation
    "E121" "E111"
    # who cares about blank lines
    "W391" "E302" "E305"
    # URLs are long
    "E501"
  ];
  libraries = [ requests ];
} ''
  from pathlib import Path
  import re
  import requests
  import subprocess
  import sys
  from tempfile import TemporaryDirectory

  def latest_nixexprs_url():
    r = requests.head('https://channels.nixos.org/nixos-unstable/nixexprs.tar.xz')

    assert r.status_code == 301
    return r.headers['location']

  def nixos_version_for_url(url):
    match = re.match(r"https://releases\.nixos\.org/nixos/unstable/nixos-([0-9a-fpre.]+)/nixexprs\.tar\.xz", url)
    return match.group(1)

  def download(url: str, filename: Path) -> Path:
    with requests.get(url, stream=True) as r:
      r.raise_for_status()
      with open(filename, 'wb') as f:
        for c in r.iter_content(chunk_size=16384):
          f.write(c)

    return filename

  def main():
    if len(sys.argv) > 2:
        print(f'Usage: {sys.argv[0]} /path/to/release.nix', file=sys.stderr)
        raise SystemExit(64)

    url = latest_nixexprs_url()
    version = nixos_version_for_url(url)

    print(f'Updating programs.sqlite to {version}', file=sys.stderr)

    with TemporaryDirectory(prefix="vuizvui-update-programs-sqlite") as dir:
      nixexprs = download(url, dir / Path('nixexprs.tar.xz'))
      programs_sqlite = dir / Path('programs.sqlite')

      with open(programs_sqlite, 'wb') as f:
        subprocess.run([
            '${bins.tar}',
            '-xJOf',
            nixexprs,
            f'nixos-{version}/programs.sqlite'
          ], stdout=f, check=True)

      hash = subprocess.run([
          '${bins.nix-hash}',
          '--base32',
          '--type', 'sha256',
          '--flat',
          programs_sqlite
        ],
        check=True,
        capture_output=True).stdout.decode('utf-8').strip()

      print(f'New hash: {hash}', file=sys.stderr)

      if len(sys.argv) == 1:
        print('Doing nothing (dry run)', file=sys.stderr)
      elif len(sys.argv) == 2:
        release_nix = Path(sys.argv[1])

        with open(release_nix, 'r+') as f:
          text = f.read()
          # base32 alphabet as per nix-rust/src/util/base32.rs
          new_text = re.sub(r'programsSqliteSha256\s*=\s*"[0-9a-fg-np-sv-z]+"',
                            f'programsSqliteSha256 = "{hash}"',
                            text)
          new_text = re.sub(r'programsSqliteVersion\s*=\s*"[0-9a-fpre.]+"',
                            f'programsSqliteVersion = "{version}"',
                            new_text)

          if text == new_text:
            print('Already up to date')
          else:
            f.seek(0)
            f.write(new_text)

            print(f'Wrote to {release_nix}', file=sys.stderr)

        subprocess.run([
            '${bins.git}',
            'commit', '-m',
            f'release.nix: update programs.sqlite to {version}',
            '--',
            release_nix
          ],
          check=True)

  if __name__ == '__main__':
    main()
  ''

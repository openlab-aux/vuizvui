#!/usr/bin/env nix-shell
#!nix-shell -i bash -p bash coreutils jq
for addon in ff2mpv styl-us tridactyl-vim multi-account-containers; do
    url="https://addons.mozilla.org/api/v5/addons/addon/$addon/versions/"
    curl -s -H 'Accept: application/json' "$url" \
        | jq --arg name "$addon" '. + {$name}'
done | jq -s '
    map({key: .name, value: .results[0].file | {url, hash}}) | from_entries
' > "$(cd "$(dirname "$0")" && pwd)/addons.json"

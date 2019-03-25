set -euo pipefail

# nix-shell -p cdb --run 'bash -c \'source ~/tmp/gitcdb.sh; for r in $(findRepos ~/kot); do genIndex . "$r"; done\''

findRepos () {
    find "$1" -type d -name ".git"
    # TODO check each repo is actually git repo
}

genIndex () {
    local indexDir=$(realpath -- "$1")
    local path=$(realpath -- "$2")
    if [ ! -d "$indexDir" ]; then
        echo "index directory does not exist: $indexDir"
        exit 111
    fi
    # TODO: multimap failure
    #
    local filename="$indexDir/$(echo $path | sed -e 's|_|__|g' -e 's|/|_|g')"
    local pathLength=$(echo "$path" | wc --bytes | tr -d '\n')
    (pushd "$path" > /dev/null \
            && ( git log --all --format="format:%H" \
                     | sed -e "s/^\(.*\)$/+40,0:\1->/" \
               ; echo \
               ; echo "+8,${pathLength}:git-path->${path}" \
               ; echo; echo \
               ) \
            | cdbmake "$filename" "$filename.tmp" \
    )
}

query () {
    local indexDir=$(realpath -- "$1")
    local key="$2"

    local found=0
    local result=

    # TODO make this parallel (and switch away from bash)
    for f in "$indexDir"/*; do

        set +e
        # don't need result because empty string
        <"$f" cdbget "$key" >/dev/null
        local ret=$?
        set -e

        case $ret in
            0)
                # TODO: back
                found=1

                set +e
                # now find original path
                local origDotGit=$(<"$f" cdbget "git-path")
                local retGitPath=$?
                set -e

                case $retGitPath in
                    0)
                        :
                        ;;
                    100)
                        echo "shouldn’t happen; git-path was not in $f"
                        exit 127
                        ;;
                    111)
                        echo "db error in $f"
                        exit 111
                        ;;
                    *)
                        echo "shouldn’t happen; exitcode was $ret"
                        exit 127
                        ;;
                esac

                # return workspace file
                result=$(dirname "$origDotGit")
                break
                ;;
            100)
                # not found
                :
                ;;
            111)
                echo "db error in $f"
                exit 111
                ;;
            *)
                echo "shouldn’t happen; exitcode was $ret"
                exit 127
                ;;
        esac
    done

    if [ $found -eq 0 ]; then
        exit 100
    else
        echo "$result"
        exit 0
    fi
}

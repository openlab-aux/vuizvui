sandbox_params_include="$(mktemp --suffix=.c)"
trap "rm -f '$sandbox_params_include'" EXIT
sandbox_references=""

hasReference() {
    local ref
    for ref in $sandbox_references; do
        if [ "$1" = "$ref" ]; then return 0; fi
    done

    return 1
}

addReference() {
    local toAdd="$1"

    sandbox_references="$sandbox_references $toAdd"

    echo 'if (!bind_mount("'"$toAdd"'", true)) return false;' \
        >> "$sandbox_params_include"
}

gatherReferencesRecursive() {
    local path="$1"

    if hasReference "$path"; then return; fi
    addReference "$path"

    local valid_hash='[0-9a-df-np-sv-z]\{32\}'
    local valid_name='[A-Za-z0-9+_?=-][A-Za-z0-9+._?=-]*'
    local valid_path="$NIX_STORE/$valid_hash-$valid_name"

    local hashpaths="$(
        find "$path" -type f -exec grep -hao "$valid_path" {} +
        find "$path" -type l -exec readlink {} +
    )"

    local hashpath
    for hashpath in $hashpaths; do
        local realsp
        for realsp in "$NIX_STORE"/*; do
            if echo "$hashpath" | grep -q -m 1 "^${realsp//./\\.}"; then
                gatherReferencesRecursive "$realsp"
                break
            fi
        done
    done
}

gatherReferences() {
    [ -z "$sandbox_references" ] || return 0

    echo 'static bool setup_app_paths(void) {' > "$sandbox_params_include"

    for output in $outputs; do
        [ -e "${!output}" ] || continue
        gatherReferencesRecursive "${!output}"
    done

    if [ -n "$extraSandboxPaths" ]; then
        local oldIfs="$IFS"
        IFS=':!*!:'
        local extra
        for extra in $extraSandboxPaths; do
            local extraC="$(echo "$extra" | sed -e 's/"\\/\\&/g')"
            echo 'if (!extra_mount("'"$extraC"'")) return false;' \
                >> "$sandbox_params_include"
        done
        IFS="$oldIfs"
    fi

    echo 'return true; }' >> "$sandbox_params_include"
    cat "$sandbox_params_include"
}

wrapSandbox() {
    local progname="$1"
    local wrapped="$2"
    local output="$3"

    @gcc@/bin/gcc -g -std=gnu11 -Wall \
        -DWRAPPED_PATH=\""$wrapped"\" \
        -DWRAPPED_PROGNAME=\""$progname"\" \
        -DPARAMS_FILE=\""$sandbox_params_include"\" \
        -o "$output" @sandbox_main@
}

makeSandbox() {
    gatherReferences

    for output in $outputs; do
        [ -e "${!output}" ] || continue
        local bin
        for bin in "${!output}"/bin/*; do
            local binbase="$(basename "$bin")"
            local newdest="$(dirname "$bin")/.$binbase-wrapped"
            mv "$bin" "$newdest"
            wrapSandbox "$binbase" "$newdest" "$bin"
        done
    done
}

postFixupHooks+=(makeSandbox)

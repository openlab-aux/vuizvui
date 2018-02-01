declare -a autoPatchelfLibs

gatherLibraries() {
    autoPatchelfLibs+=("$1/lib")
}

addEnvHooks "$targetOffset" gatherLibraries

isExecutable() {
    [ "$(@file@ -b -N --mime-type "$1")" = application/x-executable ]
}

findElfs() {
    @find@ "$1" -type f -exec @shell@ -c '
        while [ -n "$1" ]; do
            mimeType="$(@file@ -b -N --mime-type "$1")"
            if [ "$mimeType" = application/x-executable \
              -o "$mimeType" = application/x-sharedlib ]; then
                echo "$1"
            fi
            shift
        done
    ' -- {} +
}

declare -a cachedDependencies

addToDepCache() {
    local existing
    for existing in "${cachedDependencies[@]}"; do
        if [ "$existing" = "$1" ]; then return; fi
    done
    cachedDependencies+=("$1")
}

declare -gi depCacheInitialised=0
declare -gi doneRecursiveSearch=0
declare -g foundDependency

getDepsFromSo() {
    @ldd@ "$1" 2> /dev/null | @sed@ -n -e 's/[^=]*=> *\(.\+\) \+([^)]*)$/\1/p'
}

populateCacheWithRecursiveDeps() {
    local so found foundso
    for so in "${cachedDependencies[@]}"; do
        for found in $(getDepsFromSo "$so"); do
            local libdir="${found%/*}"
            local base="${found##*/}"
            local soname="${base%.so*}"
            for foundso in "${found%/*}/$soname".so*; do
                addToDepCache "$foundso"
            done
        done
    done
}

getSoArch() {
    @objdump@ -f "$1" | @sed@ -ne 's/^architecture: *\([^,]\+\).*/\1/p'
}

findDependency() {
    local filename="$1"
    local arch="$2"
    local lib dep

    if [ $depCacheInitialised -eq 0 ]; then
        for lib in "${autoPatchelfLibs[@]}"; do
            for so in "$lib/"*.so*; do addToDepCache "$so"; done
        done
        depCacheInitialised=1
    fi

    for dep in "${cachedDependencies[@]}"; do
        if [ "$filename" = "${dep##*/}" ]; then
            if [ "$(getSoArch "$dep")" = "$arch" ]; then
                foundDependency="$dep"
                return 0
            fi
        fi
    done

    if [ $doneRecursiveSearch -eq 0 ]; then
        populateCacheWithRecursiveDeps
        doneRecursiveSearch=1
        findDependency "$filename" "$arch" || return 1
        return 0
    fi
    return 1
}

autoPatchelfFile() {
    local dep rpath="" toPatch="$1"

    local interpreter="$(< "$NIX_CC/nix-support/dynamic-linker")"
    if isExecutable "$toPatch"; then
        @patchelf@ --set-interpreter "$interpreter" "$toPatch"
        if [ -n "$runtimeDependencies" ]; then
            for dep in $runtimeDependencies; do
                rpath="$rpath${rpath:+:}$dep/lib"
            done
        fi
    fi

    echo "searching for dependencies of $toPatch:" >&2

    @patchelf@ --remove-rpath "$toPatch"

    local missing="$(
        @ldd@ "$toPatch" 2> /dev/null | \
            @sed@ -n -e 's/^[\t ]*\([^ ]\+\) => not found.*/\1/p'
    )"

    for dep in $missing; do
        echo -n "  $dep -> " >&2
        if findDependency "$dep" "$(getSoArch "$toPatch")"; then
            rpath="$rpath${rpath:+:}${foundDependency%/*}"
            echo "found: $foundDependency" >&2
        else
            echo "not found!" >&2
        fi
    done

    if [ -n "$rpath" ]; then
        echo "setting RPATH to: $rpath" >&2
        @patchelf@ --set-rpath "$rpath" "$toPatch"
    fi
}

autoPatchelf() {
    echo "automatically fixing dependencies for ELF files" >&2

    cachedDependencies+=(
        $(find "$out" \! -type d \( -name '*.so' -o -name '*.so.*' \))
    )
    local elffile
    findElfs "$prefix" | while read -r elffile; do
        autoPatchelfFile "$elffile"
    done
}

postInstallHooks+=(autoPatchelf)

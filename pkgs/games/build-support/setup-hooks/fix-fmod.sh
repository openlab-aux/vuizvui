# FMOD tries to run /bin/sh -c 'pulseaudio --check > /dev/null 2>&1', so we
# need to prevent this by replacing the system() call with a successful return
# value (0). If someone doesn't have or want to have PulseAudio, FMOD still
# falls back to ALSA if it can't load libpulse-simple.so.
_doPatchFmod() {
    set -e
    set -o pipefail

    # Let's make sure it's really the affected FMOD version:
    objdump -T "$1" 2> /dev/null | grep -q "\\<FMOD_System_Init\\>"
    grep -qF "pulseaudio --check" "$1"

    case "$(objdump -f "$1" | sed -n -e 's/^architecture: *//p')" in
        i386:x86-64,*)
            local addrs="$(objdump -d "$1" | sed -n -e '
                /callq.*system@plt/s/^ *\([^:]\+\).*/\1/p
            ')"

            local addr
            for addr in $addrs; do
                # This is quite easy, just replace the system() call with XOR
                # EAX so we get a return value of 0 and pad the rest with NOP.
                local offset=$(("0x$addr"))
                ( printf '\x31\xc0'     # XOR the EAX register
                  printf '\x90\x90\x90' # Fill with NOPs
                ) | dd of="$1" obs=1 seek=$offset conv=notrunc status=none
            done
            ;;
        i386,*)
            local relocSystem="$(readelf -r "$1" | sed -n -e '
                /system@/s/^0*\([^ ]\+\).*/\1/p
            ')"
            local addr="$(objdump -d "$1" | sed -n -e '
                /call *'"$relocSystem"' /s/^ *\([^:]\+\).*/\1/p
            ')"

            # For the 32 bit library it's not so easy as the 4 bytes coming
            # after the CALL opcode will be replaced by the dynamic linker, so
            # we just XOR the EAX register with the relocation address and
            # replace the TEST opcode afterwards.
            local offset=$(("0x$addr"))
            ( printf '\x35\xfc\xff\xff\xff' # XOR EAX with the relocation addr
              printf '\x39\xc0'             # CMP EAX, EAX
            ) | dd of="$1" obs=1 seek=$offset conv=notrunc status=none
            ;;
        *) return ;;
    esac

    # Only needed if the library is used with dlopen().
    if [ -n "$runtimeDependencies" ]; then
        local dep rpath="$(patchelf --print-rpath "$1")"
        for dep in $runtimeDependencies; do
          rpath="$rpath''${rpath:+:}$dep/lib"
        done
        patchelf --set-rpath "$rpath" "$1"
    fi

    echo "$1: removed call to system()" >&2
}

patchFmod() {
    export -f _doPatchFmod
    echo "patching out system() calls in FMOD" >&2
    find "$prefix" \( -iname '*fmod*.so' -o -iname '*fmod*.so.*' \) \
        -exec "$SHELL" -c '_doPatchFmod "$1"' -- {} \;
}

# Needs to come after the setup hook of patchelf.
postFixupHooks+=(
    'for output in $outputs; do prefix="${!output}" patchFmod; done'
)

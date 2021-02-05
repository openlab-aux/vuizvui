use std::ffi::CStr;
extern crate libc;

fn to_c_argv<S: AsRef<CStr>>(s: &[S]) -> (&[S], Vec<*const libc::c_char>) {
    let mut c_ptr_args = s.iter()
        .map(|s| s.as_ref().as_ptr())
        .collect::<Vec<*const libc::c_char>>();
    c_ptr_args.push(std::ptr::null());
    (s, c_ptr_args)
}

/// Exec into argv, or exit 0 if it’s empty.
/// Will throw 127 if the executable is not found (ENOENT)
/// and 126 for any other exec error.
pub fn xmexec0<'a, S: AsRef<CStr>>(argv: &'a [S]) {
    let (c_strings, c_argv) = to_c_argv(argv);

    unsafe {
        let env = C::environ;
        C::xmexec0_af(
            c_argv[0] as *const libc::c_char,
            c_argv.as_ptr() as *const *const libc::c_char,
            env,
            C::env_len(env)
        )
    }
}

mod C {
    #[link(name = "skarnet")]
    extern "C" {
        pub fn xmexec0_af(
            file: *const libc::c_char,
            argv: *const *const libc::c_char,
            envp: *const *const libc::c_char,
            envlen: libc::size_t
        );
        pub static environ: *const *const libc::c_char;
        pub fn env_len(
            e: *const *const libc::c_char
        ) -> libc::size_t;
    }
}

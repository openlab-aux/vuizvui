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
pub fn xpathexec0<'a, S: AsRef<CStr>>(argv: &'a [S]) {
    let (c_strings, c_argv) = to_c_argv(argv);

    unsafe {
        C::xpathexec0(
            c_argv.as_ptr() as *const *const libc::c_char
        )
    }
}

mod C {
    #[link(name = "skarnet")]
    extern "C" {
        pub fn xpathexec0(
            argv: *const *const libc::c_char
        );
    }
}

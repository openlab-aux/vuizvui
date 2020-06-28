extern crate errno;
extern crate libc;

use std::ffi::{CStr, CString};

#[repr(C)]
pub struct Stralloc_C {
    s: *mut libc::c_char,
    len: libc::size_t,
    a: libc::size_t
}

fn stralloc_zero() -> Stralloc_C {
    Stralloc_C {
        s: std::ptr::null_mut(),
        len: 0,
        a: 0
    }
}

#[link(name = "skarnet")]
extern "C" {
    fn stralloc_free(sa: *mut Stralloc_C);
    fn stralloc_ready_tuned(
        sa: *mut Stralloc_C,
        n: libc::size_t,
        base: libc::size_t,
        a: libc::size_t,
        b: libc::size_t,
    ) -> libc::c_int;

    fn stralloc_copyb(
        sa: *mut Stralloc_C,
        s: *const libc::c_char,
        len: libc::size_t
    ) -> libc::c_int;
}

fn stralloc_ready(sa: *mut Stralloc_C, n: libc::size_t) {
    unsafe {
        if (stralloc_ready_tuned(sa, n, 8, 1, 8) == 0) {
            panic!("{}", errno::errno());
        }
    }
}

struct Stralloc(Stralloc_C);

impl Stralloc {
    fn new() -> Self {
        let mut sa = stralloc_zero();
        unsafe {
            stralloc_ready(&mut sa, 0);
        }
        Stralloc(sa)
    }
}

impl AsMut<Stralloc_C> for Stralloc {
    fn as_mut(&mut self) -> &mut Stralloc_C {
        match self {
            Stralloc(s) => s
        }
    }
}

impl AsRef<Stralloc_C> for Stralloc {
    fn as_ref(&self) -> &Stralloc_C {
        match self {
            Stralloc(s) => s
        }
    }
}

impl<'a> From<&mut [u8]> for Stralloc {
    fn from(s: &mut [u8]) -> Self {
        let mut sa = stralloc_zero();
        let ptr = s.as_mut_ptr() as *mut libc::c_char;
        unsafe {
            if stralloc_copyb(&mut sa, ptr, s.len()) == 0 {
                panic!("{}", errno::errno());
            }
        }
        Stralloc(sa)
    }
}

// TODO not sure if stralloc will always be a contiguous block?
// that’s the precondition for from_raw_parts
impl AsRef<[u8]> for Stralloc_C {
    fn as_ref(&self) -> &[u8] {
        let ptr = self.s as *const u8;
        unsafe {
            std::slice::from_raw_parts(ptr, self.len)
        }
    }
}

impl Drop for Stralloc {
    fn drop(&mut self) {
        match self {
            Stralloc(inner) => {
                unsafe {
                    stralloc_free(inner);
                }
            }
        }
    }
}


#[repr(C)]
#[derive(Debug)]
pub struct Elsubst_C
{
    var: libc::size_t,
    value: libc::size_t,
    // values are \0-separated strings,
    // and n is the amount of elements in one such string
    n: libc::c_uint
}

#[link(name = "execline")]
extern "C" {
    fn el_substitute(
        dst: *mut Stralloc_C,
        src: *const libc::c_char,
        len: libc::size_t,
        // length is given by nsubst
        vars: *const libc::c_char,
        // length is given by nsubst
        values: *const libc::c_char,

        substs: *const Elsubst_C,
        nsubst: libc::size_t
    ) -> libc::c_int;
}

pub struct Subst<'a> {
    pub var: &'a CStr,
    pub value: &'a CStr,
}

fn simple_substitute<'a, 'b>(subst: &[Subst<'a>], src: &CStr) -> CString {
    let len = src.to_bytes_with_nul().len() as libc::size_t;
    let src = src.as_ptr() as *const libc::c_char;
    let mut vars : Vec<u8> = Vec::new();
    let mut values : Vec<u8> = Vec::new();
    let mut substs : Vec<Elsubst_C> = Vec::new();
    let mut var_i = 0;
    let mut value_i = 0;
    for s in subst {
        let var = s.var.to_bytes_with_nul();
        let value = s.value.to_bytes_with_nul();
        vars.extend_from_slice(var);
        values.extend_from_slice(value);
        substs.push(Elsubst_C {
            // these index into the vars/values arrays given to el_substitute
            var: var_i,
            value: value_i,
            // we don’t deal with split values here
            n: 1
        });
        var_i += var.len();
        value_i += value.len();
    }
    let nsubst = subst.len();
    let mut dst = stralloc_zero();
    unsafe {
        if el_substitute(
            &mut dst,
            src,
            len,
            vars.as_ptr() as *const libc::c_char,
            values.as_ptr() as *const libc::c_char,
            substs.as_ptr() as *const Elsubst_C,
            nsubst
        ) == -1 {
            panic!("{}", errno::errno());
        }
        // el_substitute returns a \0-delim C string
        CStr::from_bytes_with_nul_unchecked(dst.as_ref()).to_owned()
    }
}

pub fn simple_substitute_argv<'a, 'b, S: AsRef<CStr>>(subst: &[Subst<'a>], argv: &'b [S]) -> Vec<CString> {
    argv.into_iter()
        .map(|arg| simple_substitute(subst, arg.as_ref()))
        .collect::<Vec<_>>()
}

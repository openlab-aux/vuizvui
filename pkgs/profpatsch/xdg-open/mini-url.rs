extern crate errno;
extern crate libc;

use std::ffi::{OsString};
use std::os::unix::ffi::OsStringExt;
use std::ffi::{CStr, CString};

// TODO: move the execline stuff into a different library

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

    fn stralloc_catb(
        sa: *mut Stralloc_C,
        s: *const libc::c_char,
        len: libc::size_t
    ) -> libc::c_int;

    fn stralloc_copyb(
        sa: *mut Stralloc_C,
        s: *const libc::c_char,
        len: libc::size_t
    ) -> libc::c_int;

    fn xpathexec0(
        argv: *const *const libc::c_char
    );
    fn xpathexec_run(
        file: *const libc::c_char,
        argv: *const *const libc::c_char,
        envp: *const *const libc::c_char
    );
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

struct Subst<'a> {
    var: &'a CStr,
    value: &'a CStr,
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

#[derive(Clone, Debug, PartialEq, Eq,)]
struct Parsed<'a> {
    protocol: Protocol,
    domain: &'a str,
    port: &'a str,
    path: &'a str,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Protocol {
    Http,
    Https,
}

impl Protocol {
    fn string(&self) -> &str {
        match self {
            Protocol::Http => "http",
            Protocol::Https => "https"
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Error {
    NotHttp,
    ColonButNoPort
}

fn mini_parse_http(url: &str) -> Result<Parsed, Error> {
    let (protocol, off) = {
        if url.starts_with("https://") {
            (Protocol::Https, 8)
        } else if url.starts_with("http://") {
            (Protocol::Http, 7)
        } else {
            return Err(Error::NotHttp)
        }
    };

    let (_, path) = url.split_at(off);

    let (domain_port, path) = match path.find('/') {
        Some(boundary) => path.split_at(boundary),
        None => (path, "/")
    };

    let (domain, port) = match domain_port.find(':') {
        Some(boundary) => match domain_port.split_at(boundary) {
            (d, ":") => return Err(Error::ColonButNoPort),
            (d, p) => unsafe { (d, p.get_unchecked(1..)) }
        },
        None => match protocol {
            Protocol::Http => (domain_port, "80"),
            Protocol::Https => (domain_port, "443"),
        }
    };

    Ok(Parsed {
        protocol,
        domain,
        port,
        path
    })
}


fn write_netstring<W: std::io::Write>(mut w: W, s: &str) -> std::io::Result<()> {
    write!(w, "{}:{},", s.len(), s)
}

fn to_cstrings(s: Vec<OsString>) -> Vec<CString> {
    s.into_iter()
        .map(|s: OsString| CString::new(s.into_vec()).unwrap())
        .collect::<Vec<CString>>()
}

fn to_c_argv(s: Vec<CString>) -> (Vec<CString>, Vec<*const libc::c_char>) {
    let mut c_ptr_args = s.iter()
        .map(|s| s.as_ptr())
        .collect::<Vec<*const libc::c_char>>();
    c_ptr_args.push(std::ptr::null());
    (s, c_ptr_args)
}


fn main() -> std::io::Result<()> {
    let args = std::env::args().collect::<Vec<_>>();
    let prog_index = 2;
    if args.len() < prog_index {
        eprintln!("1st arg is http/https url");
        std::process::exit(100);
    }

    let Parsed { domain, port, protocol, path } = mini_parse_http(&args[1]).unwrap_or_else(
        |err| {
            eprintln!("could not parse url: {:?}", err);
            std::process::exit(100);
        }
    );

    // println!("{:#?}", mini_parse_http(&args[1]).unwrap());

    let subst = |s: &CString| simple_substitute(&vec![
        Subst{
            var: &CString::new(b"host".to_vec()).unwrap(),
            value: &CString::new(domain).unwrap()
        },
        Subst{
            var: &CString::new(b"port".to_vec()).unwrap(),
            value: &CString::new(port).unwrap(),
        },
        Subst{
            var: &CString::new(b"protocol".to_vec()).unwrap(),
            value: &CString::new(protocol.string()).unwrap()
        },
        Subst{
            var: &CString::new(b"path".to_vec()).unwrap(),
            value: &CString::new(path).unwrap()
        },

    ], s);


    // TODO: replace this bit by xpathexec0 from el_exec.rs

    let cstring_argv = to_cstrings(std::env::args_os().collect::<Vec<_>>());
    let (c_strings, c_argv) = to_c_argv(cstring_argv.iter().map(subst).collect::<Vec<_>>());

    unsafe {
        xpathexec0(
            c_argv[prog_index..].as_ptr() as *const *const libc::c_char
        )
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_mini_parse() {
        assert_eq!(mini_parse_http("https://abc.bar:8080/blabla"),
                   Ok(Parsed {
                       protocol: Protocol::Https,
                       domain: "abc.bar",
                       port: "8080",
                       path: "/blabla"
                   }));
        assert_eq!(mini_parse_http("http://abc.bar:8080/blabla"),
                   Ok(Parsed {
                       protocol: Protocol::Http,
                       domain: "abc.bar",
                       port: "8080",
                       path: "/blabla"
                   }));
        assert_eq!(mini_parse_http("http://abc.bar/blabla"),
                   Ok(Parsed {
                       protocol: Protocol::Http,
                       domain: "abc.bar",
                       port: "80",
                       path: "/blabla"
                   }));
        assert_eq!(mini_parse_http("https://f"),
                   Ok(Parsed {
                       protocol: Protocol::Https,
                       domain: "f",
                       port: "443",
                       path: "/"
                   }));
        assert_eq!(mini_parse_http("foo.bar"),
                   Err(Error::NotHttp));
        assert_eq!(mini_parse_http("mirror://abc.bar:8080/blabla"),
                   Err(Error::NotHttp));
        assert_eq!(mini_parse_http("http://abc.bar:"),
                   Err(Error::ColonButNoPort));
    }


}

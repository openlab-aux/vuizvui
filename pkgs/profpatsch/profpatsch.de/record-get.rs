extern crate netencode;
extern crate el_semicolon;
extern crate el_exec;

use std::io::Read;
use std::ffi::{CString, OsStr};
use std::os::unix::ffi::{OsStringExt, OsStrExt};
use el_semicolon::{el_semicolon, Arg};
use netencode::{U};
use netencode::parse::{u_u};

fn main() {
    let args = std::env::args_os().by_ref()
        .map(|s| s.into_vec())
        .skip(1)
        .collect::<Vec<_>>();
    let args_ref : Vec<&[u8]> = args.iter().map(|v| v.as_slice()).collect();
    let (arg, prog) = el_semicolon::el_semicolon(&args_ref).unwrap();
    assert!(prog.len() > 0, "record get neets a block of vars and prog, no prog provided");
    match arg {
        Arg::EndOfArgv | Arg::Arg(_) => {
            panic!("first argument must be a block of vars");
        },
        Arg::Block(vars) => {
            let mut stdin = vec![];
            std::io::stdin().read_to_end(&mut stdin);
            match u_u(&stdin) {
                Ok((_, U::Record(m))) => {
                    for (key, val) in m.into_iter() {
                        // only set if it appears in the block of values.
                        // If the block is empty, don’t filter.
                        if vars.is_empty() || vars.contains(&key.as_bytes()) {
                            match *val {
                                U::Binary(b) => std::env::set_var(key, OsStr::from_bytes(b)),
                                _ => panic!("the value of {:?} was not a binary value!", key)
                            }
                        }
                    }
                    let mut p : Vec<CString> = vec![];
                    for arg in prog {
                        p.push(CString::new(*arg).unwrap());
                    }
                    el_exec::xpathexec0(&p);
                },
                Ok(_) => {
                    eprintln!("not a record!");
                    std::process::exit(100);
                }
                Err(e) => {
                    eprintln!("could not parse netencode: {:?}", e);
                    std::process::exit(100);
                },
            }
        }
    }
}

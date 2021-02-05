extern crate netencode;
extern crate el_semicolon;

use std::io::Read;
use std::ffi::{CString, OsStr};
use std::os::unix::ffi::{OsStringExt, OsStrExt};
use std::os::unix::process::{CommandExt};
use el_semicolon::{el_semicolon, Arg};
use netencode::{U, encode};
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
                            // TODO this is a super unprincipled special casing of some values
                            // We should have a way of destructuring stuff
                            match *val {
                                U::Binary(b) => std::env::set_var(key, OsStr::from_bytes(b)),
                                U::Text(t) => std::env::set_var(key, OsStr::from_bytes(t)),
                                u => {
                                    let mut c = std::io::Cursor::new(vec![]);
                                    encode(&mut c, u);
                                    std::env::set_var(key, OsStr::from_bytes(&c.into_inner()))
                                }
                            }
                        }
                    }
                    let env: Vec<(&[u8], &[u8])> = vec![];
                    exec_into_args("record-get", prog, env)
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

pub fn exec_into_args<'a, 'b, Args, Arg, Env, Key, Val>(current_prog_name: &str, args: Args, env_additions: Env) -> !
where
    Args: IntoIterator<Item = Arg>,
    Arg: AsRef<[u8]>,
    Env: IntoIterator<Item = (Key, Val)>,
    Key: AsRef<[u8]>,
    Val: AsRef<[u8]>,
{
    // TODO: is this possible without collecting into a Vec first, just leaving it an IntoIterator?
    let args = args.into_iter().collect::<Vec<Arg>>();
    let mut args = args.iter().map(|v| OsStr::from_bytes(v.as_ref()));
    let prog = args.nth(0).expect(&format!("{}: first argument must be an executable", current_prog_name));
    // TODO: same here
    let env = env_additions.into_iter().collect::<Vec<(Key, Val)>>();
    let env = env.iter().map(|(k,v)| (OsStr::from_bytes(k.as_ref()), OsStr::from_bytes(v.as_ref())));
    let err = std::process::Command::new(prog).args(args).envs(env).exec();
    die_missing_executable(current_prog_name, format!("exec failed: {:?}, while trying to execing into {:?}", err, prog));
}

/// Exit 127 to signify a missing executable.
pub fn die_missing_executable<S>(current_prog_name: &str, msg: S) -> !
where S: AsRef<str>
{
    die_with(127, current_prog_name, msg)
}

fn die_with<S>(status: i32, current_prog_name: &str, msg: S) -> !
where S: AsRef<str>
{
    eprintln!("{}: {}", current_prog_name, msg.as_ref());
    std::process::exit(status)
}

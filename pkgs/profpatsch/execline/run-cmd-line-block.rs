use std::ffi::{OsString, OsStr};
use std::process::{Command};
use std::os::unix::process::CommandExt;

fn main() -> std::io::Result<()> {
  let args = std::env::args_os();
  let mut cmd : Vec<OsString> = vec![];
  let mut depth = 0;
  for arg in args.skip(1) {
    if arg == OsString::from("[") {
        depth = depth + 1;
    } else if arg == OsString::from("]") {
        depth = depth - 1;
        cmd.push(prepend_block_depth(depth, &OsString::from("")));
    } else {
        cmd.push(prepend_block_depth(depth, &arg));
    }
  }

  Err(match cmd.len() {
      0 => std::process::exit(0),
      1 => Command::new(&cmd[0]).exec(),
      _ => Command::new(&cmd[0])
             .args(&cmd[1..])
             .exec()
  })
}

fn prepend_block_depth(depth: usize, arg: &OsStr) -> OsString {
    let mut s = OsString::from(" ".repeat(depth));
    s.push(arg);
    s
}

use std::collections::HashMap;
use std::env::args_os;
use std::ffi::OsString;
use std::fs::File;
use std::io::{BufReader, BufRead, Read};
use std::os::unix::ffi::OsStrExt;
use std::process::exit;

#[derive(Eq, PartialEq, Hash)]
struct OutPath(Vec<u8>);
#[derive(Eq, PartialEq, Hash, Clone)]
struct Attr(Vec<u8>);

type OutPathMap = HashMap<OutPath, Attr>;

#[derive(Eq, PartialEq)]
enum Mode {
  PrintAttrs,
  PrintOutPaths,
}

fn parse_nix_env_line(
  line: &[u8]
) -> Option<(Vec<OutPath>, Attr)> {
  // fields are separated by > 1 space
  let segments =
    line.split(|c| *c == 0x20)
        .filter(|s| s.len() > 0)
        .collect::<Vec<&[u8]>>();
  let len = segments.len();

  if len >= 2 {
    // handle multiple outputs which are listed like this:
    // <out>;lib=<bin>;dev=<dev>
    // where <…> are all out paths
    let outpaths = segments[len - 1]
      .split(|c| *c == 0x3B) // ';'
      .map(|s| s.split(|c| *c == 0x3D).last().unwrap()) // '=', should never
      .map(|s| OutPath(s.to_owned()))                   // panic if s non-empty
      .collect();

    Some((outpaths, Attr(segments[0].to_owned())))
  } else {
    None
  }
}

fn insert_out_paths<T: Read>(
  input: T,
  map: &mut OutPathMap
) -> std::io::Result<()> {
  let mut reader = BufReader::new(input);

  for line in reader.split(0x0A) {
    match line.map(|s| parse_nix_env_line(s.as_slice()))? {
      Some((outs, attr)) =>
        for out in outs {
          map.insert(out, attr.clone());
        },
      None => continue,
    };
  }

  Ok(())
}

fn print_changed<T: Read>(
  mode: Mode,
  input: T,
  map: &OutPathMap
) -> std::io::Result<()> {
  let mut reader = BufReader::new(input);

  for line in reader.split(0x0A) {
    match line.map(|s| parse_nix_env_line(s.as_slice()))? {
      Some((outs, attr)) =>
        for out in outs {
          if map.get(&out).is_some() {
            println!("{}", String::from_utf8_lossy(
                match mode {
                  Mode::PrintOutPaths => &out.0,
                  Mode::PrintAttrs => &attr.0
                }));

            if mode == Mode::PrintAttrs {
              // only need to print the attr once
              break;
            }
          }
        },
      None => continue,
    }
  }

  Ok(())
}

fn main() -> std::io::Result<()> {
  fn print_usage() {
    eprintln!(r"Usage: nix-env-diff [--attrs] BASE [ NEW ]

BASE and NEW need to be both the output of a call to
`nix-env -qaP --out-path` or similar. If NEW is omitted,
stdin is used. `--json` is not supported because it
doesn't contain the output paths which are crucial.

nix-env-diff parses the outputs of nix-env and prints
all output paths (or attribute paths if --attrs is
given) that are in NEW, but not in BASE, i. e. all
new or changed attributes / output paths. This can
be used as a poor man's nixpkgs-review to inspect
all potential rebuilds between two evaluations of
nixpkgs with the added benefit that processing the
output is easier and the nix-env parameters can
be specified freely, so you can even get around
`lib.dontRecurseIntoAttrs`.

Options:

    --attrs    Print attributes instead of out paths
    --help     Print this message"
    );
  }

  let mut mode = Mode::PrintOutPaths;
  let (flags, args): (Vec<OsString>, Vec<OsString>) =
    args_os().skip(1)
      .partition(|a| a.as_bytes().starts_with(b"-"));

  for flag in flags {
    match flag.as_bytes() {
      b"--attrs" => mode = Mode::PrintAttrs,
      b"--help"  => {
        print_usage();
        exit(0);
      },
      _ => {
        print_usage();
        exit(100);
      }
    }
  }

  if args.len() < 1 {
    print_usage();
    exit(100);
  }

  let old = File::open(args[0].as_os_str())?;
  let new = args.get(1)
                .map(|p| File::open(p.as_os_str()))
                .transpose()?;

  let mut old_outpaths = HashMap::new();

  insert_out_paths(old, &mut old_outpaths)?;
  match new {
    Some(f) => print_changed(mode, f, &old_outpaths),
    None    => print_changed(mode, std::io::stdin(), &old_outpaths),
  }?;

  Ok(())
}

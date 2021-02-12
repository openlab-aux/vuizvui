use std::ffi::{OsStr, OsString};
use std::io::{Error, ErrorKind, self, Write};
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::process::{Command,ExitStatus};

struct TempDir {
    inner: Vec<u8>,
}

impl AsRef<Path> for TempDir {
    fn as_ref(&self) -> &Path {
        OsStr::from_bytes(&self.inner[..]).as_ref()
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        std::fs::remove_dir_all(self.as_ref());
        std::fs::remove_dir(self.as_ref());
    }
}

fn mktemp(suffix: &str) -> std::io::Result<TempDir> {
    let mut mktemp = Command::new("mktemp")
                    .arg("-d")
                    .arg("--suffix")
                    .arg(suffix)
                    .output()?;

    if mktemp.status.success() {
        // remove trailing newline
        if mktemp.stdout.ends_with(b"\n") {
            mktemp.stdout.pop();
        }

        Ok(TempDir {
            inner: mktemp.stdout
        })
    } else {
        Err(Error::new(ErrorKind::Other, "mktemp exited with a non-zero status"))
    }
}

// TODO(sterni): track helpful error context in this enum
enum NmanError {
    NoTempDir,
    Instantiate,
    Build,
    Man,
    NotFound,
    NixParseError,
    Usage,
    Execution,
}

impl NmanError {
    fn code(&self) -> i32 {
        match self {
            NmanError::NoTempDir => 9,
            NmanError::Instantiate => 10,
            NmanError::Build => 11,
            NmanError::Man => 12,
            NmanError::NotFound => 1,
            NmanError::NixParseError => 69, // EX_SOFTWARE
            NmanError::Usage => 64, // EX_USAGE
            NmanError::Execution => 127, // like bash
        }
    }

    fn msg(&self) -> &str {
        match self {
            NmanError::NoTempDir => "failed to create temporary directory",
            NmanError::Instantiate => "failure evaluating the attribute",
            NmanError::Build => "failed to realise derivation",
            NmanError::Man => "couldn't open man page using man(1)",
            NmanError::NotFound => "desired man page could not be found",
            NmanError::NixParseError => "nix executable produced unexpected output",
            NmanError::Usage => "usage error",
            NmanError::Execution => "Couldn't execute required command",
        }
    }
}

#[derive(PartialEq, PartialOrd, Eq, Ord)]
enum DrvOutput<'a> {
    Man,
    DevMan,
    Out,
    Doc,
    DevDoc,
    Info,
    Dev,
    Bin,
    Lib,
    Other(&'a [u8]),
}

struct DrvWithOutput<'a> {
    path: &'a [u8],
    output: DrvOutput<'a>,
    rendered: &'a [u8],
}

impl DrvWithOutput<'_> {
    fn render(&self) -> OsString {
        match self.output {
            DrvOutput::Out => {
                let mut r = OsStr::from_bytes(self.rendered).to_os_string();
                r.push("!out");
                r
            }
            _ => OsStr::from_bytes(self.rendered).to_os_string(),
        }
    }
}

fn parse_output<'a>(output: &'a [u8]) -> DrvOutput<'a> {
    match output {
        b"out" => DrvOutput::Out,
        b"bin" => DrvOutput::Bin,
        b"lib" => DrvOutput::Lib,
        b"man" => DrvOutput::Man,
        b"dev" => DrvOutput::Dev,
        b"devdoc" => DrvOutput::DevDoc,
        b"devman" => DrvOutput::DevMan,
        _ => DrvOutput::Other(output),
    }
}

fn parse_drv_path<'a>(drv_path: &'a [u8]) -> Option<DrvWithOutput<'a>> {
    let mut split = drv_path.split(|c| char::from(*c) == '!');
    let path = split.next().filter(|s| s.len() > 0)?;
    let output = split.next()
                      .map(parse_output)
                      .unwrap_or(DrvOutput::Out);

    match split.next() {
        None => Some(DrvWithOutput {
            path: path,
            output: output,
            rendered: drv_path,
        }),
        Some(_) => None,
    }
}

fn build_man_page(drv: DrvWithOutput, section: &str, page: &str, tempdir: &TempDir) -> Result<Option<PathBuf>, NmanError> {
    let mut build = Command::new("nix-store")
                            .arg("--realise")
                            .arg(drv.render())
                            .arg("--add-root")
                            .arg(tempdir.as_ref().join("build-result"))
                            .arg("--indirect")
                            .output()
                            .map_err(|_| NmanError::Execution)?;

    if !build.status.success() {
        return Err(NmanError::Build);
    }

    // get the first line of the output, usually only one line
    // is printed, but this way we also get rid of the trailing '\n'
    let first_path = build.stdout.split(|c| char::from(*c) == '\n')
                          .next().ok_or(NmanError::Build)?;

    // TODO(sterni): 😑😑😑😑😑😑😑😑😑😑😑
    let mut path = PathBuf::from(OsStr::from_bytes(first_path))
                       .join("share/man")
                       .join(format!("man{}", section))
                       .join(page);

    path.set_extension(format!("{}.gz", section));

    if path.exists() {
        Ok(Some(path))
    } else {
        // check for uncompressed man page if the derivation
        // has no / a custom fixupPhase
        path.set_extension(section);

        if path.exists() {
            Ok(Some(path))
        } else {
            Ok(None)
        }
    }
}

fn open_man_page(attr: &str, section: &str, page: &str) -> Result<(), NmanError> {
    let tmpdir = mktemp("-nman").map_err(|_| NmanError::NoTempDir)?;
    let expr = format!("with (import <nixpkgs> {{}}); builtins.map (o: {}.\"${{o}}\") {}.outputs", attr, attr);
    let inst = Command::new("nix-instantiate")
                       .arg("-E")
                       .arg(expr)
                       .arg("--add-root")
                       .arg(tmpdir.as_ref().join("instantiation-result"))
                       .arg("--indirect")
                       .output()
                       .map_err(|_| NmanError::Execution)?;

    if !inst.status.success() {
        return Err(NmanError::Instantiate);
    }

    let mut drvs: Vec<DrvWithOutput> =
            inst.stdout.split(|c| char::from(*c) == '\n')
                .filter_map(parse_drv_path).collect();

    // the sort order is such that the outputs where we
    // expect the man page to be are checked first.
    // This means we realise the least amount of outputs
    // necessary
    //
    // TODO(sterni): change sorting depending on section:
    //               "3" and "3p" should prioritize DevMan
    drvs.sort_unstable_by(|a, b| a.output.cmp(&b.output));

    if drvs.len() <= 0 {
        return Err(NmanError::NixParseError);
    }

    for drv in drvs {
        let man_file = build_man_page(drv, section, page, &tmpdir)?;

        match man_file {
            None => continue,
            Some(f) => {
                let res = Command::new("man")
                                  .arg("-l").arg(f)
                                  .spawn()
                                  .and_then(|mut c| c.wait())
                                  .map(|c| c.success());

                return match res {
                    Ok(true) => Ok(()),
                    Ok(false) => Err(NmanError::Man),
                    Err(_) => Err(NmanError::Execution),
                };
            },
        }
    }

    Err(NmanError::NotFound)
}

fn parse_man_section(section: &str) -> Result<&str, &str> {
    match section {
        "3p" => Ok(section),
        _ => match u8::from_str_radix(section, 10) {
            Ok(_) => Ok(section),
            Err(_)  => Err("Invalid man section: not a number and not \"3p\""),
        },
    }
}

enum CliAction<'a> {
    Usage,
    // attribute, section, page
    // TODO(sterni): section should be an option type, so we can implement
    //               the search logic as man(1) does. Also worth considering
    //               would be to not find the man page ourselves, but to just
    //               set MANPATH and let man(1) do the searching. Use case
    //               would be to allow
    //                  nman libunwind unw_apply_reg_state
    //              to work like
    //                  nman libunwind 3 unw_apply_reg_state
    Man(&'a str, &'a str, &'a str),
}

fn main() -> std::io::Result<()> {
    fn dispatch_action(progname: &str, action: CliAction) -> std::io::Result<()> {
        match action {
            CliAction::Usage => {
                println!("Usage: {} ATTR [SECTION | PAGE] [PAGE]", progname);
                Ok(())
            },
            CliAction::Man(attr, section, page) =>
                match open_man_page(attr, section, page) {
                    Ok(_) => Ok(()),
                    Err(t) => {
                        eprintln!("error: {}", t.msg());
                        std::process::exit(t.code())
                    },
                },
        }
    }

    let (opts, args) : (Vec<String>, Vec<String>) =
            std::env::args().partition(|s| s.starts_with("-"));

    let mut action : Result<CliAction, &str> = match args.len() {
        2 => Ok(CliAction::Man(&args[1], "1", &args[1])),
        3 => Ok(match parse_man_section(&args[2]) {
            Ok(sec) => CliAction::Man(&args[1], sec, &args[1]),
            Err(_) => CliAction::Man(&args[1], "1", &args[2]),
        }),
        4 => parse_man_section(&args[2])
                .map(|s| CliAction::Man(&args[1], s, &args[3])),
        _ => Err("Unexpected number of arguments"),
    };

    for opt in opts {
        match &opt[..] {
            "--help" | "--usage" | "-h" =>
                action = Ok(CliAction::Usage),
            _ => action = Err("Unknown option"),
        }
    }

    match action {
        Ok(action) => dispatch_action(&args[0], action),
        Err(msg) => {
            eprintln!("usage error: {}", msg);
            dispatch_action(&args[0], CliAction::Usage);
            std::process::exit(NmanError::Usage.code());
        },
    }
}

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

enum NmanError<'a> {
    NoTempDir,
    Instantiate(&'a str, Vec<u8>),
    Build(OsString, Vec<u8>),
    Man,
    NotFound(&'a str, &'a str),
    ParseError(&'a str),
    Usage,
    Execution(&'a str),
}

impl NmanError<'_> {
    fn code(&self) -> i32 {
        match self {
            // expected errors
            NmanError::NotFound(_, _) => 1,
            // most likely due to attribute missing
            NmanError::Instantiate(_, _) => 1,
            // missing executable
            NmanError::Execution(_) => 127,
            // user error, EX_USAGE (sysexits.h)
            NmanError::Usage => 64,
            // everything else is an unexpected error
            _ => 101
        }
    }

    fn msg(&self) -> String {
        match self {
            NmanError::NoTempDir => String::from("failed to create temporary directory"),
            NmanError::Instantiate(attr, stderr) =>
                format!("could not instantiate \"{}\". nix-instantiate reported:\n{}", attr,
                        std::str::from_utf8(&stderr).unwrap_or("<invalid utf-8>")),
            NmanError::Build(drv_path, stderr) =>
                format!("failed to build \"{}\". nix-store reported:\n{}",
                        drv_path.to_str().unwrap_or("<invalid utf-8>"),
                        std::str::from_utf8(&stderr).unwrap_or("<malformed utf-8>")),
            NmanError::Man => String::from("man failed while opening while opening man page"),
            NmanError::NotFound(page, sec) => format!("man page {}({}) could not be found", page, sec),
            NmanError::ParseError(exec) => format!("could not parse output of {}", exec),
            NmanError::Execution(exec) => format!("could not execute {}", exec),
            NmanError::Usage => String::from("usage error"),
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

fn build_man_page<'a>(drv: DrvWithOutput, section: &str, page: &str, tempdir: &TempDir) -> Result<Option<PathBuf>, NmanError<'a>> {
    let mut build = Command::new("nix-store")
                            .arg("--realise")
                            .arg(drv.render())
                            .arg("--add-root")
                            .arg(tempdir.as_ref().join("build-result"))
                            .arg("--indirect")
                            .output()
                            .map_err(|_| NmanError::Execution("nix-store"))?;

    if !build.status.success() {
        return Err(NmanError::Build(drv.render(), build.stderr));
    }

    // get the first line of the output, usually only one line
    // is printed, but this way we also get rid of the trailing '\n'
    let first_path = build.stdout.split(|c| char::from(*c) == '\n')
                          .next().filter(|l| l.len() > 0)
                          .ok_or(NmanError::ParseError("nix-store"))?;

    let mut path = PathBuf::from(OsStr::from_bytes(first_path));
    path.push("share/man");
    path.push(format!("man{}", section));
    path.push(page);

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

fn open_man_page<'a>(attr: &'a str, section: &'a str, page: &'a str) -> Result<(), NmanError<'a>> {
    let tmpdir = mktemp("-nman").map_err(|_| NmanError::NoTempDir)?;
    let expr = format!("with (import <nixpkgs> {{}}); builtins.map (o: {}.\"${{o}}\") {}.outputs", attr, attr);
    let inst = Command::new("nix-instantiate")
                       .arg("-E")
                       .arg(expr)
                       .arg("--add-root")
                       .arg(tmpdir.as_ref().join("instantiation-result"))
                       .arg("--indirect")
                       .output()
                       .map_err(|_| NmanError::Execution("nix-instantiate"))?;

    if !inst.status.success() {
        return Err(NmanError::Instantiate(attr, inst.stderr));
    }

    let mut drvs: Vec<DrvWithOutput> =
            inst.stdout.split(|c| char::from(*c) == '\n')
                .filter_map(parse_drv_path).collect();

    if drvs.len() <= 0 {
        return Err(NmanError::ParseError("nix-instantiate"));
    }

    // the sort order is such that the outputs where we
    // expect the man page to be are checked first.
    // This means we realise the least amount of outputs
    // necessary
    //
    // TODO(sterni): change sorting depending on section:
    //               "3" and "3p" should prioritize DevMan
    drvs.sort_unstable_by(|a, b| a.output.cmp(&b.output));

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
                    Err(_) => Err(NmanError::Execution("man")),
                };
            },
        }
    }

    Err(NmanError::NotFound(page, section))
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
                        let msg = t.msg();
                        eprint!("error: {}", msg);
                        if !msg.ends_with("\n") {
                            eprint!("\n");
                        }
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

extern crate temp;

use std::ffi::{OsStr, OsString};
use std::os::unix::ffi::OsStrExt;
use std::path::PathBuf;
use std::process::Command;
use temp::TempDir;

/// Represents all errors that can occurr in `nman`.
/// The inner structure of this type is rather messy
/// as it is highly specific to the location it may
/// occurr, so whatever is most efficient is passed
/// back.
///
/// The common interface is `err.msg()` for building
/// an user facing error message for an `NmanError`
/// and `err.code()` for returning a descriptive
/// exit code for the occurred error (not that it
/// really matters for an interactive tool).
enum NmanError<'a> {
    IO,
    Instantiate(&'a str, Vec<u8>),
    Build(OsString, Vec<u8>),
    Man,
    NotFound(&'a str, Option<&'a str>),
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
            // TODO(sterni): make more descriptive
            NmanError::IO => String::from("unexpected IO error"),
            NmanError::Instantiate(attr, stderr) =>
                format!("could not instantiate \"{}\". nix-instantiate reported:\n{}", attr,
                        std::str::from_utf8(&stderr).unwrap_or("<invalid utf-8>")),
            NmanError::Build(drv_path, stderr) =>
                format!("failed to build \"{}\". nix-store reported:\n{}",
                        drv_path.to_str().unwrap_or("<invalid utf-8>"),
                        std::str::from_utf8(&stderr).unwrap_or("<malformed utf-8>")),
            NmanError::Man => String::from("man failed while opening while opening man page"),
            NmanError::NotFound(page, sec) => format!("man page {}({}) could not be found", page, sec.unwrap_or("?")),
            NmanError::ParseError(exec) => format!("could not parse output of {}", exec),
            NmanError::Execution(exec) => format!("could not execute {}", exec),
            NmanError::Usage => String::from("usage error"),
        }
    }
}

/// Represents an output of a Nix derivation.
/// These can theoretically be any strings,
/// but are limited to the first 9 options
/// in `nixpkgs` by convention.
///
/// The main purpose of parsing derivation
/// outputs is to order them from most
/// likely to least likely to contain man
/// pages to save on realizing store paths.
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

impl<'a> DrvOutput<'a> {
    /// Convert a string (Nix strings may be arbitrary bytes)
    /// into a parsed [`DrvOutput`]. No sanity checking is
    /// done, anything strange is pased into [`DrvOutput::Other`].
    fn parse(output: &'a [u8]) -> Self {
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
}

/// A derivation represented as a path
/// coupled with a parsed [`DrvOutput`]
/// for sorting purposes.
struct DrvWithOutput<'a> {
    /// The original derivation path as printed
    /// by `nix-instantiate` _including_ the output
    /// indicator if `output` is not [`DrvOutput::Out`]
    path: &'a [u8],
    /// The parsed output of `path` for sorting purposes
    output: DrvOutput<'a>,
}

impl DrvWithOutput<'_> {
    fn render(&self) -> OsString {
        match self.output {
            DrvOutput::Out => {
                let mut r = OsStr::from_bytes(self.path).to_os_string();
                r.push("!out");
                r
            }
            _ => OsStr::from_bytes(self.path).to_os_string(),
        }
    }
}

impl<'a> DrvWithOutput<'a> {
    /// Parse a line of the output of `nix-instantiate`, of the form:
    /// `/nix/store/<drv file>[!<output>]` into a [`DrvWithOutput`]
    /// structure.
    fn parse(drv_path: &'a [u8]) -> Option<Self> {
        let mut split = drv_path.split(|c| char::from(*c) == '!');
        let _ = split.next().filter(|s| s.len() > 0)?;
        let output = split.next()
            .map(DrvOutput::parse)
            .unwrap_or(DrvOutput::Out);

        match split.next() {
            None => Some(DrvWithOutput {
                path: drv_path,
                output: output,
            }),
            Some(_) => None,
        }
    }
}

/// Realises the given derivation output using `nix-store --realise` and
/// checks if the man page described by `section` and `page` can be found
/// within it. If that is the case, the path to is returned. If it can't
/// be found, `None` is returned. `Err` is only used to describe unrecoverable
/// errors.
///
/// `section == None` indicates that the section is not given. `build_man_page`
/// then searches all man section directories for any matching page. If multiple
/// matches exist, the one with an alphanumerically lower section is preferred,
/// e. g. section 1 is preferred over section 3.
fn build_man_page<'a>(drv: DrvWithOutput, section: Option<&str>, page: &str, tempdir: &TempDir) -> Result<Option<PathBuf>, NmanError<'a>> {
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

    // no share/man, no man pages
    if !path.exists() {
        return Ok(None);
    }

    // expected sub directory of share/man or, if no section
    // is given, all potential sub directories
    let mut section_dirs: Vec<OsString> =
        match section {
            Some(s) => vec!(OsString::from(format!("man{}", s))),
            None => {
                std::fs::read_dir(path.as_path())
                    .map_err(|_| NmanError::IO)?
                    .filter_map(|entry| entry.ok())
                    .map(|entry| entry.file_name())
                    .collect()
            },
        };

    // sorting should be ascending in terms of numerics,
    // apart from that, not many requirements
    section_dirs.sort();

    const EXTENSIONS: [&str; 2] = [ ".gz", "" ];

    for dir in section_dirs {
        // separate "man" prefix from section indicator,
        // while validating the particular sub directory
        match dir.to_str().filter(|d| d.len() > 3).map(|d| d.split_at(3)) {
            Some((_, "")) => continue,
            Some(("man", s)) => {
                // we have a valid man dir, check if it contains our page
                path.push(dir.as_os_str());
                path.push(page);

                // for nix we almost always have .{section}.gz as extension,
                // but traditionally plain .{section} is common and possible
                for ext in EXTENSIONS.iter() {
                    path.set_extension(format!("{}{}", s, ext));

                    if path.exists() {
                        return Ok(Some(path));
                    }
                }

                // reset the PathBuf if we didn't find anything
                path.pop(); // page
                path.pop(); // section directory
            },
            _ => continue,
        }
    }

    Ok(None)
}

/// This function implements the main operation of `nman`:
/// It instantiates the given attribute to get all outputs
/// of the described derivation and checks the outputs
/// for the desired man page using `build_man_page`.
/// Finally the man page is opened using `man(1)`.
/// Both GNU's `man-db` and OpenBSD's `mandoc` work
/// (any man implementation that implements `-l` should
/// for that matter).
fn open_man_page<'a>(attr: &'a str, section: Option<&'a str>, page: &'a str) -> Result<(), NmanError<'a>> {
    let tmpdir = TempDir::new("nman").map_err(|_| NmanError::IO)?;
    // TODO(sterni): allow selecting other base package sets,
    //               like <vuizvui>, /home/lukas/src/nix/nixpkgs, …
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
                .filter_map(DrvWithOutput::parse).collect();

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

/// Check if a string describes a man section,
/// i. e. is a number or "3p" (Perl Developer's
/// manual). Used to distinguish between man pages
/// and manual sections on the command line.
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
    /// print help
    Usage,
    /// attribute, section, page
    Man(&'a str, Option<&'a str>, &'a str),
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
        2 => Ok(CliAction::Man(&args[1], None, &args[1])),
        3 => Ok(match parse_man_section(&args[2]) {
            Ok(s) => CliAction::Man(&args[1], Some(s), &args[1]),
            Err(_) => CliAction::Man(&args[1], None, &args[2]),
        }),
        4 => parse_man_section(&args[2])
                .map(|s| CliAction::Man(&args[1], Some(s), &args[3])),
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

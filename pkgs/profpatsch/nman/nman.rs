extern crate temp;

use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::fs::{canonicalize, read_dir};
use std::io::Write;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::process::ExitStatusExt;
use std::path::PathBuf;
use std::process::{Command, ExitStatus, Stdio};
use temp::TempDir;

enum CliAction<'a> {
    Man {
        attr: &'a str,
        section: Option<&'a str>,
        page: &'a str,
        file_path: Option<&'a str>,
    },
}

enum CliResult<'a> {
    ShowUsage { err_msg: Option<&'a str> },
    Action(CliAction<'a>),
}

fn main() {
    use CliResult::*;
    let all_args: Vec<String> = std::env::args().collect();
    
    let mut is_debug: bool = false;
    let mut file_path: Option<&str> = None;
    let mut cli_res: CliResult = ShowUsage { err_msg: Some("Unexpected number of arguments") };
    let mut positional_args = Vec::new();

    let mut i = 1; // Skip program name
    while i < all_args.len() {
        let arg = &all_args[i];
        
        if arg.starts_with("-") {
            match &arg[..] {
                "--help" | "--usage" | "-h" => {
                    cli_res = ShowUsage { err_msg: None };
                    break;
                },
                "--verbose" | "-v" => is_debug = true,
                "-f" | "--file" => {
                    if i + 1 >= all_args.len() {
                        cli_res = ShowUsage {
                            err_msg: Some("Option -f/--file requires an argument"),
                        };
                        break;
                    }
                    file_path = Some(&all_args[i + 1]);
                    i += 1; // Skip the next argument since it's the file path
                },
                _ => {
                    cli_res = ShowUsage {
                        err_msg: Some("Unknown option"),
                    };
                    break;
                }
            }
        } else {
            positional_args.push(arg);
        }
        i += 1;
    }
    
    // Only process positional arguments if we haven't already set an error
    if let ShowUsage { err_msg: _ } = cli_res {
        cli_res = match positional_args.len() {
            1 => Action(CliAction::Man {
                attr: &positional_args[0],
                section: None,
                page: extract_page_name_from_attr(&positional_args[0]),
                file_path,
            }),
            2 => match parse_man_section(&positional_args[1]) {
                Ok(s) => Action(CliAction::Man {
                    attr: &positional_args[0],
                    section: Some(s),
                    page: extract_page_name_from_attr(&positional_args[0]),
                    file_path,
                }),
                Err(_) => Action(CliAction::Man {
                    attr: &positional_args[0],
                    section: None,
                    page: &positional_args[1],
                    file_path,
                }),
            },
            3 => match parse_man_section(&positional_args[1]) {
                Err(err_msg) => ShowUsage {
                    err_msg: Some(err_msg),
                },
                Ok(s) => Action(CliAction::Man {
                    attr: &positional_args[0],
                    section: Some(s),
                    page: &positional_args[2],
                    file_path,
                }),
            },
            _ => ShowUsage {
                err_msg: Some("Unexpected number of arguments"),
            },
        };
    }

    let main = Main { is_debug };
    match cli_res {
        ShowUsage { err_msg } => {
            if let Some(msg) = err_msg {
                eprintln!("nman: usage error: {}", msg);
            }
            println!("Usage: {} [OPTIONS] ATTR [PAGE | SECTION [PAGE]]", &all_args[0]);
            println!("Options:");
            println!("  -h, --help, --usage  Show this help message");
            println!("  -v, --verbose        Enable verbose output");
            println!("  -f, --file <path>    Use specified .nix file instead of <nixpkgs>");
            std::process::exit(NmanError::Usage.code());
        }
        Action(action) => match action {
            CliAction::Man { attr, section, page, file_path } => match main.open_man_page(attr, section, page, file_path) {
                Ok(_) => (),
                Err(t) => {
                    let msg = t.msg();
                    eprint!("nman: error: {}", msg);
                    if !msg.ends_with("\n") {
                        eprint!("\n");
                    }
                    std::process::exit(t.code())
                }
            },
        },
    }
}

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
    IO(std::io::Error),
    Instantiate(&'a str, ExitStatus),
    Build(OsString, ExitStatus),
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
            // user error
            NmanError::Usage => 100,
            // everything else is an unexpected error
            _ => 101,
        }
    }

    fn msg(&self) -> String {
        match self {
            NmanError::IO(err) => format!("unexpected IO error occurred: {}", err),
            NmanError::Instantiate(attr, s) => format!(
                "could not instantiate \"{}\", nix-instantiate {}.",
                attr,
                pretty_exit_status(s)
            ),
            NmanError::Build(drv_path, s) => format!(
                "failed to build \"{}\", nix-store {}.",
                drv_path.to_string_lossy(),
                pretty_exit_status(s)
            ),
            NmanError::Man => String::from("man failed while opening while opening man page"),
            NmanError::NotFound(page, sec) => format!(
                "man page {}({}) could not be found. Run with `--verbose` to see more.",
                page,
                sec.unwrap_or("?")
            ),
            NmanError::ParseError(exec) => format!("could not parse output of {}", exec),
            NmanError::Execution(exec) => format!("could not execute {}", exec),
            NmanError::Usage => String::from("usage error"),
        }
    }
}

/// Pretty print an [`ExitStatus`]
fn pretty_exit_status(status: &ExitStatus) -> String {
    match status.code() {
        Some(i) => format!("exited with {}", i),
        None => match status.signal() {
            Some(s) => format!("was killed by signal {}", s),
            None => String::from("exited for unknown reason"),
        },
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
#[derive(PartialEq, PartialOrd, Eq, Ord, Debug)]
enum DrvOutput<'a> {
    Man,
    DevMan,
    Doc,
    Out,
    DevDoc,
    // Info,
    Dev,
    Bin,
    Lib,
    Other(&'a [u8]),
}

impl<'a> DrvOutput<'a> {
    /// Convert a string (Nix strings may be arbitrary bytes)
    /// into a parsed [`DrvOutput`]. No sanity checking is
    /// done, anything strange is passed into [`DrvOutput::Other`].
    fn parse(output: &'a [u8]) -> Self {
        match output {
            b"out" => DrvOutput::Out,
            b"bin" => DrvOutput::Bin,
            b"lib" => DrvOutput::Lib,
            b"doc" => DrvOutput::Doc,
            b"man" => DrvOutput::Man,
            b"dev" => DrvOutput::Dev,
            b"devdoc" => DrvOutput::DevDoc,
            b"devman" => DrvOutput::DevMan,
            _ => DrvOutput::Other(output),
        }
    }

    fn display(&self) -> Cow<str> {
        match self {
            DrvOutput::Out => Cow::Borrowed("out"),
            DrvOutput::Bin => Cow::Borrowed("bin"),
            DrvOutput::Lib => Cow::Borrowed("lib"),
            DrvOutput::Doc => Cow::Borrowed("doc"),
            DrvOutput::Man => Cow::Borrowed("man"),
            DrvOutput::Dev => Cow::Borrowed("dev"),
            DrvOutput::DevDoc => Cow::Borrowed("devdoc"),
            DrvOutput::DevMan => Cow::Borrowed("devman"),
            DrvOutput::Other(s) => String::from_utf8_lossy(s),
        }
    }
}

/// A derivation represented as a path
/// coupled with a parsed [`DrvOutput`]
/// for sorting purposes.
#[derive(Debug, PartialEq, Eq)]
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
        let output = split.next().map(DrvOutput::parse).unwrap_or(DrvOutput::Out);

        match split.next() {
            None => Some(DrvWithOutput {
                path: drv_path,
                output: output,
            }),
            Some(_) => None,
        }
    }
}

struct Main {
    /// Whether the program is running in debug mode
    is_debug: bool,
}

#[derive(Debug)]
struct DerivationOutputPath(PathBuf);

struct BuildResult {
    first_path: DerivationOutputPath,
}

enum OutputDirResult {
    NoManDir,
    NoManPageFound { ignored_manpages: Vec<FoundManPage> },
    FoundManPage(PathBuf),
}

#[derive(Debug)]
struct FoundManPage {
    man_section: String,
    file_name: String,
    drv_share_man: PathBuf,
}

impl FoundManPage {
    /// From a share/man path we can reconstruct the full manpage path.
    fn manpage_path(&self) -> PathBuf {
        self.drv_share_man
            .join(String::from("man") + &self.man_section)
            .join(&self.file_name)
    }
}

impl Main {
    /// This function implements the main operation of `nman`:
    /// It instantiates the given attribute to get all outputs
    /// of the described derivation and checks the outputs
    /// for the desired man page using `build_man_page`.
    /// Finally the man page is opened using `man(1)`.
    /// Both GNU's `man-db` and OpenBSD's `mandoc` work
    /// (any man implementation that implements `-l` should
    /// for that matter).
    fn open_man_page<'a>(
        &self,
        attr: &'a str,
        section: Option<&'a str>,
        page: &'a str,
        file_path: Option<&'a str>,
    ) -> Result<(), NmanError<'a>> {
        let tmpdir = TempDir::new("nman").map_err(NmanError::IO)?;
        let nix_import = match file_path {
            Some(path) => {
                if path.starts_with('/') {
                    // Absolute path - strip trailing slashes
                    path.trim_end_matches('/').to_string()
                } else {
                    // Relative path - prepend ./
                    format!("./{}", path)
                }
            },
            None => "<nixpkgs>".to_string(),
        };
        let expr = format!(
            "with (import {} {{}}); builtins.map (o: {}.\"${{o}}\") {}.outputs",
            nix_import, attr, attr
        );
        let inst = self
            .debug_log_command(
                Command::new("nix-instantiate")
                    .arg("-E")
                    .arg(expr)
                    .arg("--add-root")
                    .arg(tmpdir.as_ref().join("instantiation-result"))
                    .arg("--indirect")
                    .stderr(Stdio::inherit()),
            )
            .and_then(|cmd| cmd.output())
            .map_err(|_| NmanError::Execution("nix-instantiate"))?;

        if !inst.status.success() {
            return Err(NmanError::Instantiate(attr, inst.status));
        }

        let mut drvs: Vec<DrvWithOutput> = inst
            .stdout
            .split(|c| char::from(*c) == '\n')
            .filter_map(DrvWithOutput::parse)
            .collect();

        if drvs.len() <= 0 {
            return Err(NmanError::ParseError("nix-instantiate"));
        }

        self.debug_log(format!(
            "Found derivation outputs: {:?}",
            drvs.iter()
                .map(|drv| drv.output.display())
                .collect::<Vec<_>>()
        ));

        // the sort order is such that the outputs where we
        // expect the man page to be are checked first.
        // This means we realise the least amount of outputs
        // necessary
        //
        // TODO(sterni): change sorting depending on section:
        //               "3" and "3p" should prioritize DevMan
        drvs.sort_unstable_by(|a, b| a.output.cmp(&b.output));

        let manpage_display = format!(
            r#""{}{}""#,
            page,
            section.map_or(String::from(""), |m| format!("({})", m))
        );
        let mut all_ignored_manpages = Vec::new();
        for drv in drvs {
            self.debug_log(format!(
                r#"Searching for manpage {} in output "{}""#,
                manpage_display,
                drv.output.display()
            ));
            let build_result = self.build_drv_with_output(&drv, &tmpdir)?;
            let man_file = self.find_man_page(section, page, build_result)?;

            match man_file {
                OutputDirResult::NoManDir => {
                    self.debug_log(format!(
                        r#"no share/man directory found in output "{}""#,
                        &drv.output.display()
                    ));
                    continue;
                }
                OutputDirResult::NoManPageFound { ignored_manpages } => {
                    all_ignored_manpages.extend(ignored_manpages);
                    self.debug_log(format!(
                        r#"no manpage for {} found in output "{}""#,
                        manpage_display,
                        &drv.output.display()
                    ));
                    continue;
                }
                OutputDirResult::FoundManPage(file) => {
                    self.debug_log(format!(
                        r#"found manpage {} in output "{}", opening …"#,
                        manpage_display,
                        &drv.output.display()
                    ));
                    let res = self
                        .debug_log_command(Command::new("man").arg("-l").arg(file))
                        .and_then(|cmd| cmd.spawn())
                        .and_then(|mut c| c.wait())
                        .map(|c| c.success());

                    return match res {
                        Ok(true) => Ok(()),
                        Ok(false) => Err(NmanError::Man),
                        Err(_) => Err(NmanError::Execution("man")),
                    };
                }
            }
        }

        self.debug_log(format!(
            "We could not find a manpage. These are the ones we found in all outputs: {:#?}",
            all_ignored_manpages
                .into_iter()
                .map(|mp| mp.manpage_path())
                .collect::<Vec<_>>()
        ));
        Err(NmanError::NotFound(page, section))
    }

    /// Checks if the man page described by `section` and `page` can be found
    /// within it. If that is the case, the path to is returned. If it can't
    /// be found, `None` is returned. `Err` is only used to describe unrecoverable
    /// errors.
    ///
    /// `section == None` indicates that the section is not given. `build_man_page`
    /// then searches all man section directories for any matching page. If multiple
    /// matches exist, the one with an alphanumerically lower section is preferred,
    /// e. g. section 1 is preferred over section 3.
    fn find_man_page<'a>(
        &self,
        section: Option<&str>,
        page: &str,
        build_result: BuildResult,
    ) -> Result<OutputDirResult, NmanError<'a>> {
        let drv_share_man = build_result.first_path.0.join("share/man");

        // no share/man, no man pages
        if !drv_share_man.exists() {
            return Ok(OutputDirResult::NoManDir);
        }

        // expected sub directory of share/man or, if no section
        // is given, all potential sub directories
        let mut manpages: Vec<FoundManPage> = Self::enumerate_man_pages(&drv_share_man)?;

        // sorting should be ascending in terms of numerics,
        // apart from that, not many requirements
        manpages.sort_unstable_by(|man1, man2| man1.man_section.cmp(&man2.man_section));

        // take the first manpage that matches our criteria
        for manpage in &manpages {
            // If we want to restrict to a section, skip manpages of the wrong section.
            if let Some(sect) = section {
                if sect != manpage.man_section {
                    continue;
                }
            }
            if match_man_page_file(&manpage.file_name, &manpage.man_section, page) {
                return Ok(OutputDirResult::FoundManPage(manpage.manpage_path()));
            }
        }

        Ok(OutputDirResult::NoManPageFound {
            ignored_manpages: manpages,
        })
    }

    /// For the given manpage directory (`share/man`), return every manpage we can find.
    fn enumerate_man_pages<'a>(
        drv_share_man: &PathBuf,
    ) -> Result<Vec<FoundManPage>, NmanError<'a>> {
        let dirs = read_dir(drv_share_man.as_path()).map_err(NmanError::IO)?;
        let mut res = Vec::new();
        for entry in dirs.collect::<Vec<_>>() {
            // ignore directories/files that cannot be read
            if let Ok(section_dir) = entry {
                // separate "man" prefix from section indicator,
                // while validating the particular sub directory
                if let Some((prefix, man_section)) = section_dir
                    .file_name()
                    .to_str()
                    .filter(|d| d.len() > 3)
                    .map(|d| d.split_at(3))
                {
                    if prefix == "man" {
                        let manpages = read_dir(section_dir.path()).map_err(NmanError::IO)?;
                        for manpage in manpages.collect::<Vec<_>>() {
                            // ignore directories/files that cannot be read
                            if let Ok(Some(file_name)) =
                                manpage.map(|mp| mp.file_name().to_str().map(String::from))
                            {
                                res.push(FoundManPage {
                                    man_section: String::from(man_section),
                                    file_name,
                                    drv_share_man: drv_share_man.clone(),
                                })
                            }
                        }
                    }
                }
            }
        }
        Ok(res)
    }

    /// Realises the given derivation output using `nix-store --realise` and
    /// returns the path to the output directory.
    fn build_drv_with_output<'a>(
        &self,
        drv: &DrvWithOutput,
        tempdir: &TempDir,
    ) -> Result<BuildResult, NmanError<'a>> {
        let build = self
            .debug_log_command(
                Command::new("nix-store")
                    .arg("--realise")
                    .arg(drv.render())
                    .arg("--add-root")
                    .arg(tempdir.as_ref().join("build-result"))
                    .arg("--indirect")
                    .stderr(Stdio::inherit()),
            )
            .and_then(|cmd| cmd.output())
            .map_err(|_| NmanError::Execution("nix-store"))?;

        if !build.status.success() {
            return Err(NmanError::Build(drv.render(), build.status));
        }

        // get the first line of the output, usually only one line
        // is printed, but this way we also get rid of the trailing '\n'
        build
            .stdout
            .split(|c| char::from(*c) == '\n')
            .next()
            .filter(|l| l.len() > 0)
            .ok_or(NmanError::ParseError("nix-store"))
            .map(|path| {
                let p = PathBuf::from(OsStr::from_bytes(path));
                let store_path = canonicalize(&p).unwrap_or(p);
                BuildResult {
                    first_path: DerivationOutputPath(store_path),
                }
            })
    }

    fn debug_log<S>(&self, msg: S)
    where
        S: AsRef<str>,
        S: std::fmt::Display,
    {
        if self.is_debug {
            writeln!(std::io::stderr(), "{}", msg).unwrap()
        }
    }

    /// Log the given command to stderr, but only in debug mode
    fn debug_log_command<'a>(
        &self,
        cmd: &'a mut Command,
    ) -> Result<&'a mut Command, std::io::Error> {
        if self.is_debug {
            let mut formatted = vec![b'$', b' '];
            formatted.extend(
                vec![cmd.get_program()]
                    .into_iter()
                    .chain(cmd.get_args())
                    .map(|arg| simple_bash_escape(arg.as_bytes()))
                    .collect::<Vec<_>>()
                    .join(&b' '),
            );
            formatted.push(b'\n');
            std::io::stderr().write_all(&formatted).map(|()| cmd)
        } else {
            Ok(cmd)
        }
    }
}

/// Match if a file name is a man file matching the given
/// section and page. It is checked that the filename is
/// of the form `<page>.<section>` or
/// `<page>.<section>.<extra extension>` where extra
/// extension may not be a valid man section to prevent
/// mismatches if a man page name itself contains a valid
/// section extension.
fn match_man_page_file(name: &str, section: &str, page: &str) -> bool {
    let init = format!("{}.{}", page, section);
    let init_len = init.len();

    if !name.starts_with(&init[..]) {
        false
    } else {
        if name.len() == init_len {
            true
        } else {
            let rem = &name[init_len..];
            rem.chars().nth(0) == Some('.')                       // remainder is an extension
                && rem.chars().filter(|c| *c == '.').count() == 1 // only one extension
                && parse_man_section(&rem[1..]).is_err() // not a man extension
        }
    }
}

/// Extract the page name from a dotted attribute path.
/// For example, "pkgs.profpatsch.nman" becomes "nman".
/// If there are no dots, returns the original string.
fn extract_page_name_from_attr(attr: &str) -> &str {
    attr.split('.').last().unwrap_or(attr)
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
            Err(_) => Err("Invalid man section: not a number and not \"3p\""),
        },
    }
}

/// Simple escaping for bash words. If they contain anything that’s not ascii chars
/// and a bunch of often-used special characters, put the word in single quotes.
fn simple_bash_escape(arg: &[u8]) -> Cow<[u8]> {
    let mut is_simple: bool = true;
    let mut number_of_single_quotes: usize = 0;
    // any word that is just ascii characters is simple (no spaces or control characters)
    // or contains a few often-used characters like - or .
    for c in arg {
        if !(c.is_ascii_alphabetic() || c.is_ascii_digit() || [b'-', b'.', b':', b'/'].contains(c))
        {
            is_simple = false;
        }
        if *c == b'\'' {
            number_of_single_quotes += 1;
        }
    }
    if is_simple {
        return Cow::Borrowed(arg);
    }
    // Put the word in single quotes
    // If there is a single quote in the word,
    // close the single quoted word, add a single quote, open the word again
    // replace single quotes with `'\''` (i.e. escape from the string, then add a `'`, then open another string)
    if number_of_single_quotes > 0 {
        // we know the capacity we need to build the string, so vec will only allocate once
        let mut v = Vec::with_capacity(get_bash_escaped_capacity(arg, number_of_single_quotes));
        v.push(b'\'');
        for c in arg {
            if *c == b'\'' {
                v.extend(b"'\\''")
            } else {
                v.push(*c)
            }
        }
        v.push(b'\'');
        return Cow::Owned(v);
    }

    let mut v = Vec::with_capacity(arg.len() + 2);
    v.push(b'\'');
    v.extend(arg);
    v.push(b'\'');
    return Cow::Owned(v);
}

fn get_bash_escaped_capacity(arg: &[u8], number_of_single_quotes: usize) -> usize {
    arg.len()
    // initial `'` and final `'`
    + 2
    // replacing `'` with `'\''` adds three bytes
    + number_of_single_quotes * 3
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_section_parsing() {
        assert!(parse_man_section("1").is_ok());
        assert!(parse_man_section("2").is_ok());
        assert!(parse_man_section("3").is_ok());
        assert!(parse_man_section("3p").is_ok());
        assert!(parse_man_section("4").is_ok());
        assert!(parse_man_section("5").is_ok());
        assert!(parse_man_section("6").is_ok());
        assert!(parse_man_section("7").is_ok());
        assert!(parse_man_section("8").is_ok());
        assert!(parse_man_section("9").is_ok());

        assert!(parse_man_section("man").is_err());
        assert!(parse_man_section("ocamlPackages.sexp").is_err());
        assert!(parse_man_section("lowdown").is_err());
        assert!(parse_man_section("").is_err());
    }

    #[test]
    fn test_extract_page_name_from_attr() {
        // Simple case without dots
        assert_eq!(extract_page_name_from_attr("nman"), "nman");
        assert_eq!(extract_page_name_from_attr("hello"), "hello");
        
        // Dotted attribute paths
        assert_eq!(extract_page_name_from_attr("pkgs.profpatsch.nman"), "nman");
        assert_eq!(extract_page_name_from_attr("a.b.c.d"), "d");
        assert_eq!(extract_page_name_from_attr("nixpkgs.hello"), "hello");
        
        // Edge cases
        assert_eq!(extract_page_name_from_attr(""), "");
        assert_eq!(extract_page_name_from_attr("."), "");
        assert_eq!(extract_page_name_from_attr("package."), "");
        assert_eq!(extract_page_name_from_attr(".package"), "package");
    }

    #[test]
    fn test_output_preference() {
        // lower =^= preferred
        assert!(DrvOutput::Man < DrvOutput::Out);
        assert!(DrvOutput::DevMan < DrvOutput::Out);
        // assert!(DrvOutput::Out    < DrvOutput::Doc);
        assert!(DrvOutput::Out < DrvOutput::DevDoc);
        assert!(DrvOutput::Out < DrvOutput::Lib);
        assert!(DrvOutput::Out < DrvOutput::Bin);
        assert!(DrvOutput::Out < DrvOutput::Other(b"foo"));
    }

    const OUT: &[u8] = b"/nix/store/3v06l2clmzxx4pna0yd0wiggqlh7b33s-lowdown-0.8.1.drv";
    const DEVMAN: &[u8] = b"/nix/store/1ilsvw0y81mi8rdz2jp5kng2wakg2mq8-libunwind-1.4.0.drv!devman";

    #[test]
    fn test_drv_path_parsing() {
        assert_eq!(
            DrvWithOutput::parse(OUT),
            Some(DrvWithOutput {
                path: OUT,
                output: DrvOutput::Out,
            })
        );

        assert_eq!(
            DrvWithOutput::parse(DEVMAN),
            Some(DrvWithOutput {
                path: DEVMAN,
                output: DrvOutput::DevMan,
            })
        );
    }

    #[test]
    fn test_drv_path_rendering() {
        let mut expected_out_path = Vec::from(OUT);
        expected_out_path.extend(b"!out");

        let out = DrvWithOutput {
            path: OUT,
            output: DrvOutput::Out,
        };
        assert_eq!(
            out.render().as_os_str(),
            OsStr::from_bytes(&expected_out_path[..])
        );

        let devman = DrvWithOutput {
            path: DEVMAN,
            output: DrvOutput::DevMan,
        };
        assert_eq!(devman.render().as_os_str(), OsStr::from_bytes(DEVMAN));
    }

    #[test]
    fn test_man_page_matching() {
        assert!(match_man_page_file("man.1", "1", "man"));
        assert!(match_man_page_file("lowdown.3", "3", "lowdown"));
        assert!(match_man_page_file("lowdown.3.gz", "3", "lowdown"));
        assert!(match_man_page_file("magrep.1.gz", "1", "magrep"));
        assert!(match_man_page_file("CGI.3p", "3p", "CGI"));

        assert!(!match_man_page_file("lowdown.1", "3", "lowdown"));
        assert!(!match_man_page_file("lowdown.5.3", "3", "lowdown"));
        assert!(!match_man_page_file("lowdown.5.3", "5", "lowdown"));
        assert!(!match_man_page_file("mblaze.gz.1", "1", "mblaze"));

        // make sure these don't panic
        assert!(match_man_page_file("lowdown.3.", "3", "lowdown"));
        assert!(!match_man_page_file("lowdown.3f", "3", "lowdown"));
        assert!(match_man_page_file("lowdown.", "", "lowdown"));
        assert!(!match_man_page_file("", "", ""));
    }

    #[test]
    fn test_simple_bash_escape() {
        assert_eq!(&simple_bash_escape(b""), &vec![], "empty string");
        assert_eq!(
            &simple_bash_escape(b"abc"),
            &"abc".as_bytes(),
            "simple word"
        );
        assert_eq!(
            &simple_bash_escape(b"12ab3c4"),
            &"12ab3c4".as_bytes(),
            "simple word with digits"
        );
        assert_eq!(
            &simple_bash_escape(b"a-b.c:d/e"),
            &"a-b.c:d/e".as_bytes(),
            "simple word with allowed special chars"
        );
        assert_eq!(
            &simple_bash_escape("a$bc€de".as_bytes()),
            &"'a$bc€de'".as_bytes(),
            "escaped word with special chars"
        );
        assert_eq!(
            &simple_bash_escape("a'bc'".as_bytes()),
            &"'a'\\''bc'\\'''".as_bytes(),
            "escaped word with single quotes"
        );
        assert_eq!(
            &simple_bash_escape("a'bc'".as_bytes()),
            &"'a'\\''bc'\\'''".as_bytes(),
            "escaped word with single quotes"
        );
        assert_eq!(
            get_bash_escaped_capacity("a'bc'".as_bytes(), 2),
            13,
            "escaped vec capacity is correct"
        );
    }
}

//! Tiny temp dir/file crate.
//!
//! This crate implements a tiny rust wrapper around libc's
//! `mkdtemp(3)` and `mkstemp(3)` which has the following
//! notable features:
//!
//! * Temporary files and directories are distinguished on
//!   the type level: [`TempDir`] and [`TempFile`] are different
//!   types to ensure the right kind of temporary artifact
//!   is passed to a function and dealt with accordingly.
//! * The killer feature: Temporary artifacts are automatically
//!   deleted as soon as the associated value goes out of
//!   scope using the [`Drop`] trait, meaning a) it is impossible
//!   to forget to delete a temporary artifact and b) temporary
//!   artifact are cleaned up as soon as possible.
//!
//! The intended use of this crate is to create the desired
//! artifact via [`TempDir::new()`] or [`TempFile::new()`].
//! Interfacing with the rest of rust's `std` is possible by using
//! the [`AsRef`] trait to get a [`Path`] reference for a given
//! temporary artifact.
//!
//! Note that you need to take care not to let the [`TempDir`] or
//! [`TempFile`] get dropped early while you are still using a copy
//! of the represented path which can easily happen when using
//! `temp_dir.as_ref().join("foo.txt")` to work with a temporary
//! directory.
use std::ffi::OsStr;
use std::io::{Error, ErrorKind};
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::path::Path;

// libc interaction

#[link(name = "c")]
extern {
    fn mkdtemp(template: *mut u8) -> *mut u8;
    fn mkstemp(template: *mut u8) -> *mut u8;
}

fn template(prefix: &str) -> std::io::Result<Vec<u8>> {
    if prefix.contains('/') {
        return Err(Error::new(ErrorKind::Other, "prefix may not contain any slashes"));
    }

    // TODO(sterni): systemd support
    let mut template = std::env::temp_dir();
    template.push(prefix);

    // mkdtemp and mkstemp require the template to end in 6 or more 'X's
    template.set_extension("XXXXXX");

    Ok(template.into_os_string().into_vec())
}

// internal implementation for files and directories

enum TempKind {
    File,
    Dir,
}

struct Temp {
    path: Vec<u8>,
    kind: TempKind,
}

impl AsRef<Path> for Temp {
    fn as_ref(&self) -> &Path {
        OsStr::from_bytes(&self.path[..]).as_ref()
    }
}

impl Drop for Temp {
    fn drop(&mut self) {
        let _ = match self.kind {
            TempKind::File => std::fs::remove_file(self.as_ref()),
            TempKind::Dir => std::fs::remove_dir_all(self.as_ref()),
        };
    }
}

fn temp(kind: TempKind, prefix: &str) -> std::io::Result<Temp> {
    let mut tpl = template(prefix)?;
    tpl.push(0);
    let tpl_ptr: *mut u8 = tpl.as_mut_ptr();


    let res: *mut u8 = match kind {
        TempKind::Dir => unsafe { mkdtemp(tpl_ptr) },
        TempKind::File => unsafe { mkstemp(tpl_ptr) },
    };

    if res.is_null() {
        Err(Error::last_os_error())
    } else {
        // get rid of NUL byte
        tpl.pop();

        Ok(Temp {
            path: tpl,
            kind: kind,
        })
    }
}

// public, type safe API which wraps the internal one

pub struct TempDir(Temp);

impl TempDir {
    /// Create a temporary directory in the directory returned
    /// by [`std::env::temp_dir()`]. The temporary directory's
    /// name will have the following form: `<prefix>.XXXXXX`.
    /// The six `X`s are replaced by random characters.
    ///
    /// See `mkdtemp(3)` for details of the underlying
    /// implementation and possible errors.
    pub fn new(prefix: &str) -> std::io::Result<TempDir> {
        temp(TempKind::Dir, prefix).map(|t| TempDir(t))
    }
}

impl AsRef<Path> for TempDir {
    fn as_ref(&self) -> &Path {
        self.0.as_ref()
    }
}

pub struct TempFile(Temp);

impl TempFile {
    /// Create a temporary file in the directory returned
    /// by [`std::env::temp_dir()`]. The temporary file's
    /// name will have the following form: `<prefix>.XXXXXX`.
    /// The six `X`s are replaced by random characters.
    ///
    /// See `mkstemp(3)` for details of the underlying
    /// implementation and possible errors.
    pub fn new(prefix: &str) -> std::io::Result<TempFile> {
        temp(TempKind::File, prefix).map(|t| TempFile(t))
    }
}

impl AsRef<Path> for TempFile {
    fn as_ref(&self) -> &Path {
        self.0.as_ref()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Check that the temporary artifact is
    //
    //   * created as expected
    //   * deleted after it goes out of scope
    //
    // for both temp files and temp dirs.

    #[test]
    fn tempdir_exists() {
        let temp = TempDir::new("temp-dir-test");
        assert!(temp.map(|p| p.as_ref().exists()).unwrap())
    }

    #[test]
    fn tempdir_cleaned() {
        let temp_copy = {
            let temp = TempDir::new("temp-dir-test").unwrap();
            temp.as_ref().to_path_buf()
        };
        assert!(!temp_copy.exists())
    }

    #[test]
    fn tempfile_exists() {
        let temp = TempFile::new("temp-file-test");
        assert!(temp.map(|p| p.as_ref().exists()).unwrap())
    }

    #[test]
    fn tempfile_cleaned() {
        let temp_copy = {
            let temp = TempFile::new("temp-file-test").unwrap();
            temp.as_ref().to_path_buf()
        };
        assert!(!temp_copy.exists())
    }
}

{ pkgs, writeRustSimple }:

let

  # Given a directory, finds all files directly in that directory.
  # If the size of all files (ignoring symlinks and directories)
  # is bigger than MAX_SIZE, delete the 25% files with oldest mtime.
  #
  # Only deletes
  lru-dir = writeRustSimple "lru-dir" {} (pkgs.writeText "main.rs" ''
    use std::fs;
    use std::convert::identity;
    use std::path::Path;

    const MAX_SIZE : usize = 200 * 1000*1000; // 200 MB

    fn main() -> std::io::Result<()> {
      let mut args = std::env::args_os();
      let dir = args.nth(1).unwrap();
      // only files, only in dir
      // ignore all files that can’t be read
      let mut files = std::fs::read_dir(&dir)?
        .filter_map(|f| f.ok())
        .filter_map(|f| f.metadata().ok()
                         .map(|m| (f.path(), m)))
        .filter(|f| f.1.is_file())
        .collect::<Vec<(std::path::PathBuf, std::fs::Metadata)>>();
      let size : usize = files.iter().map(|f| f.1.len() as usize).sum();

      if size > MAX_SIZE {
        let all = files.len();
        // keep ~75% of files (no floats required)
        let to_keep : usize = (all >> 1) + (all >> 2);
        let to_delete : usize = all - to_keep;

        println!("{} is {} bytes (MAX = {})", &Path::new(&dir).display(), size, MAX_SIZE);
        println!("to keep: {}, to delete: {}", to_keep, to_delete);

        let really_delete = std::env::var("REALLY_DELETE").ok();

        if let None = really_delete {
          println!("dry-running. If you want to actually delete files, set REALLY_DELETE");
        }

        // sort oldest files to the front
        files.sort_by_cached_key(|f| f.1.modified().ok());

        for f in files.iter().take(to_delete) {
          match really_delete {
            None => {
              println!("Would delete: {}", f.0.display());
            },
            Some(_) => {
              println!("Deleting: {}", f.0.display());
              if let Err(e) = fs::remove_file(&f.0) {
                println!("Warning: could not remove {}: {:?}", f.0.display(), e);
              }
            }
          }
        }

      }

      Ok(())
    }
  '');


in {
  inherit
    lru-dir
    ;
}

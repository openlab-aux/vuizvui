
/// Every element inside the block starts with a space
const BLOCK_QUOTE_CHAR : u8 = b' ';
/// The end of a block is signified by an empty string
const BLOCK_END : &'static [u8] = &[];

/// A parsed execline argument.
#[derive(Debug, PartialEq, Eq)]
enum Arg<'a> {
    /// Normal argument.
    Arg(&'a [u8]),
    /// A block.
    ///
    /// On the command line a block is represented
    /// by a list of arguments which start with a space
    /// and end with an empty string.
    Block(Vec<&'a [u8]>)
}

#[derive(Debug, PartialEq, Eq)]
enum Error {
    /// The argument was not quoted, at index.
    UnquotedArgument(usize),
    /// The last block was not terminated
    UnterminatedBlock
}

/// Parse a command line into a list of `Arg`s.
///
/// Blocks can be nested by adding more spaces,
/// but `el_semicolon` will only parse one level.
/// Usually that is intended, because nested blocks
/// are intended to be parsed by nested programs.
fn el_semicolon<'a>(args: &'a [&'a [u8]]) -> Result<Vec<Arg<'a>>, Error> {
    let mut cur_block : Option<Vec<&'a [u8]>> = None;
    let mut res : Vec<Arg<'a>> = vec![];
    for (i, arg) in args.iter().enumerate() {
        if arg == &BLOCK_END {
            let bl = cur_block.take();
            match bl {
                None => res.push(Arg::Arg(arg)),
                Some(bl) => res.push(Arg::Block(bl))
            }
        } else {
            match arg[0] {
                BLOCK_QUOTE_CHAR => {
                    let new = &arg[1..];
                    cur_block = Some(cur_block.map_or_else(
                        || vec![new],
                        |mut bl| { bl.push(new); bl }
                    ))
                },
                _ => {
                    if cur_block != None {
                        return Err(Error::UnquotedArgument(i));
                    }
                    res.push(Arg::Arg(arg))
                }
            }
        }
    }
    if cur_block != None {
        Err(Error::UnterminatedBlock)
    } else {
        Ok(res)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn success() {
        assert_eq!(
            el_semicolon(&vec![
                "-b".as_bytes(),
                " echo".as_bytes(),
                " hi".as_bytes(),
                "".as_bytes(),
                "test".as_bytes(),
                "".as_bytes(),
            ]),
            Ok(vec![
                Arg::Arg("-b".as_bytes()),
                Arg::Block(vec![
                    "echo".as_bytes(),
                    "hi".as_bytes(),
                ]),
                Arg::Arg("test".as_bytes()),
                Arg::Arg("".as_bytes()),
            ])
        )
    }

    #[test]
    fn unquoted_argument() {
        assert_eq!(
            el_semicolon(&vec![
                "-b".as_bytes(),
                " echo".as_bytes(),
                "hi".as_bytes(),
                "".as_bytes(),
                "test".as_bytes(),
                "".as_bytes(),
            ]),
            Err(Error::UnquotedArgument(2))
        );
        assert_eq!(
            el_semicolon(&vec![
                " -b".as_bytes(),
                " echo".as_bytes(),
                "".as_bytes(),
                " test".as_bytes(),
                "a".as_bytes(),
            ]),
            Err(Error::UnquotedArgument(4))
        )
    }

    #[test]
    fn unterminated_block() {
        assert_eq!(
            el_semicolon(&vec![
                "-b".as_bytes(),
                " echo".as_bytes(),
            ]),
            Err(Error::UnterminatedBlock)
        )
    }
}

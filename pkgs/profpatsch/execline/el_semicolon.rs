
/// Every element inside the block starts with a space
const BLOCK_QUOTE_CHAR : u8 = b' ';
/// The end of a block is signified by an empty string
const BLOCK_END : &'static [u8] = &[];

/// A parsed execline argument.
#[derive(Debug, PartialEq, Eq)]
pub enum Arg<'a> {
    /// Normal argument.
    Arg(&'a [u8]),
    /// A block.
    ///
    /// On the command line a block is represented
    /// by a list of arguments which start with a space
    /// and end with an empty string.
    ///
    /// An empty block was just an empty string on its own.
    /// You will have to decide whether you want to treat
    /// it as a block or an empty string.
    Block(Vec<&'a [u8]>),
    /// The given argv list is empty
    EndOfArgv
}

#[derive(Debug, PartialEq, Eq)]
pub enum Error<'a> {
    /// The argument was not quoted, at index.
    UnquotedArgument(&'a [u8]),
    /// The last block was not terminated
    UnterminatedBlock,
}

enum ArgKind<'a> {
    // Reference to the argument without the block prefix
    BlockArg(&'a [u8]),
    // Matches BLOCK_END
    BlockEnd(&'a [u8]),
    // Same argument
    JustArg(&'a [u8])
}


/// Finds out whether an argument belongs to a block
/// or is just a normal argument.
fn what<'a>(arg: &'a [u8]) -> ArgKind<'a> {
    let arg = arg.as_ref();
    if arg == BLOCK_END {
        ArgKind::BlockEnd(arg)
    } else {
        match arg[0] {
            BLOCK_QUOTE_CHAR => ArgKind::BlockArg(&arg[1..]),
            _ => ArgKind::JustArg(arg)
        }
    }
}

/// Fetch one Arg from the given argv,
/// which is either a full block or one plain argument.
///
/// Returns the Arg and the unparsed rest of argv.
///
/// Blocks can be nested by adding more spaces,
/// but `el_semicolon` will only parse one level.
/// Usually that is intended, because nested blocks
/// are intended to be parsed by nested programs.
pub fn el_semicolon<'a>(args: &'a [&'a [u8]]) -> Result<(Arg<'a>, &'a [&'a [u8]]), Error<'a>> {
    let args = args.as_ref();
    let mut cur_block : Option<Vec<&'a [u8]>> = None;
    let mut res : Vec<Arg<'a>> = vec![];
    match args.first() {
        None => Ok((Arg::EndOfArgv, args)),
        Some(arg) => match what(arg) {
            ArgKind::BlockEnd(arg) => Ok((Arg::Arg(arg), &args[1..])),
            ArgKind::JustArg(arg) => Ok((Arg::Arg(arg), &args[1..])),
            ArgKind::BlockArg(arg) => {
                // if it’s a block, we have to repeatedly
                // fetch more args
                let mut block: Vec<&'a [u8]> = vec![arg];
                // we already looked at the 0th element
                let mut args = &args[1..];
                loop {
                    match args.first() {
                        None => break Err(Error::UnterminatedBlock),
                        Some(arg) => match what(arg) {
                            ArgKind::BlockEnd(_) => break Ok((Arg::Block(block), &args[1..])),
                            ArgKind::JustArg(arg) => break Err(Error::UnquotedArgument(arg)),
                            ArgKind::BlockArg(arg) => block.push(arg),
                        }
                    }
                    args = &args[1..];
                }
            }
        }
    }
}

pub fn el_semicolon_full_argv<'a>(args: &'a [&'a [u8]]) -> Result<Vec<Arg<'a>>, Error<'a>> {
    let mut res = vec![];
    let mut args = args.as_ref();
    loop {
        let (arg, rest) = match el_semicolon(args) {
            Ok((res, rest)) => (res, rest),
            Err(err) => break Err(err)
        };
        match arg {
            Arg::Arg(_) => res.push(arg),
            Arg::Block(_) => res.push(arg),
            Arg::EndOfArgv => break Ok(res),
        }
        args = &rest;
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn success() {
        assert_eq!(
            el_semicolon_full_argv(&vec![
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
            el_semicolon_full_argv(&vec![
                "-b".as_bytes(),
                " echo".as_bytes(),
                "hi".as_bytes(),
                "".as_bytes(),
                "test".as_bytes(),
                "".as_bytes(),
            ]),
            Err(Error::UnquotedArgument("hi".as_bytes()))
        );
        assert_eq!(
            el_semicolon_full_argv(&vec![
                " -b".as_bytes(),
                " echo".as_bytes(),
                "".as_bytes(),
                " test".as_bytes(),
                "a".as_bytes(),
            ]),
            Err(Error::UnquotedArgument("a".as_bytes()))
        )
    }

    #[test]
    fn unterminated_block() {
        assert_eq!(
            el_semicolon_full_argv(&vec![
                "-b".as_bytes(),
                " echo".as_bytes(),
            ]),
            Err(Error::UnterminatedBlock)
        )
    }
}

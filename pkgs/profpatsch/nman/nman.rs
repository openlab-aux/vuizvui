#[derive(Debug)]
enum DrvOutput<'a> {
    Out,
    Bin,
    Lib,
    Man,
    Dev,
    DevDoc,
    DevMan,
    Other(&'a [u8]),
}

#[derive(Debug)]
struct DrvWithOutput<'a> {
    drv_path: &'a [u8],
    output: DrvOutput<'a>,
}

fn parse_drv_path<'a>(path: &'a [u8]) -> Option<DrvWithOutput<'a>> {
    let mut split = path.split(|c| char::from(c.to_owned()) == '!');
    split.next().map(|p| DrvWithOutput {
        drv_path: p,
        output: split.next().map(|s| DrvOutput::Other(s))
                            .unwrap_or(DrvOutput::Out),
    }).and_then(|parsed| match split.next() {
        Some(_) => None,
        None => Some(parsed),
    })
}

fn main() -> std::io::Result<()> {
    println!("{:?}", parse_drv_path(b"/nix/store/58i9psln992xjwk8ig1v3l3a4p9sslnp-lowdown-0.7.9.drv"));
    Ok(())
}

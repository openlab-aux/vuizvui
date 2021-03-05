extern crate httparse;
extern crate netencode;

use std::os::unix::io::FromRawFd;
use std::io::Read;
use std::io::Write;

use netencode::{encode, U};

fn main() -> std::io::Result<()> {

    fn die<T: std::fmt::Display>(msg: T) {
        eprintln!("{}", msg);
        std::process::exit(1);
    }

    // max header size chosen arbitrarily
    let mut headers = [httparse::EMPTY_HEADER; 128];
    let mut resp = httparse::Response::new(&mut headers);
    let mut buf = vec![];
    std::io::stdin().read_to_end(&mut buf)?;
    let res = resp.parse(&mut buf);
    match res {
        Err(err) => die(format!("http response error: {:?}", err)),
        Ok(_start_of_body) => {
            match resp.code {
                Some(code) => write_dict(code, resp.reason, resp.headers)?,
                None => die(format!("no http status in {:?}", resp))
            }
        }
    };
    Ok(())
}

fn write_dict<'buf>(code: u16, reason: Option<&'buf str>, headers: &mut [httparse::Header<'buf>]) -> std::io::Result<()> {
    let mut http = vec![
        ("status", Box::new(U::N6(code as u64))),
    ];

    if let Some(t) = reason {
        http.push(("status-text", Box::new(U::Text(t.as_bytes()))))
    };

    let lowercase_headers = headers.iter_mut().map(
        |httparse::Header { name, value }|
        // lowercase the headers, since the standard doesn’t care
        // and we want unique strings to match agains
        (name.to_lowercase(), value)
    ).collect::<Vec<_>>();


    http.push(("headers", Box::new(U::Record(
        lowercase_headers.iter().map(
            |(name, value)|
            (name.as_str(), Box::new(U::Binary(value)))
        ).collect::<Vec<_>>()
    ))));

    encode(
        &mut std::io::stdout(),
        U::Record(http)
    )
}

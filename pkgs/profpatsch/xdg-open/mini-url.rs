extern crate el_exec;

use std::os::unix::ffi::OsStrExt;
use std::ffi::{CString};

#[derive(Clone, Debug, PartialEq, Eq,)]
struct Parsed<'a> {
    protocol: Protocol,
    domain: &'a str,
    port: &'a str,
    path: &'a str,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Protocol {
    Http,
    Https,
}

impl Protocol {
    fn string(&self) -> &str {
        match self {
            Protocol::Http => "http",
            Protocol::Https => "https"
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Error {
    NotHttp,
    ColonButNoPort
}

fn mini_parse_http(url: &str) -> Result<Parsed, Error> {
    let (protocol, off) = {
        if url.starts_with("https://") {
            (Protocol::Https, 8)
        } else if url.starts_with("http://") {
            (Protocol::Http, 7)
        } else {
            return Err(Error::NotHttp)
        }
    };

    let (_, path) = url.split_at(off);

    let (domain_port, path) = match path.find('/') {
        Some(boundary) => path.split_at(boundary),
        None => (path, "/")
    };

    let (domain, port) = match domain_port.find(':') {
        Some(boundary) => match domain_port.split_at(boundary) {
            (d, ":") => return Err(Error::ColonButNoPort),
            (d, p) => unsafe { (d, p.get_unchecked(1..)) }
        },
        None => match protocol {
            Protocol::Http => (domain_port, "80"),
            Protocol::Https => (domain_port, "443"),
        }
    };

    Ok(Parsed {
        protocol,
        domain,
        port,
        path
    })
}


fn main() -> std::io::Result<()> {
    let args = std::env::args().collect::<Vec<_>>();
    let prog_index = 2;
    if args.len() < prog_index {
        eprintln!("1st arg is http/https url");
        std::process::exit(100);
    }

    let Parsed { domain, port, protocol, path } = mini_parse_http(&args[1]).unwrap_or_else(
        |err| {
            eprintln!("could not parse url: {:?}", err);
            std::process::exit(100);
        }
    );

    let args = std::env::args_os().into_iter()
        .map(|arg| CString::new(arg.as_bytes()).unwrap())
        .collect::<Vec<_>>();

    std::env::set_var("host", domain);
    std::env::set_var("port", port);
    std::env::set_var("protocol", protocol.string());
    std::env::set_var("path", path);

    el_exec::xmexec0(&args[prog_index..]);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_mini_parse() {
        assert_eq!(mini_parse_http("https://abc.bar:8080/blabla"),
                   Ok(Parsed {
                       protocol: Protocol::Https,
                       domain: "abc.bar",
                       port: "8080",
                       path: "/blabla"
                   }));
        assert_eq!(mini_parse_http("http://abc.bar:8080/blabla"),
                   Ok(Parsed {
                       protocol: Protocol::Http,
                       domain: "abc.bar",
                       port: "8080",
                       path: "/blabla"
                   }));
        assert_eq!(mini_parse_http("http://abc.bar/blabla"),
                   Ok(Parsed {
                       protocol: Protocol::Http,
                       domain: "abc.bar",
                       port: "80",
                       path: "/blabla"
                   }));
        assert_eq!(mini_parse_http("https://f"),
                   Ok(Parsed {
                       protocol: Protocol::Https,
                       domain: "f",
                       port: "443",
                       path: "/"
                   }));
        assert_eq!(mini_parse_http("foo.bar"),
                   Err(Error::NotHttp));
        assert_eq!(mini_parse_http("mirror://abc.bar:8080/blabla"),
                   Err(Error::NotHttp));
        assert_eq!(mini_parse_http("http://abc.bar:"),
                   Err(Error::ColonButNoPort));
    }


}

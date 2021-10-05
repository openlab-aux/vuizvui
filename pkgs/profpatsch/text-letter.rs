//! Reads a toml file on stdin describing a simple letter format,
//! outputs a text formatting of the file, which can be piped to lp
//! and should be viable to use on A4 paper with one of these windowed envelopes.
//! Might depend on the printer.
//!
//! ```
//! cat letter.toml | ./text-letter | lp -h localhost -d printer-name -
//! ```
extern crate mustache;
extern crate toml;
extern crate regex;
extern crate lazy_static;


use std::io::{Read, Write};

struct Letter {
    current_city: String,
    from_address: String,
    to_address: String,
    content: String,
    date: Date
}

struct Date {
    year: String,
    month: String,
    day: String,
}


lazy_static::lazy_static!{
    static ref date_regex: regex::bytes::Regex = regex::bytes::Regex::new(r"^([0-9]{4})-([0-9]{2})-([0-9]{2})$").unwrap();
}

fn parse_date(s: &str) -> Option<Date> {
    date_regex.captures(s.as_bytes()).map(|c| Date {
        year: String::from_utf8(c.get(1).unwrap().as_bytes().to_vec()).unwrap(),
        month: String::from_utf8(c.get(2).unwrap().as_bytes().to_vec()).unwrap(),
        day: String::from_utf8(c.get(3).unwrap().as_bytes().to_vec()).unwrap(),
    })
}

fn parse_letter_toml(input: &str) -> Letter {
    let v : toml::Value = toml::from_str(input).expect("could not parse letter TOML");
    let get = |field: &str| -> &str {
        v.get(field).expect(&format!("field {} is missing in letter TOML", field))
          .as_str().expect(&format!("field {} must be a string", field))
    };
    assert_eq!(get("type"), "letter", "type must be `letter`");
    assert_eq!(get("version"), "0.0.1", "version must be `0.0.1`");

    Letter {
        current_city: get("current-city").to_owned(),
        from_address: get("from-address").trim_right().to_owned(),
        to_address: get("to-address").trim_right().to_owned(),
        content: get("content").trim_right().to_owned(),
        date: parse_date(get("date")).expect("field `date` needs to be a date like yyyy-mm-dd"),
    }
}

fn main() {
    let mut letter = String::new();
    std::io::stdin().read_to_string(&mut letter).expect("stdin has to be an utf-8 string");
    let letter = parse_letter_toml(&letter);
    let from_address_one_line = letter.from_address.replace("\n", ", ");

    fn indent4(s: String) -> String {
        let mut res = String::from("    ");
        res.push_str(&s.replace("\n", "\n    "));
        res
    }
    fn indent2(s: String) -> String {
        let mut res = String::from("  ");
        res.push_str(&s.replace("\n", "\n  "));
        res
    }

    let template = mustache::compile_str(r###"
{{{date}}}





{{{from_address_one_line}}}

{{{an}}}
{{{to_address}}}





{{content}}
"###
    ).expect("the template is malformed");


    let data : std::collections::HashMap<String, mustache::Data> =
        [("an", indent4("An:".to_string())),
         ("from_address_one_line", indent4(from_address_one_line)),
         ("to_address", indent4(letter.to_address)),
         ("content", letter.content),
         ("date", format!(
             "{}, den {}.{}.{}",
             letter.current_city,
             letter.date.day,
             letter.date.month,
             letter.date.year
         ))
        ][..].into_iter().cloned()
        .map(|(k,v)| (k.to_owned(), mustache::Data::String(v))).collect();
    let data = mustache::Data::Map(data);
    let res = template.render_data_to_string(
        &data
    ).expect("could not render template");

    // give a slight margin
    let res = indent2(res);

    std::io::stdout().write_all(&res.as_bytes()).unwrap()
}

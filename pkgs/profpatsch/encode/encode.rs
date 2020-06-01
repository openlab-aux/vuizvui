extern crate nom;

use nom::{
    IResult
};

use std::str::FromStr;
use std::ops::Neg;
use std::collections::HashMap;
use nom::bytes::complete::{tag, take};
use nom::branch::{alt};
use nom::character::complete::{digit1, char};
use nom::sequence::{tuple};
use nom::combinator::{map, map_res, flat_map, opt};
use nom::error::{context, ErrorKind, ParseError};

#[derive(Debug, PartialEq, Eq, Clone)]
struct Tag {
    tag: String,
    val: Box<T>
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum T {
    // Unit
    Unit,
    // Naturals
    N3(u8),
    N6(u64),
    N7(u128),
    // Integers
    I3(i8),
    I6(i64),
    I7(i128),
    // Text
    Text(String),
    // Tags
    Sum(Tag),
    Record(HashMap<String, Box<T>>),
    List(Box<Vec<T>>),
}

#[derive(Debug, PartialEq, Eq)]
enum Err {
    Empty,
    ParseLen,
}

fn unit_t(s: &[u8]) -> IResult<&[u8], ()> {
    let (s, _) = context("unit", tag("u,"))(s)?;
    Ok((s, ()))
}

fn usize_t(s: &[u8]) -> IResult<&[u8], usize> {
    context(
        "usize",
        map_res(
            map_res(digit1, |n| std::str::from_utf8(n)),
            |s| s.parse::<usize>())
    )(s)
}


fn uint_t<'a, I: FromStr + 'a>(t: &'static str) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], I> {
    move |s: &'a [u8]| {
        let (s, (_, _, int, _)) = tuple((
            tag(t.as_bytes()),
            char(':'),
            map_res(
                map_res(digit1, |n: &[u8]| std::str::from_utf8(n)),
                |s| s.parse::<I>()
            ),
            char(',')
        ))(s)?;
        Ok((s, int))
    }
}

fn int_t<'a, I: FromStr + Neg<Output=I>>(t: &'static str) -> impl Fn(&'a [u8]) -> IResult<&[u8], I> {
    context(
        t,
        move |s: &'a [u8]| {
            let (s, (_, _, neg, int, _)) = tuple((
                tag(t.as_bytes()),
                char(':'),
                opt(char('-')),
                map_res(
                    map_res(digit1, |n: &[u8]| std::str::from_utf8(n)),
                    |s| s.parse::<I>()
                ),
                char(',')
            ))(s)?;
            let res = match neg {
                Some(_) => -int,
                None => int,
            };
            Ok((s, res))
        }
    )
}

fn tag_t(s: &[u8]) -> IResult<&[u8], Tag> {
    let (s, (_, len, _)) = tuple((
        char('<'),
        usize_t,
        char(':'),
    ))(s)?;
    let (s, (tag, _, recurse)) = tuple((
        take(len),
        char('|'),
        // recurses into the main parser
        t_t
    ))(s)?;
    Ok((s, Tag {
        tag: std::str::from_utf8(tag)
            .map_err(|_| nom::Err::Failure((s, ErrorKind::Char)))
            .map(|s| s.to_string())?,
        val: Box::new(recurse)
    }))
}

/// parse text scalar (`t5:hello,`)
fn text(s: &[u8]) -> IResult<&[u8], T> {
    let (s, (_, len, _)) = tuple((
        char('t'),
        usize_t,
        char(':')
    ))(s)?;
    let (s, (res, _)) = tuple((
        take(len),
        char(',')
    ))(s)?;
    Ok((s, T::Text(
        std::str::from_utf8(res)
            .map_err(|_| nom::Err::Failure((s, ErrorKind::Char)))
            .map(|s| s.to_string())?,
    )))
}

fn list_t(s: &[u8]) -> IResult<&[u8], Vec<T>> {
    let (s, (_, vec, _)) = tuple((
        char('['),
        nom::multi::many0(t_t),
        char(']')
    ))(s)?;
    Ok((s, vec))
}

fn record_t(s: &[u8]) -> IResult<&[u8], HashMap<String, Box<T>>> {
    let (s, (_, map, _)) = tuple((
        char('{'),
        nom::multi::fold_many1(
            tag_t,
            HashMap::new(),
            |mut acc: HashMap<_, _>, Tag { tag, val }| {
                // ignore duplicated tag names that appear later
                if !acc.contains_key(&tag) {
                    acc.insert(tag, val);
                }
                acc
            }
        ),
        char('}')
    ))(s)?;
    Ok((s, map))
}

fn t_t(s: &[u8]) -> IResult<&[u8], T>  {
    alt((
        text,
        map(unit_t, |_| T::Unit),
        map(tag_t, |t| T::Sum(t)),
        map(list_t, |l| T::List(Box::new(l))),
        map(record_t, |p| T::Record(p)),

        // 8, 64 and 128 bit
        map(uint_t("n3"), |u| T::N3(u)),
        map(uint_t("n6"), |u| T::N6(u)),
        map(uint_t("n7"), |u| T::N7(u)),
        map(int_t("i3"), |u| T::I3(u)),
        map(int_t("i6"), |u| T::I6(u)),
        map(int_t("i7"), |u| T::I7(u)),

        // less common
        map(uint_t("n1"), |u| T::N3(u)),
        map(uint_t("n2"), |u| T::N3(u)),
        map(uint_t("n4"), |u| T::N6(u)),
        map(uint_t("n5"), |u| T::N6(u)),
        map(int_t("i1"), |u| T::I3(u)),
        map(int_t("i2"), |u| T::I3(u)),
        map(int_t("i4"), |u| T::I6(u)),
        map(int_t("i5"), |u| T::I6(u)),
        // TODO: 8, 9 not supported
    ))(s)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_unit_t() {
        assert_eq!(
            unit_t("u,".as_bytes()),
            Ok(("".as_bytes(), ()))
        );
    }

    #[test]
    fn test_parse_usize_t() {
        assert_eq!(
            usize_t("32foo".as_bytes()),
            Ok(("foo".as_bytes(), 32))
        );
    }

    #[test]
    fn test_parse_int_t() {
        assert_eq!(
            uint_t::<u8>("n3")("n3:42,abc".as_bytes()),
            Ok(("abc".as_bytes(), 42))
        );
        assert_eq!(
            uint_t::<u8>("n3")("n3:1024,abc".as_bytes()),
            Err(nom::Err::Error(("1024,abc".as_bytes(), nom::error::ErrorKind::MapRes)))
        );
        assert_eq!(
            int_t::<i64>("i6")("i6:-23,abc".as_bytes()),
            Ok(("abc".as_bytes(), -23))
        );
        assert_eq!(
            int_t::<i128>("i3")("i3:0,:abc".as_bytes()),
            Ok((":abc".as_bytes(), 0))
        );
        assert_eq!(
            uint_t::<u8>("n7")("n7:09,".as_bytes()),
            Ok(("".as_bytes(), 9))
        );
        // assert_eq!(
        //     length("c"),
        //     Err(nom::Err::Error(("c", nom::error::ErrorKind::Digit)))
        // );
        // assert_eq!(
        //     length(":"),
        //     Err(nom::Err::Error((":", nom::error::ErrorKind::Digit)))
        // );
    }

    #[test]
    fn test_parse_text() {
        assert_eq!(
            text("t5:hello,".as_bytes()),
            Ok(("".as_bytes(), T::Text("hello".to_owned())))
        );
        assert_eq!(
            text("t4:fo,".as_bytes()),
            // TODO: way better parse error messages
            Err(nom::Err::Error(("fo,".as_bytes(), nom::error::ErrorKind::Eof)))
        );
        assert_eq!(
            text("t9:今日は,".as_bytes()),
            Ok(("".as_bytes(), T::Text("今日は".to_owned())))
        );
    }

    #[test]
    fn test_list() {
        assert_eq!(
            list_t("[]".as_bytes()),
            Ok(("".as_bytes(), vec![]))
        );
        assert_eq!(
            list_t("[u,u,u,]".as_bytes()),
            Ok(("".as_bytes(), vec![
                T::Unit,
                T::Unit,
                T::Unit,
            ]))
        );
        assert_eq!(
            list_t("[u,[t3:foo,]u,]".as_bytes()),
            Ok(("".as_bytes(), vec![
                T::Unit,
                T::List(Box::new(vec![T::Text("foo".to_owned())])),
                T::Unit,
            ]))
        );
    }

    #[test]
    fn test_record() {
        assert_eq!(
            record_t("{<1:a|u,<1:b|u,<1:c|u,}".as_bytes()),
            Ok(("".as_bytes(), vec![
                ("a".to_owned(), Box::new(T::Unit)),
                ("b".to_owned(), Box::new(T::Unit)),
                ("c".to_owned(), Box::new(T::Unit)),
            ].into_iter().collect::<HashMap<String, Box<T>>>()))
        );
        // duplicated keys are ignored (first is taken)
        assert_eq!(
            record_t("{<1:a|u,<1:b|u,<1:a|i1:-1,}".as_bytes()),
            Ok(("".as_bytes(), vec![
                ("a".to_owned(), Box::new(T::Unit)),
                ("b".to_owned(), Box::new(T::Unit)),
            ].into_iter().collect::<HashMap<_,_>>()))
        );
    }

    #[test]
    fn test_parse() {
        assert_eq!(
            t_t("n3:255,".as_bytes()),
            Ok(("".as_bytes(), T::N3(255)))
        );
        assert_eq!(
            t_t("t6:halloo,".as_bytes()),
            Ok(("".as_bytes(), T::Text("halloo".to_owned())))
        );
        assert_eq!(
            t_t("<3:foo|t6:halloo,".as_bytes()),
            Ok(("".as_bytes(), T::Sum (Tag {
                tag: "foo".to_owned(),
                val: Box::new(T::Text("halloo".to_owned()))
            })))
        );
        // { a: Unit
        // , foo: List <A: Unit | B: List i3> }
        assert_eq!(
            t_t("{<1:a|u,<3:foo|[<1:A|u,<1:A|u,<1:B|[i3:127,]]}".as_bytes()),
            Ok(("".as_bytes(), T::Record(vec![
                ("a".to_owned(), Box::new(T::Unit)),
                ("foo".to_owned(), Box::new(T::List(Box::new(vec![
                    T::Sum(Tag { tag: "A".to_owned(), val: Box::new(T::Unit) }),
                    T::Sum(Tag { tag: "A".to_owned(), val: Box::new(T::Unit) }),
                    T::Sum(Tag { tag: "B".to_owned(), val: Box::new(T::List(Box::new(vec![T::I3(127)]))) }),
                ]))))
            ].into_iter().collect::<HashMap<String, Box<T>>>())))
        );
    }

}

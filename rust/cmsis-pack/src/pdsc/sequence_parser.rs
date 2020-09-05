use std::collections::HashMap;
use std::num::ParseIntError;
use std::str::FromStr;
use std::u64;

use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case},
    character::complete::{
        alpha1,
        alphanumeric1,
        char,
        digit1,
        hex_digit1,
        line_ending,
        multispace1,
        none_of,
    },
    combinator::{all_consuming, map, map_res, recognize, value},
    multi::{fold_many0, many0, many0_count, separated_list},
    sequence::{delimited, pair, preceded, terminated},
    IResult,
};

use crate::pdsc::sequence::*;

// C operator precedence:
//
// 1. Call
// 2. ! ~ + - (unary)
// 3. * / %
// 4. + - (binary)
// 5. << >>
// 6. < <= > >=
// 7. == !=
// 8. &
// 9. ^
// 10. |
// 11. &&
// 12. ||
// 13. ?: (ternary)

fn ws_or_comment(i: &str) -> IResult<&str, &str> {
    value(
        "",
        many0_count(
            alt((
                delimited(
                    tag("//"),
                    many0_count(none_of("\r\n")),
                    line_ending,
                ),
                delimited(
                    tag("/*"),
                    many0_count(none_of("*/")),
                    tag("* /")
                ),
                value(0, multispace1),
            )))
    )(i)
}

// terminated by whitespace or comment
fn tws<'a, O, F>(f: F) -> impl Fn(&'a str) -> IResult<&'a str, O>
where
    F: Fn(&'a str) -> IResult<&'a str, O>,
{
    terminated(f, ws_or_comment)
}

fn ident(i: &str) -> IResult<&str, &str> {
    recognize(
        pair(
            alt((alpha1, tag("_"))),
            many0_count(alt((alphanumeric1, tag("_"))))))(i)
}

fn parens(i: &str) -> IResult<&str, Expr> {
    delimited(tws(tag("(")), expr, tws(tag(")")))(i)
}

fn num(i: &str) -> IResult<&str, Num> {
    alt((
        map_res(
            tws(preceded(tag_no_case("0x"), hex_digit1)),
            |n: &str| Ok::<_, ParseIntError>(Num(u64::from_str_radix(n, 16)?, NumStyle::Hex))),
        map_res(
            tws(digit1),
            |n: &str| Ok::<_, ParseIntError>(Num(n.parse()?, NumStyle::Dec))),
    ))(i)
}

fn expr_num(i: &str) -> IResult<&str, Expr> {
    alt((
        map(num, Expr::Num),
        parens))(i)
}

fn expr_var(i: &str) -> IResult<&str, Expr> {
    alt((
        map(tws(ident), |name| Expr::Var(name.to_string())),
        expr_num,
    ))(i)
}

fn arg(i: &str) -> IResult<&str, Arg> {
    alt((
        map(
            tws(delimited(
                tag("\""),
                recognize(many0_count(none_of("\""))),
                tag("\""))),
            |s: &str| Arg::String(s.to_string())),
        map(expr, Arg::Expr)
    ))(i)
}

fn expr_call(i: &str) -> IResult<&str, Expr> {
    alt((
        map_res(
            pair(
                tws(ident),
                delimited(
                    tws(tag("(")),
                    separated_list(tws(tag(",")), arg),
                    tws(tag(")")))),
            |(name, args): (&str, Vec<Arg>)| {
                Ok::<_, ()>(Expr::Call(name.to_string(), args))
            }),
        expr_var,
    ))(i)
}

fn expr_unary(i: &str) -> IResult<&str, Expr> {
    alt((
        map_res(
            pair(
                tws(alt((tag("~"), tag("!"), tag("+"), tag("-")))),
                expr_unary),
            |(op, expr): (&str, Expr)| {
                Ok::<_, ()>(Expr::Unary(UnOp::from_str(op).expect("unreachable"), Box::new(expr)))
            }),
            expr_call,
    ))(i)
}

fn expr_binary(i: &str) -> IResult<&str, Expr> {
   fn bin<'a>(
        i: &'a str,
        next: impl Fn(&'a str) -> IResult<&str, Expr>,
        parse_op: impl Fn(&'a str) -> IResult<&'a str, &'a str>)
            -> IResult<&'a str, Expr>
    {
        let (i, init) = next(i)?;

        fold_many0(
            pair(tws(parse_op), next),
            init,
            |prev, (op, expr): (&str, Expr)| {
                Expr::Binary(BinOp::from_str(op).expect("unreachable"), Box::new((prev, expr)))
            },
        )(i)
    }

    // Precedence is defined by this chain.
    fn p1(i: &str) -> IResult<&str, Expr> { bin(i, expr_unary, alt((tag("*"), tag("/"), tag("%")))) }
    fn p2(i: &str) -> IResult<&str, Expr> { bin(i, p1, alt((tag("+"), tag("-")))) }
    fn p3(i: &str) -> IResult<&str, Expr> { bin(i, p2, alt((tag("<<"), tag(">>")))) }
    fn p4(i: &str) -> IResult<&str, Expr> { bin(i, p3, alt((tag("<="), tag(">="), tag("<"), tag(">")))) }
    fn p5(i: &str) -> IResult<&str, Expr> { bin(i, p4, alt((tag("=="), tag("!=")))) }
    fn p6(i: &str) -> IResult<&str, Expr> { bin(i, p5, tag("&")) }
    fn p7(i: &str) -> IResult<&str, Expr> { bin(i, p6, tag("^")) }
    fn p8(i: &str) -> IResult<&str, Expr> { bin(i, p7, tag("|")) }
    fn p9(i: &str) -> IResult<&str, Expr> { bin(i, p8, tag("&&")) }
    fn p10(i: &str) -> IResult<&str, Expr> { bin(i, p9, tag("||")) }

    p10(i)
}

fn expr_ternary(i: &str) -> IResult<&str, Expr> {
    let (i, init) = expr_binary(i)?;

    fold_many0(
        pair(preceded(tws(char('?')), expr_binary), preceded(tws(char(':')), expr_ternary)),
        init,
        |cond_expr, (then_expr, else_expr): (Expr, Expr)| {
            Expr::Cond(Box::new((cond_expr, then_expr, else_expr)))
        },
    )(i)
}

fn expr(i: &str) -> IResult<&str, Expr> {
    tws(expr_ternary)(i)
}

fn stmt(i: &str) -> IResult<&str, Stmt> {
    terminated(
        alt((
            map(
                pair(
                    preceded(
                        tws(terminated(tag("__var"), multispace1)),
                        tws(ident)),
                    preceded(
                        tws(char('=')),
                        expr)),
                |(name, value)| Stmt::Declare(name.to_string(), value)),
            map(
                pair(
                    tws(ident),
                    preceded(
                        tws(char('=')),
                        expr)),
                |(name, value)| Stmt::Assign(name.to_string(), value)),
            map(expr, Stmt::Expr),
        )),
        tws(char(';')))(i)
}

pub fn parse_expr(i: &str) -> IResult<&str, Expr> {
    all_consuming(preceded(ws_or_comment, expr))(i)
}

pub fn parse_stmts(i: &str) -> IResult<&str, Vec<Stmt>> {
    all_consuming(preceded(ws_or_comment, many0(stmt)))(i)
}

pub fn parse_debug_vars(i: &str) -> IResult<&str, HashMap<String, Num>> {
    all_consuming(
        preceded(
            ws_or_comment,
            fold_many0(
                terminated(
                    pair(
                        preceded(
                            tws(terminated(tag("__var"), multispace1)),
                            tws(ident)),
                        preceded(
                            tws(char('=')),
                            num)),
                    tws(char(';'))),
                HashMap::new(),
                |mut map, (name, value)| {
                    map.insert(name.to_string(), value);
                    map
                })))(i)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_expr() {
        // TODO: real tests

        parse_expr("1 * 2 + 3").unwrap();
        parse_expr("1 * (2 + 3)").unwrap();
        parse_expr("1 ? 2 : 3 ? 4 : 5").unwrap();

        let code = r#"
        __var nReset      = 0;

        // pollo
        canReadPins = (DAP_SWJ_Pins(0x00, nReset, 0) != 0xFFFFFFFF);"#;

        parse_stmts(code).unwrap();
    }
}
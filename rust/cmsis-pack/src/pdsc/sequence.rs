use std::str::FromStr;

use failure::{format_err, Error};
use minidom::Element;
use serde::{Deserialize, Serialize};

use crate::utils::prelude::*;
use crate::pdsc::sequence_parser;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Sequence {
    name: String,
    pname: Option<String>,
    disable: bool,
    info: Option<String>,
    body: Vec<Struct>,
}

impl FromElem for Sequence {
    fn from_elem(e: &Element) -> Result<Self, Error> {
        Ok(Sequence {
            name: attr_parse(e, "name", "sequence")?,
            pname: attr_parse(e, "Pname", "sequence").ok(),
            disable: attr_parse(e, "disable", "sequence").unwrap_or(false),
            info: attr_parse(e, "info", "sequence").ok(),
            body: e
                .children()
                .map(|child| Struct::from_elem(child))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Struct {
    Control(Control),
    Block(Block),
}

impl FromElem for Struct {
    fn from_elem(e: &Element) -> Result<Self, Error> {
        match e.name() {
            "control" => Ok(Struct::Control(Control::from_elem(e)?)),
            "block" => Ok(Struct::Block(Block::from_elem(e)?)),
            unknown => Err(format_err!("Unknown sequence element {}", unknown)),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Control {
    if_cond: Option<Expr>,
    while_cond: Option<Expr>,
    timeout: Option<u64>,
    info: Option<String>,
    body: Vec<Struct>,
}

impl FromElem for Control {
    fn from_elem(e: &Element) -> Result<Self, Error> {
        Ok(Control {
            if_cond: attr_parse(e, "if", "control").ok(),
            while_cond: attr_parse(e, "while", "control").ok(),
            timeout: attr_parse(e, "timeout", "control").ok(),
            info: attr_parse(e, "info", "control").ok(),
            body: e
                .children()
                .map(|child| Struct::from_elem(child))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Block {
    atomic: bool,
    info: Option<String>,
    body: Vec<Stmt>,
}

impl FromElem for Block {
    fn from_elem(e: &Element) -> Result<Self, Error> {
        Ok(Block {
            atomic: attr_parse(e, "atomic", "block").unwrap_or(false),
            info: attr_parse(e, "body", "control").ok(),
            body: Stmt::parse(&e.text())?,
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Stmt {
    Declare(String, Expr),
    Assign(String, Expr),
    Expr(Expr),
}

impl Stmt {
    fn parse(from: &str) -> Result<Vec<Self>, Error> {
        sequence_parser::parse_stmts(from)
            .map(|res| res.1)
            .map_err(|err| format_err!("block parse error: {}", err))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Expr {
    Num(u64, NumStyle),
    Var(String),
    Call(String, Vec<Arg>),
    Unary(UnOp, Box<Expr>),
    Binary(BinOp, Box<(Expr, Expr)>),
    Cond(Box<(Expr, Expr, Expr)>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum NumStyle {
    Dec,
    Hex,
}

impl FromStr for Expr {
    type Err = Error;
    fn from_str(from: &str) -> Result<Self, Error> {
        sequence_parser::parse_expr(from)
            .map(|res| res.1)
            .map_err(|err| format_err!("expression parse error: {}", err))
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum UnOp {
    BitNot,
    Not,
    Pos,
    Neg,
}

impl FromStr for UnOp {
    type Err = Error;
    fn from_str(from: &str) -> Result<Self, Error> {
        match from {
            "~" => Ok(UnOp::BitNot),
            "!" => Ok(UnOp::Not),
            "+" => Ok(UnOp::Pos),
            "-" => Ok(UnOp::Neg),
            unknown => Err(format_err!("unknown unary operator {}", unknown)),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    BitXor,
    BitAnd,
    BitOr,
    Shr,
    Shl,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
}

impl FromStr for BinOp {
    type Err = Error;
    fn from_str(from: &str) -> Result<Self, Error> {
        match from {
            "+" => Ok(BinOp::Add),
            "-" => Ok(BinOp::Sub),
            "*" => Ok(BinOp::Mul),
            "/" => Ok(BinOp::Div),
            "%" => Ok(BinOp::Rem),
            "&&" => Ok(BinOp::And),
            "||" => Ok(BinOp::Or),
            "^" => Ok(BinOp::BitXor),
            "&" => Ok(BinOp::BitAnd),
            "|" => Ok(BinOp::BitOr),
            ">>" => Ok(BinOp::Shr),
            "<<" => Ok(BinOp::Shl),
            "==" => Ok(BinOp::Eq),
            "<" => Ok(BinOp::Lt),
            "<=" => Ok(BinOp::Le),
            "!=" => Ok(BinOp::Ne),
            ">=" => Ok(BinOp::Ge),
            ">" => Ok(BinOp::Gt),
            unknown => Err(format_err!("unknown binary operator {}", unknown)),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Arg {
    Expr(Expr),
    String(String),
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parser() {
        // Sample sequence from CMSIS-Pack documentation
        let test_xml = r#"
            <sequence name="ResetHardware">
                <block>
                    __var nReset      = 0x80;
                    __var canReadPins = 0;
                    // De-assert nRESET line
                    canReadPins = (DAP_SWJ_Pins(0x00, nReset, 0) != 0xFFFFFFFF);
                </block>
                <!-- Keep reset active for 50 ms -->
                <control while="1" timeout="50000"/>
                <control if="canReadPins">
                    <!-- Assert nRESET line and wait max. 1s for recovery -->
                    <control while="(DAP_SWJ_Pins(nReset, nReset, 0) &amp; nReset) == 0" timeout="1000000"/>
                </control>
                <control if="!canReadPins">
                    <block>
                        // Assert nRESET line
                        DAP_SWJ_Pins(nReset, nReset, 0);
                    </block>
                    <!-- Wait 100ms for recovery if nRESET not readable -->
                    <control while="1" timeout="100000"/>
                </control>
            </sequence>
        "#;

        // TODO: Real tests
        Sequence::from_string(test_xml).unwrap();
    }
}
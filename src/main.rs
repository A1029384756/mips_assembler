mod parse_tests;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till},
    character::complete::{alphanumeric1, i64, multispace0, multispace1},
    error::{Error, ErrorKind, ParseError},
    sequence::{terminated, tuple},
    Err, IResult,
};

fn main() {
    println!("Hello, world!");
}

fn parse_mem_decl(input: &str) -> IResult<&str, i64> {
    let (i, (t, _, val)) = alt((
        tuple((tag(".word"), multispace1, i64)),
        tuple((tag(".half"), multispace1, i64)),
        tuple((tag(".byte"), multispace1, i64)),
        tuple((tag(".space"), multispace1, i64)),
    ))(input)?;

    match t {
        ".word" => {
            if valid_int_size(val, 32) {
                return Ok((i, val));
            }
        }
        ".half" => {
            if valid_int_size(val, 16) {
                return Ok((i, val));
            }
        }
        ".byte" => {
            if valid_int_size(val, 8) {
                return Ok((i, val));
            }
        }
        ".space" => {
            if u32::try_from(val).is_ok() {
                return Ok((i, val));
            }
        }
        _ => {}
    };

    Err(Err::Error(Error::from_error_kind(input, ErrorKind::IsNot)))
}

fn parse_const_decl(input: &str) -> IResult<&str, (&str, i64)> {
    let (i, (name, _, _, _, val)) =
        tuple((parse_var_name, multispace0, tag("="), multispace0, i64))(input)?;

    if get_msb(val) <= 32 {
        Ok((i, (name, val)))
    } else {
        Err(Err::Error(Error::from_error_kind(input, ErrorKind::IsNot)))
    }
}

fn parse_label_decl(input: &str) -> IResult<&str, &str> {
    terminated(parse_var_name, tag(":"))(input)
}

fn parse_var_name(input: &str) -> IResult<&str, &str> {
    let (i, str) = alphanumeric1(input)?;
    if str.chars().next().unwrap().is_alphabetic() {
        dbg!("{}", i);
        Ok((i, str))
    } else {
        Err(Err::Error(Error::from_error_kind(input, ErrorKind::IsNot)))
    }
}

fn parse_opcode(input: &str) -> IResult<&str, (u32, u32)> {
    let (i, op) = take_till(|c: char| c.is_whitespace())(input)?;

    match op {
        "add" => Ok((i, (0x00, 0x20))),
        "addi" => Ok((i, (0x08, 0x00))),
        "addiu" => Ok((i, (0x09, 0x00))),
        "addu" => Ok((i, (0x00, 0x21))),
        "and" => Ok((i, (0x00, 0x24))),
        "andi" => Ok((i, (0x0C, 0x00))),
        "beq" => Ok((i, (0x04, 0x00))),
        "bne" => Ok((i, (0x05, 0x00))),
        "j" => Ok((i, (0x02, 0x00))),
        "jal" => Ok((i, (0x03, 0x00))),
        "jr" => Ok((i, (0x00, 0x08))),
        "lbu" => Ok((i, (0x24, 0x00))),
        "lhu" => Ok((i, (0x25, 0x00))),
        "ll" => Ok((i, (0x30, 0x00))),
        "lui" => Ok((i, (0x0F, 0x00))),
        "lw" => Ok((i, (0x23, 0x00))),
        "nor" => Ok((i, (0x00, 0x27))),
        "or" => Ok((i, (0x00, 0x25))),
        "ori" => Ok((i, (0x0D, 0x00))),
        "slt" => Ok((i, (0x00, 0x2A))),
        "stli" => Ok((i, (0x0A, 0x00))),
        "stliu" => Ok((i, (0x0B, 0x00))),
        "stlu" => Ok((i, (0x00, 0x2B))),
        "sll" => Ok((i, (0x00, 0x00))),
        "srl" => Ok((i, (0x00, 0x02))),
        "sb" => Ok((i, (0x28, 0x00))),
        "sc" => Ok((i, (0x38, 0x00))),
        "sh" => Ok((i, (0x29, 0x00))),
        "sw" => Ok((i, (0x2B, 0x00))),
        "sub" => Ok((i, (0x00, 0x22))),
        "subu" => Ok((i, (0x00, 0x23))),
        _ => Err(Err::Error(Error::from_error_kind(input, ErrorKind::IsNot))),
    }
}

fn parse_register(input: &str) -> IResult<&str, u32> {
    let (i, op) = take_till(|c: char| c.is_whitespace())(input)?;

    match op {
        "$0" | "$zero" => Ok((i, 0)),
        "$1" | "$at" => Ok((i, 1)),
        "$2" | "$v0" => Ok((i, 2)),
        "$3" | "$v1" => Ok((i, 3)),
        "$4" | "$a0" => Ok((i, 4)),
        "$5" | "$a1" => Ok((i, 5)),
        "$6" | "$a2" => Ok((i, 6)),
        "$7" | "$a3" => Ok((i, 7)),
        "$8" | "$t0" => Ok((i, 8)),
        "$9" | "$t1" => Ok((i, 9)),
        "$10" | "$t2" => Ok((i, 10)),
        "$11" | "$t3" => Ok((i, 11)),
        "$12" | "$t4" => Ok((i, 12)),
        "$13" | "$t5" => Ok((i, 13)),
        "$14" | "$t6" => Ok((i, 14)),
        "$15" | "$t7" => Ok((i, 15)),
        "$16" | "$s0" => Ok((i, 16)),
        "$17" | "$s1" => Ok((i, 17)),
        "$18" | "$s2" => Ok((i, 18)),
        "$19" | "$s3" => Ok((i, 19)),
        "$20" | "$s4" => Ok((i, 20)),
        "$21" | "$s5" => Ok((i, 21)),
        "$22" | "$s6" => Ok((i, 22)),
        "$23" | "$s7" => Ok((i, 23)),
        "$24" | "$t8" => Ok((i, 24)),
        "$25" | "$t9" => Ok((i, 25)),
        "$26" | "$k0" => Ok((i, 26)),
        "$27" | "$k1" => Ok((i, 27)),
        "$28" | "$gp" => Ok((i, 28)),
        "$29" | "$sp" => Ok((i, 29)),
        "$30" | "$fp" => Ok((i, 30)),
        "$31" | "$ra" => Ok((i, 31)),
        _ => Err(Err::Error(Error::from_error_kind(input, ErrorKind::IsNot))),
    }
}

fn valid_int_size(val: i64, size: i64) -> bool {
    dbg!(val);
    dbg!(get_msb(val));
    dbg!(get_msb(val) <= size);
    get_msb(val) < size
}

fn get_msb(n: i64) -> i64 {
    for i in (0..(std::mem::size_of::<i64>() * 8)).rev() {
        if (n >> i) & 1 == 1 {
            return i as i64;
        }
    }

    0
}

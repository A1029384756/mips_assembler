mod parse_tests;
use std::collections::HashMap;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alphanumeric1, i64, multispace0, multispace1},
    error::{Error, ErrorKind, ParseError},
    multi::many0,
    sequence::{terminated, tuple},
    Err, Finish, IResult,
};

pub fn parse(i: &str) -> Result<(Vec<u32>, Vec<u8>), String> {
    let preprocessed = preprocess(i)?;
    match parse_asm(&preprocessed).finish() {
        Ok(asm) => match assemble(&asm.1) {
            Ok(out) => Ok(out),
            Result::Err(e) => Err(e),
        },
        Result::Err(e) => Err(e.input.to_string()),
    }
}

fn preprocess(i: &str) -> Result<String, String> {
    let out = i
        .lines()
        .filter_map(|line| {
            if line.starts_with('#') {
                None
            } else {
                match line.split_once('#') {
                    Some((l, _)) => Some(l.to_string() + "\n"),
                    None => Some(line.to_string() + "\n"),
                }
            }
        })
        .reduce(|acc, e| acc + &e)
        .ok_or("File empty")?;

    Ok(out)
}

fn assemble(i: &Vec<Section>) -> Result<(Vec<u32>, Vec<u8>), String> {
    let data = i
        .into_iter()
        .filter_map(|e| match e {
            Section::Data(data) => Some(data.clone()),
            Section::Text(_) => None,
        })
        .reduce(|mut acc, e| {
            acc.extend(e);
            acc
        })
        .ok_or("Data section does not exist")?;

    let mut data_mem: Vec<u8> = Vec::new();
    let mut mem_back_ptr: i64 = 0;
    let mut mem_labels: HashMap<String, i64> = HashMap::new();
    let mut constants: HashMap<String, i64> = HashMap::new();
    data.iter().for_each(|elem| {
        match elem {
            Declaration::Allocation(a) => match a {
                Allocation::Value(num, size) => {
                    (*num as u32).to_be_bytes().iter().for_each(|e| {
                        data_mem.push(*e);
                    });
                    mem_back_ptr += size;
                }
                Allocation::Space(size) => mem_back_ptr += size,
            },
            Declaration::Label(l) => {
                mem_labels.insert(l.clone(), mem_back_ptr);
            }
            Declaration::Constant(c) => {
                constants.insert(c.0.clone(), c.1);
            }
        };
    });

    let lines = i
        .into_iter()
        .filter_map(|e| match e {
            Section::Data(_) => None,
            Section::Text(t) => Some(t.clone()),
        })
        .reduce(|mut acc, e| {
            acc.extend(e);
            acc
        })
        .ok_or("Text section does not exist")?;

    let mut insts: Vec<Instruction> = Vec::new();
    let mut inst_labels: HashMap<String, i64> = HashMap::new();
    let mut curr_line: i64 = 0;
    for e in lines.iter() {
        match e {
            Line::Declaration(d) => {
                match d {
                    Declaration::Label(l) => inst_labels.insert(l.to_owned(), curr_line * 4),
                    _ => return Err(format!("Out of place declaration: `{:#?}`", d)),
                };
            }
            Line::Instruction(e) => {
                insts.push(e.clone());
                curr_line += 1;
            }
        }
    }

    if let None = inst_labels.get("main") {
        return Err("Missing main label".to_string());
    }

    let mut assembled_insts: Vec<u32> = Vec::new();
    for inst in insts.iter() {
        let mut assembled = 0;
        match inst {
            Instruction::ArithLogic(op, rd, rs, rt) => {
                assembled += rs << 21;
                assembled += rt << 16;
                assembled += rd << 11;
                assembled += op.1;
            }
            Instruction::ArithLogicImm(op, rt, rs, imm) => {
                assembled += op.0 << 26;
                assembled += rs << 21;
                assembled += rt << 16;
                match imm {
                    ImmArg::Immediate(imm) => {
                        assembled += (*imm as u16) as i64;
                    }
                    ImmArg::Constant(c) => match constants.get(c) {
                        Some(v) => {
                            assembled += (*v as u16) as i64;
                        }
                        None => return Err(format!("Invalid constant name `{c}`")),
                    },
                }
            }
            Instruction::Shift(op, rd, rt, shamt) => {
                assembled += op.0 << 26;
                assembled += rt << 16;
                assembled += rd << 11;
                match shamt {
                    ImmArg::Immediate(shamt) => {
                        assembled += shamt << 6;
                    }
                    ImmArg::Constant(c) => match constants.get(c) {
                        Some(v) => {
                            assembled += v << 6;
                        }
                        None => return Err(format!("Invalid constant name `{c}`")),
                    },
                }
            }
            Instruction::LoadStore(op, rt, memref) => {
                assembled += op.0 << 26;
                match memref.1.clone() {
                    MemLoc::Register(rs) => {
                        assembled += rs << 21;
                        assembled += rt << 16;
                        assembled += memref.0;
                    }
                    MemLoc::Label(l) => match mem_labels.get(&l) {
                        Some(addr) => {
                            // lui $1, 4097 - loads the data section into register $at
                            assembled_insts.push(0x3c011001);

                            // Performs load
                            assembled += 1 << 21;
                            assembled += rt << 16;
                            assembled += addr + memref.0;
                        }
                        None => return Err(format!("Invalid label name `{l}`")),
                    },
                    MemLoc::Immediate(imm) => {
                        // lui $1, 4097 - loads the data section into register $at
                        assembled_insts.push(0x3c011001);

                        assembled += 1 << 21;
                        assembled += rt << 16;
                        assembled += imm;
                    }
                };
                assembled += (memref.0 as u16) as i64;
            }
            Instruction::Branch(op, rs, rt, memref) => match memref.1.clone() {
                MemLoc::Label(l) => match inst_labels.get(&l) {
                    Some(line) => {
                        let offset = line - (assembled_insts.len() as i64);

                        assembled += op.0 << 26;
                        assembled += rs << 21;
                        assembled += rt << 16;
                        assembled += (offset as u16) as i64;
                    }
                    None => return Err(format!("Invalid label name `{l}`")),
                },
                _ => return Err(format!("Invalid branch destination: `{:#?}`, must use label", memref.1)),
            },
            Instruction::Lui(op, rt, imm) => {
                assembled += op.0 << 26;
                assembled += rt << 16;
                match imm {
                    ImmArg::Immediate(imm) => {
                        assembled += (*imm as u16) as i64;
                    }
                    ImmArg::Constant(c) => match constants.get(c) {
                        Some(v) => {
                            assembled += (*v as u16) as i64;
                        }
                        None => return Err(format!("Invalid constant name `{c}`")),
                    },
                }
            }
            Instruction::Jump(op, memref) => match memref.1.clone() {
                MemLoc::Label(l) => match inst_labels.get(&l) {
                    Some(line) => {
                        let offset = line - (inst_labels.len() as i64);
                        assembled += op.0 << 26;
                        assembled += offset;
                    }
                    None => return Err(format!("Invalid label name `{l}`")),
                },
                _ => return Err(format!("Invalid jump destination: `{:#?}`, must use label", memref.1)),
            },
            Instruction::Jr(op, rs) => {
                assembled += op.0 << 26;
                assembled += rs << 21;
                assembled += op.1;
            }
        };
        assembled_insts.push(assembled as u32);
    }

    Ok((assembled_insts, data_mem))
}

#[derive(Debug, Clone)]
enum Section {
    Data(Vec<Declaration>),
    Text(Vec<Line>),
}

// ------------------ Data Section Types ------------------
#[derive(Debug, Clone)]
enum Declaration {
    Allocation(Allocation),
    Label(Label),
    Constant(Constant),
}

#[derive(Debug, Clone)]
enum Allocation {
    Value(i64, i64),
    Space(i64),
}

type Label = String;
type Constant = (String, i64);

// ------------------ Text Section Types ------------------
#[derive(Debug, Clone)]
enum Line {
    Declaration(Declaration),
    Instruction(Instruction),
}

#[derive(Debug, Clone)]
enum Instruction {
    ArithLogic(Operation, Register, Register, Register),
    ArithLogicImm(Operation, Register, Register, ImmArg),
    Shift(Operation, Register, Register, ImmArg),
    LoadStore(Operation, Register, MemRef),
    Branch(Operation, Register, Register, MemRef),
    Lui(Operation, Register, ImmArg),
    Jump(Operation, MemRef),
    Jr(Operation, Register),
}

type MemRef = (i64, MemLoc);
type Operation = (i64, i64);
type Register = i64;

#[derive(Debug, Clone)]
enum MemLoc {
    Register(Register),
    Label(String),
    Immediate(i64),
}

#[derive(Debug, Clone)]
enum ImmArg {
    Immediate(i64),
    Constant(String),
}

fn parse_asm(input: &str) -> IResult<&str, Vec<Section>> {
    let (i, (_, sections, _)) = tuple((
        multispace0,
        many0(tuple((
            multispace0,
            alt((parse_data_section, parse_text_section)),
        ))),
        multispace0,
    ))(input)?;

    if !i.is_empty() {
        Err(Err::Error(Error::from_error_kind(
            input,
            ErrorKind::Satisfy,
        )))
    } else {
        Ok((i, sections.into_iter().map(|e| e.1).collect()))
    }
}

// ------------------ Section Parsing ------------------
fn parse_data_section(input: &str) -> IResult<&str, Section> {
    let (i, (_, decls)) = tuple((
        tag(".data"),
        many0(tuple((
            multispace1,
            alt((parse_mem_decl, parse_const_decl, parse_label_decl)),
        ))),
    ))(input)?;

    Ok((i, Section::Data(decls.into_iter().map(|e| e.1).collect())))
}

fn parse_text_section(input: &str) -> IResult<&str, Section> {
    let (i, (_, instructions)) = tuple((
        tag(".text"),
        many0(tuple((
            multispace1,
            alt((
                parse_jr,
                parse_jump,
                parse_lui,
                parse_branch,
                parse_load_store,
                parse_shift,
                parse_arith_log,
                parse_arith_log_imm,
                parse_label_decl_data,
            )),
        ))),
    ))(input)?;

    Ok((
        i,
        Section::Text(instructions.into_iter().map(|e| e.1).collect()),
    ))
}

fn parse_label_decl_data(input: &str) -> IResult<&str, Line> {
    let (i, label) = parse_label_decl(input)?;
    Ok((i, Line::Declaration(label)))
}

// ------------------ ASM Data Declarations ------------------
fn parse_mem_decl(input: &str) -> IResult<&str, Declaration> {
    let (i, (t, _, val)) = alt((
        tuple((tag(".word"), multispace1, i64)),
        tuple((tag(".half"), multispace1, i64)),
        tuple((tag(".byte"), multispace1, i64)),
        tuple((tag(".space"), multispace1, i64)),
    ))(input)?;

    match t {
        ".word" => {
            if valid_int_size(val, 32) {
                return Ok((i, Declaration::Allocation(Allocation::Value(val, 4))));
            }
        }
        ".half" => {
            if valid_int_size(val, 16) {
                return Ok((i, Declaration::Allocation(Allocation::Value(val, 2))));
            }
        }
        ".byte" => {
            if valid_int_size(val, 8) {
                return Ok((i, Declaration::Allocation(Allocation::Value(val, 1))));
            }
        }
        ".space" => {
            if u32::try_from(val).is_ok() {
                return Ok((i, Declaration::Allocation(Allocation::Space(val))));
            }
        }
        _ => {}
    };

    Err(Err::Error(Error::from_error_kind(input, ErrorKind::IsNot)))
}

fn parse_const_decl(input: &str) -> IResult<&str, Declaration> {
    let (i, (name, _, _, _, val)) =
        tuple((parse_var, multispace0, tag("="), multispace0, i64))(input)?;

    if valid_int_size(val, 32) {
        Ok((i, Declaration::Constant((name.to_string(), val))))
    } else {
        Err(Err::Error(Error::from_error_kind(input, ErrorKind::IsNot)))
    }
}

fn parse_label_decl(input: &str) -> IResult<&str, Declaration> {
    let (i, label) = terminated(parse_var, tag(":"))(input)?;
    Ok((i, Declaration::Label(label.to_string())))
}

// ------------------ General Purpose Parsers ------------------
fn parse_var(input: &str) -> IResult<&str, &str> {
    let (i, str) = alphanumeric1(input)?;
    if str.chars().next().unwrap().is_alphabetic() {
        Ok((i, str))
    } else {
        Err(Err::Error(Error::from_error_kind(input, ErrorKind::IsNot)))
    }
}

// ------------------ Instruction Parsers ------------------
fn parse_jr(input: &str) -> IResult<&str, Line> {
    let (i, (op, _, reg)) = tuple((tag("jr"), multispace1, parse_register))(input)?;

    Ok((
        i,
        Line::Instruction(Instruction::Jr(op_to_opcode_func(op), reg)),
    ))
}

fn parse_jump(input: &str) -> IResult<&str, Line> {
    let (i, (op, _, mem)) = tuple((alt((tag("j"), tag("jal"))), multispace1, parse_memref))(input)?;

    Ok((
        i,
        Line::Instruction(Instruction::Jump(op_to_opcode_func(op), mem)),
    ))
}

fn parse_lui(input: &str) -> IResult<&str, Line> {
    let (i, (op, _, rd, _, _, imm)) = tuple((
        tag("lui"),
        multispace1,
        parse_register,
        tag(","),
        multispace1,
        parse_immarg,
    ))(input)?;

    match imm {
        ImmArg::Immediate(im) => {
            if valid_int_size(im, 16) {
                Ok((
                    i,
                    Line::Instruction(Instruction::Lui(op_to_opcode_func(op), rd, imm)),
                ))
            } else {
                Err(Err::Error(Error::from_error_kind(input, ErrorKind::IsNot)))
            }
        }
        ImmArg::Constant(_) => Ok((
            i,
            Line::Instruction(Instruction::Lui(op_to_opcode_func(op), rd, imm)),
        )),
    }
}

fn parse_branch(input: &str) -> IResult<&str, Line> {
    let (i, (op, _, rt, _, _, rs, _, _, mr)) = tuple((
        alt((tag("beq"), tag("bne"))),
        multispace1,
        parse_register,
        tag(","),
        multispace1,
        parse_register,
        tag(","),
        multispace1,
        parse_memref,
    ))(input)?;

    Ok((
        i,
        Line::Instruction(Instruction::Branch(op_to_opcode_func(op), rt, rs, mr)),
    ))
}

fn parse_load_store(input: &str) -> IResult<&str, Line> {
    let (i, (op, _, rt, _, _, mr)) = tuple((
        alt((
            tag("lbu"),
            tag("lhu"),
            tag("ll"),
            tag("lw"),
            tag("sb"),
            tag("sc"),
            tag("sh"),
            tag("sw"),
        )),
        multispace1,
        parse_register,
        tag(","),
        multispace1,
        parse_memref,
    ))(input)?;

    Ok((
        i,
        Line::Instruction(Instruction::LoadStore(op_to_opcode_func(op), rt, mr)),
    ))
}

fn parse_shift(input: &str) -> IResult<&str, Line> {
    let (i, (op, _, rd, _, _, rs, _, _, shamt)) = tuple((
        alt((tag("sll"), tag("srl"))),
        multispace1,
        parse_register,
        tag(","),
        multispace1,
        parse_register,
        tag(","),
        multispace1,
        parse_immarg,
    ))(input)?;

    match shamt {
        ImmArg::Immediate(imm) => {
            if valid_int_size(imm, 5) {
                Ok((
                    i,
                    Line::Instruction(Instruction::Shift(op_to_opcode_func(op), rd, rs, shamt)),
                ))
            } else {
                Err(Err::Error(Error::from_error_kind(input, ErrorKind::IsNot)))
            }
        }
        ImmArg::Constant(_) => Ok((
            i,
            Line::Instruction(Instruction::Shift(op_to_opcode_func(op), rd, rs, shamt)),
        )),
    }
}

fn parse_arith_log(input: &str) -> IResult<&str, Line> {
    let (i, (op, _, rd, _, _, rs, _, _, rt)) = tuple((
        alt((
            tag("add"),
            tag("addu"),
            tag("and"),
            tag("nor"),
            tag("or"),
            tag("sub"),
            tag("subu"),
            tag("slt"),
            tag("sltu"),
        )),
        multispace1,
        parse_register,
        tag(","),
        multispace1,
        parse_register,
        tag(","),
        multispace1,
        parse_register,
    ))(input)?;

    Ok((
        i,
        Line::Instruction(Instruction::ArithLogic(op_to_opcode_func(op), rd, rs, rt)),
    ))
}

fn parse_arith_log_imm(input: &str) -> IResult<&str, Line> {
    let (i, (op, _, rd, _, _, rs, _, _, imm)) = tuple((
        alt((
            tag("addi"),
            tag("addiu"),
            tag("andi"),
            tag("ori"),
            tag("slti"),
            tag("sltiu"),
        )),
        multispace1,
        parse_register,
        tag(","),
        multispace1,
        parse_register,
        tag(","),
        multispace1,
        parse_immarg,
    ))(input)?;

    match imm {
        ImmArg::Immediate(im) => {
            if valid_int_size(im, 16) {
                Ok((
                    i,
                    Line::Instruction(Instruction::ArithLogicImm(
                        op_to_opcode_func(op),
                        rd,
                        rs,
                        imm,
                    )),
                ))
            } else {
                Err(Err::Error(Error::from_error_kind(input, ErrorKind::IsNot)))
            }
        }
        ImmArg::Constant(_) => Ok((
            i,
            Line::Instruction(Instruction::ArithLogicImm(
                op_to_opcode_func(op),
                rd,
                rs,
                imm,
            )),
        )),
    }
}

// ------------------ Component Parsers ------------------
fn parse_memref(input: &str) -> IResult<&str, MemRef> {
    alt((
        parse_reg_mem_normal,
        parse_reg_mem_offset,
        parse_label_mem_normal,
        parse_label_mem_offset,
        parse_immediate_arg,
    ))(input)
}

fn parse_label_mem_normal(input: &str) -> IResult<&str, MemRef> {
    let (i, label) = parse_var(input)?;
    Ok((i, (0, MemLoc::Label(label.to_string()))))
}

fn parse_label_mem_offset(input: &str) -> IResult<&str, MemRef> {
    let (i, (offset, _, label, _)) = tuple((i64, tag("("), parse_var, tag(")")))(input)?;
    Ok((i, (offset, MemLoc::Label(label.to_string()))))
}

fn parse_reg_mem_normal(input: &str) -> IResult<&str, MemRef> {
    let (i, reg) = parse_register(input)?;
    Ok((i, (0, MemLoc::Register(reg))))
}

fn parse_reg_mem_offset(input: &str) -> IResult<&str, MemRef> {
    let (i, (offset, _, reg, _)) = tuple((i64, tag("("), parse_register, tag(")")))(input)?;

    let offset_int = offset;
    if valid_int_size(offset_int, 16) {
        Ok((i, (offset_int, MemLoc::Register(reg))))
    } else {
        Err(Err::Error(Error::from_error_kind(input, ErrorKind::IsNot)))
    }
}

fn parse_immediate_arg(input: &str) -> IResult<&str, MemRef> {
    let (i, imm) = i64(input)?;
    if valid_int_size(imm, 16) {
        Ok((i, (0, MemLoc::Immediate(imm))))
    } else {
        Err(Err::Error(Error::from_error_kind(input, ErrorKind::IsNot)))
    }
}

fn parse_register(input: &str) -> IResult<&str, Register> {
    let (i, (_, reg)) = tuple((tag("$"), alphanumeric1))(input)?;

    match reg {
        "0" | "zero" => Ok((i, 0)),
        "1" | "at" => Ok((i, 1)),
        "2" | "v0" => Ok((i, 2)),
        "3" | "v1" => Ok((i, 3)),
        "4" | "a0" => Ok((i, 4)),
        "5" | "a1" => Ok((i, 5)),
        "6" | "a2" => Ok((i, 6)),
        "7" | "a3" => Ok((i, 7)),
        "8" | "t0" => Ok((i, 8)),
        "9" | "t1" => Ok((i, 9)),
        "10" | "t2" => Ok((i, 10)),
        "11" | "t3" => Ok((i, 11)),
        "12" | "t4" => Ok((i, 12)),
        "13" | "t5" => Ok((i, 13)),
        "14" | "t6" => Ok((i, 14)),
        "15" | "t7" => Ok((i, 15)),
        "16" | "s0" => Ok((i, 16)),
        "17" | "s1" => Ok((i, 17)),
        "18" | "s2" => Ok((i, 18)),
        "19" | "s3" => Ok((i, 19)),
        "20" | "s4" => Ok((i, 20)),
        "21" | "s5" => Ok((i, 21)),
        "22" | "s6" => Ok((i, 22)),
        "23" | "s7" => Ok((i, 23)),
        "24" | "t8" => Ok((i, 24)),
        "25" | "t9" => Ok((i, 25)),
        "26" | "k0" => Ok((i, 26)),
        "27" | "k1" => Ok((i, 27)),
        "28" | "gp" => Ok((i, 28)),
        "29" | "sp" => Ok((i, 29)),
        "30" | "fp" => Ok((i, 30)),
        "31" | "ra" => Ok((i, 31)),
        _ => Err(Err::Error(Error::from_error_kind(input, ErrorKind::IsNot))),
    }
}

fn parse_immarg(input: &str) -> IResult<&str, ImmArg> {
    alt((parse_immarg_imm, parse_immarg_const))(input)
}

fn parse_immarg_imm(input: &str) -> IResult<&str, ImmArg> {
    let (i, imm) = i64(input)?;
    Ok((i, ImmArg::Immediate(imm)))
}

fn parse_immarg_const(input: &str) -> IResult<&str, ImmArg> {
    let (i, imm) = parse_var(input)?;
    Ok((i, ImmArg::Constant(imm.to_string())))
}

// ------------------ Conversion and Validation Functions ------------------
fn op_to_opcode_func(input: &str) -> Operation {
    match input {
        "add" => (0x00, 0x20),
        "addi" => (0x08, 0x00),
        "addiu" => (0x09, 0x00),
        "addu" => (0x00, 0x21),
        "and" => (0x00, 0x24),
        "andi" => (0x0C, 0x00),
        "beq" => (0x04, 0x00),
        "bne" => (0x05, 0x00),
        "j" => (0x02, 0x00),
        "jal" => (0x03, 0x00),
        "jr" => (0x00, 0x08),
        "lbu" => (0x24, 0x00),
        "lhu" => (0x25, 0x00),
        "ll" => (0x30, 0x00),
        "lui" => (0x0F, 0x00),
        "lw" => (0x23, 0x00),
        "nor" => (0x00, 0x27),
        "or" => (0x00, 0x25),
        "ori" => (0x0D, 0x00),
        "slt" => (0x00, 0x2A),
        "stli" => (0x0A, 0x00),
        "stliu" => (0x0B, 0x00),
        "stlu" => (0x00, 0x2B),
        "sll" => (0x00, 0x00),
        "srl" => (0x00, 0x02),
        "sb" => (0x28, 0x00),
        "sc" => (0x38, 0x00),
        "sh" => (0x29, 0x00),
        "sw" => (0x2B, 0x00),
        "sub" => (0x00, 0x22),
        "subu" => (0x00, 0x23),
        _ => (0x00, 0x00),
    }
}

fn valid_int_size(val: i64, size: i64) -> bool {
    let ranged_val = if val >= 0 { val } else { !(val - 1) };
    ranged_val < 1 << size
}

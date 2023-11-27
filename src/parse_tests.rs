#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_register() {
        {
            let input = Span::new("$s3");
            let result: Result<i64, ErrorTree<Span>> =
                final_parser(parse_register::<ErrorTree<Span>>)(input);

            match result {
                Ok(v) => assert_eq!(v, 19),
                Result::Err(_) => panic!(),
            }
        }
        {
            let input = Span::new("$hi");
            let result: Result<i64, ErrorTree<Span>> =
                final_parser(parse_register::<ErrorTree<Span>>)(input);

            assert!(result.is_err());
        }
    }

    #[test]
    fn test_const_decl() {
        {
            let input = Span::new("value = 3");
            let result: Result<Declaration, ErrorTree<Span>> =
                final_parser(parse_const_decl::<ErrorTree<Span>>)(input);

            match result {
                Ok(v) => assert!(matches!(v, Declaration::Constant((_, 3)))),
                Result::Err(_) => panic!(),
            }
        }
        {
            let input = Span::new("value = e");
            let result: Result<Declaration, ErrorTree<Span>> =
                final_parser(parse_const_decl::<ErrorTree<Span>>)(input);

            assert!(result.is_err());
        }
    }

    #[test]
    fn test_label_decl() {
        {
            let input = Span::new("main:");
            let result: Result<Declaration, ErrorTree<Span>> =
                final_parser(parse_label_decl::<ErrorTree<Span>>)(input);

            match result {
                Ok(v) => assert!(matches!(v, Declaration::Label(_))),
                Result::Err(_) => panic!(),
            }
        }
        {
            let input = Span::new("lanes :");
            let result: Result<Declaration, ErrorTree<Span>> =
                final_parser(parse_label_decl::<ErrorTree<Span>>)(input);

            assert!(result.is_err());
        }
    }

    #[test]
    fn test_mem_decl() {
        {
            let input = Span::new(".space 100");
            let result: Result<Declaration, ErrorTree<Span>> =
                final_parser(parse_mem_decl::<ErrorTree<Span>>)(input);

            match result {
                Ok(v) => assert!(matches!(v, Declaration::Allocation(Allocation::Space(_)))),
                Result::Err(_) => panic!(),
            }
        }
        {
            let input = Span::new(".space -30");
            let result: Result<Declaration, ErrorTree<Span>> =
                final_parser(parse_mem_decl::<ErrorTree<Span>>)(input);

            assert!(result.is_err());
        }
        {
            let input = Span::new(".byte 255");
            let result: Result<Declaration, ErrorTree<Span>> =
                final_parser(parse_mem_decl::<ErrorTree<Span>>)(input);

            match result {
                Ok(v) => assert!(matches!(
                    v,
                    Declaration::Allocation(Allocation::Value(_, 1))
                )),
                Result::Err(_) => panic!(),
            }
        }
        {
            let input = Span::new(".byte 256");
            let result: Result<Declaration, ErrorTree<Span>> =
                final_parser(parse_mem_decl::<ErrorTree<Span>>)(input);

            assert!(result.is_err());
        }
    }

    #[test]
    fn test_parse_memref() {
        {
            let input = Span::new("-3($sp)");
            let result: Result<(i64, MemLoc), ErrorTree<Span>> =
                final_parser(parse_memref::<ErrorTree<Span>>)(input);

            match result {
                Ok(v) => assert!(matches!(v, (-3, MemLoc::Register(29)))),
                Result::Err(_) => panic!(),
            }
        }
        {
            let input = Span::new("$sp");
            let result: Result<(i64, MemLoc), ErrorTree<Span>> =
                final_parser(parse_memref::<ErrorTree<Span>>)(input);

            match result {
                Ok(v) => assert!(matches!(v, (0, MemLoc::Register(29)))),
                Result::Err(_) => panic!(),
            }
        }
        {
            let input = Span::new("($sp)");
            let result: Result<(i64, MemLoc), ErrorTree<Span>> =
                final_parser(parse_memref::<ErrorTree<Span>>)(input);

            assert!(result.is_err());
        }
        {
            let input = Span::new("100");
            let result: Result<(i64, MemLoc), ErrorTree<Span>> =
                final_parser(parse_memref::<ErrorTree<Span>>)(input);

            match result {
                Ok(v) => assert!(matches!(v, (0, MemLoc::Immediate(100)))),
                Result::Err(_) => panic!(),
            }
        }
    }

    #[test]
    fn test_full_assembly() {
        {
            let input = include_str!("../test_files/valid/test01.asm");
            let result = parse(input);

            match result {
                Ok(r) => {
                    let expected_instructions: Vec<u32> = vec![
                        0x3c011001, 0x8c280000, 0x3c011001, 0x8c290004, 0x3c011001, 0xac290004,
                        0x01095025, 0x350a0003, 0x1129fff8, 0x01200008, 0x000d60c0, 0x08100000,
                        0x08100006,
                    ];
                    let expected_memory = vec![0, 0, 0, 12, 0, 0, 0, 10];

                    assert_eq!(expected_instructions, r.0);
                    assert_eq!(expected_memory, r.1);
                }
                Err(_) => panic!(),
            }
        }
        {
            let input = include_str!("../test_files/valid/test02.asm");
            let result = parse(input);

            match result {
                Ok(r) => {
                    let expected_instructions: Vec<u32> = vec![0x01084020];
                    let expected_memory = vec![
                        0, 0, 4, 0, 0, 12, 0, 1, 1, 0, 5, 104, 101, 108, 108, 111, 103, 111, 111,
                        100, 98, 121, 101, 0,
                    ];

                    assert_eq!(expected_instructions, r.0);
                    assert_eq!(expected_memory, r.1);
                }
                Err(_) => panic!(),
            }
        }
        {
            let input = include_str!("../test_files/valid/test03.asm");
            let result = parse(input);

            match result {
                Ok(r) => {
                    let expected_instructions: Vec<u32> = vec![
                        0x3c011001, 0x8c280000, 0x20090001, 0x200a0000, 0x200d0001, 0x200c0000,
                        0x01695820, 0x218c0001, 0x1589fffe, 0x014b5020, 0x21290001, 0x0128602a,
                        0x118dfff9, 0x1128fff8, 0x3c011001, 0xac2a0004, 0x08100010,
                    ];
                    let expected_memory = vec![0, 0, 0, 10, 0, 0, 0, 0];

                    assert_eq!(expected_instructions, r.0);
                    assert_eq!(expected_memory, r.1);
                }
                Err(_) => panic!(),
            }
        }
        {
            let input = include_str!("../test_files/invalid/test01.asm");
            let result = parse(input);

            match result {
                Ok(_) => panic!(),
                Err(e) => assert_eq!(e, "Parse error at: oi $t2, $t0, 3 "),
            }
        }
        {
            let input = include_str!("../test_files/invalid/test02.asm");
            let result = parse(input);

            match result {
                Ok(_) => panic!(),
                Err(e) => assert_eq!(e, "File empty"),
            }
        }
    }
}

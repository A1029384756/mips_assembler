#[cfg(test)]
mod tests {
    use crate::*;
    use nom::Finish;

    #[test]
    fn test_register() {
        {
            let input = "$s3";
            let result = parse_register(input).finish();

            match result {
                Ok(v) => assert_eq!(v, ("", 19)),
                Result::Err(_) => assert!(false),
            }
        }
        {
            let input = "$hi";
            let result = parse_register(input).finish();

            match result {
                Ok(_) => assert!(false),
                Result::Err(err) => assert_eq!(err.input, input),
            }
        }
    }

    #[test]
    fn test_const_decl() {
        {
            let input = "value = 3";
            let result = parse_const_decl(input).finish();

            match result {
                Ok(v) => assert!(matches!(v, ("", Declaration::Constant((_, 3))))),
                Result::Err(_) => assert!(false),
            }
        }
        {
            let input = "value = e";
            let result = parse_const_decl(input).finish();

            match result {
                Ok(_) => assert!(false),
                Result::Err(err) => assert_eq!(err.input, "e"),
            }
        }
    }

    #[test]
    fn test_label_decl() {
        {
            let input = "main:";
            let result = parse_label_decl(input).finish();

            match result {
                Ok(v) => assert!(matches!(v, ("", Declaration::Label(_)))),
                Result::Err(_) => assert!(false),
            }
        }
        {
            let input = "lanes :";
            let result = parse_label_decl(input).finish();

            match result {
                Ok(_) => assert!(false),
                Result::Err(err) => assert_eq!(err.input, " :"),
            }
        }
    }

    #[test]
    fn test_mem_decl() {
        {
            let input = ".space 100";
            let result = parse_mem_decl(input).finish();

            match result {
                Ok(v) => assert!(matches!(
                    v,
                    ("", Declaration::Allocation(Allocation::Space(100)))
                )),
                Result::Err(_) => assert!(false),
            }
        }
        {
            let input = ".space -30";
            let result = parse_mem_decl(input).finish();

            match result {
                Ok(_) => assert!(false),
                Result::Err(err) => assert_eq!(err.input, input),
            }
        }
        {
            let input = ".byte 255";
            let result = parse_mem_decl(input).finish();

            match result {
                Ok(v) => assert!(matches!(
                    v,
                    ("", Declaration::Allocation(Allocation::Value(255, 1)))
                )),
                Result::Err(_) => assert!(false),
            }
        }
        {
            let input = ".byte 256";
            let result = parse_mem_decl(input).finish();

            match result {
                Ok(_) => assert!(false),
                Result::Err(err) => assert_eq!(err.input, input),
            }
        }
    }

    #[test]
    fn test_parse_memref() {
        {
            let input = "-3($sp)";
            let result = parse_memref(input).finish();

            match result {
                Ok(v) => assert!(matches!(v, ("", (-3, MemLoc::Register(29))))),
                Result::Err(_) => assert!(false),
            }
        }
        {
            let input = "$sp";
            let result = parse_memref(input).finish();

            match result {
                Ok(v) => assert!(matches!(v, ("", (0, MemLoc::Register(29))))),
                Result::Err(_) => assert!(false),
            }
        }
        {
            let input = "($sp)";
            let result = parse_memref(input).finish();

            match result {
                Ok(_) => assert!(false),
                Result::Err(err) => assert_eq!(err.input, input),
            }
        }
        {
            let input = "100";
            let result = parse_memref(input).finish();

            match result {
                Ok(v) => assert!(matches!(v, ("", (0, MemLoc::Immediate(100))))),
                Result::Err(_) => assert!(false),
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
                        0x01095025, 0x350a0003, 0x1129fff8, 0x01200008, 0x000d60c0,
                    ];
                    let expected_memory = vec![0, 0, 0, 12, 0, 0, 0, 10];

                    assert_eq!(expected_instructions, r.0);
                    assert_eq!(expected_memory, r.1);
                }
                Err(_) => assert!(false),
            }
        }
        {
            let input = include_str!("../test_files/valid/test02.asm");
            let result = parse(input);

            match result {
                Ok(r) => {
                    let expected_instructions: Vec<u32> = vec![0x01084020];
                    let expected_memory = vec![
                        0, 0, 4, 0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 1, 0, 104, 101, 108, 108,
                        111, 103, 111, 111, 100, 98, 121, 101, 0,
                    ];

                    assert_eq!(expected_instructions, r.0);
                    assert_eq!(expected_memory, r.1);
                }
                Err(_) => assert!(false),
            }
        }
        {
            let input = include_str!("../test_files/valid/test03.asm");
            let result = parse(input);

            match result {
                Ok(_) => assert!(false),
                Err(_) => assert!(true),
            }
        }
    }
}

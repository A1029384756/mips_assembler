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
}

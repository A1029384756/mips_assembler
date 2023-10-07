#[cfg(test)]
mod tests {
    use crate::*;
    use nom::Finish;

    #[test]
    fn test_opcode() {
        {
            let input = "add";
            let result = parse_opcode(input).finish();

            match result {
                Ok(v) => assert_eq!(v, ("", (0x00, 0x20))),
                Result::Err(_) => assert!(false),
            }
        }
        {
            let input = "sub";
            let result = parse_opcode(input).finish();

            match result {
                Ok(v) => assert_ne!(v, ("", (0x00, 0x20))),
                Result::Err(_) => assert!(false),
            }
        }
    }

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
                Ok(v) => assert_eq!(v, ("", ("value", 3))),
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
                Ok(v) => assert_eq!(v, ("", "main")),
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
}

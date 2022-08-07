pub mod lexer;
pub mod pt;

#[allow(clippy::all)]
mod func {
    include!(concat!(env!("OUT_DIR"), "/func.rs"));
}

pub fn parse(src: &str, file_no: usize) -> Result<(), ()> {
    let lex = lexer::Lexer::new(src, file_no);

    let test = func::ExprParser::new().parse(src, file_no, lex);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expression() {
        const SRC: &str = "test >>= (-1 + 2 * (-4) + 3) << 1 == 3 / 10";

        let lex = lexer::Lexer::new(SRC, 0);
        let expr = func::ExprParser::new().parse(SRC, 0, lex).unwrap();
        println!("{expr:#?}");
    }
}

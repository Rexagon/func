use std::str::CharIndices;

use itertools::{peek_nth, PeekNth};
use phf::phf_map;

use super::pt::{CodeLocation, Loc};

pub type Spanned<Token, Loc, Error> = Result<(Loc, Token, Loc), Error>;

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Token<'input> {
    Identifier(&'input str),
    StringLiteral(&'input str),
    Number(&'input str),
    HexNumber(&'input str),

    /// "+"
    Add,
    /// "-"
    Sub,
    /// "*"
    Mul,
    /// "/"
    Div,
    /// "%"
    Mod,
    /// "?"
    Question,
    /// ":"
    Colon,
    /// ","
    Comma,
    /// ";"
    Semi,
    /// "("
    OpenParen,
    /// ")"
    CloseParen,
    /// "["
    OpenBracket,
    /// "]"
    CloseBracket,
    /// "{"
    OpenBrace,
    /// "}"
    CloseBrace,
    /// "="
    Let,
    /// "<"
    Lt,
    /// ">"
    Gt,
    /// "&"
    And,
    /// "|"
    Or,
    /// "^"
    Xor,
    /// "~"
    Tilde,
    /// "."
    Dot,

    /// "=="
    Eq,
    /// "!="
    Neq,
    /// "<="
    Leq,
    /// ">="
    Geq,
    /// "<=>"
    Spaceship,
    /// "<<"
    LShift,
    /// ">>"
    RShift,
    /// "~>>"
    RShiftR,
    /// "^>>"
    RShiftC,
    /// "~/"
    DivR,
    /// "^/"
    DivC,
    /// "~%"
    ModR,
    /// "^%"
    ModC,
    /// "/%"
    DivMod,
    /// "+="
    AddLet,
    /// "-="
    SubLet,
    /// "*="
    MulLet,
    /// "/="
    DivLet,
    /// "~/="
    DivRLet,
    /// "^/="
    DivCLet,
    /// "%="
    ModLet,
    /// "~%="
    ModRLet,
    /// "^%="
    ModCLet,
    /// "<<="
    LShiftLet,
    /// ">>="
    RShiftLet,
    /// "~>>="
    RShiftRLet,
    /// "^>>="
    RShiftCLet,
    /// "&="
    AndLet,
    /// "|="
    OrLet,
    /// "^="
    XorLet,

    /// "_"
    Hole,
    /// "return"
    Return,
    /// "var"
    Var,
    /// "repeat"
    Repeat,
    /// "do"
    Do,
    /// "while"
    While,
    /// "until"
    Until,
    /// "if"
    If,
    /// "ifnot"
    IfNot,
    /// "then"
    Then,
    /// "else"
    Else,
    /// "elseif"
    ElseIf,
    /// "elseifnot"
    ElseIfNot,

    /// "int"
    Int,
    /// "cell"
    Cell,
    /// "slice"
    Slice,
    /// "builder"
    Builder,
    /// "cont"
    Cont,
    /// "tuple"
    Tuple,
    /// "type"
    Type,
    /// "->"
    MapsTo,
    /// "forall"
    Forall,

    /// "extern"
    Extern,
    /// "global"
    Global,
    /// "asm"
    Asm,
    /// "impure"
    Impure,
    /// "inline"
    Inline,
    /// "inline_ref"
    InlineRef,
    /// "auto_apply"
    AutoApply,
    /// "method_id"
    MethodId,
    /// "operator"
    Operator,
    /// "infix"
    Infix,
    /// "infixl"
    InfixL,
    /// "infixr"
    InfixR,
}

impl<'input> std::fmt::Display for Token<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Identifier(data) => write!(f, "{data}"),
            Token::StringLiteral(data) => write!(f, "\"{data}\""),
            Token::Number(data) => write!(f, "{data}"),
            Token::HexNumber(data) => write!(f, "0x{data}"),
            Token::Add => write!(f, "+"),
            Token::Sub => write!(f, "-"),
            Token::Mul => write!(f, "*"),
            Token::Div => write!(f, "/"),
            Token::Mod => write!(f, "%"),
            Token::Question => write!(f, "?"),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::Semi => write!(f, ";"),
            Token::OpenParen => write!(f, "("),
            Token::CloseParen => write!(f, ")"),
            Token::OpenBracket => write!(f, "["),
            Token::CloseBracket => write!(f, "]"),
            Token::OpenBrace => write!(f, "{{"),
            Token::CloseBrace => write!(f, "}}"),
            Token::Let => write!(f, "="),
            Token::Lt => write!(f, "<"),
            Token::Gt => write!(f, ">"),
            Token::And => write!(f, "&"),
            Token::Or => write!(f, "|"),
            Token::Xor => write!(f, "^"),
            Token::Tilde => write!(f, "~"),
            Token::Dot => write!(f, "."),
            Token::Eq => write!(f, "=="),
            Token::Neq => write!(f, "!="),
            Token::Leq => write!(f, "<="),
            Token::Geq => write!(f, ">="),
            Token::Spaceship => write!(f, "<=>"),
            Token::LShift => write!(f, "<<"),
            Token::RShift => write!(f, ">>"),
            Token::RShiftR => write!(f, "~>>"),
            Token::RShiftC => write!(f, "^>>"),
            Token::DivR => write!(f, "~/"),
            Token::DivC => write!(f, "^/"),
            Token::ModR => write!(f, "~%"),
            Token::ModC => write!(f, "^%"),
            Token::DivMod => write!(f, "/%"),
            Token::AddLet => write!(f, "+="),
            Token::SubLet => write!(f, "-="),
            Token::MulLet => write!(f, "*="),
            Token::DivLet => write!(f, "/="),
            Token::DivRLet => write!(f, "~/="),
            Token::DivCLet => write!(f, "^/="),
            Token::ModLet => write!(f, "%="),
            Token::ModRLet => write!(f, "~%="),
            Token::ModCLet => write!(f, "^%="),
            Token::LShiftLet => write!(f, "<<="),
            Token::RShiftLet => write!(f, ">>="),
            Token::RShiftRLet => write!(f, "~>>="),
            Token::RShiftCLet => write!(f, "^>>="),
            Token::AndLet => write!(f, "&="),
            Token::OrLet => write!(f, "|="),
            Token::XorLet => write!(f, "^="),
            Token::Hole => write!(f, "_"),
            Token::Return => write!(f, "return"),
            Token::Var => write!(f, "var"),
            Token::Repeat => write!(f, "repeat"),
            Token::Do => write!(f, "do"),
            Token::While => write!(f, "while"),
            Token::Until => write!(f, "until"),
            Token::If => write!(f, "if"),
            Token::IfNot => write!(f, "ifnot"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),
            Token::ElseIf => write!(f, "elseif"),
            Token::ElseIfNot => write!(f, "elseifnot"),
            Token::Int => write!(f, "int"),
            Token::Cell => write!(f, "cell"),
            Token::Slice => write!(f, "slice"),
            Token::Builder => write!(f, "builder"),
            Token::Cont => write!(f, "cont"),
            Token::Tuple => write!(f, "tuple"),
            Token::Type => write!(f, "type"),
            Token::MapsTo => write!(f, "->"),
            Token::Forall => write!(f, "forall"),
            Token::Extern => write!(f, "extern"),
            Token::Global => write!(f, "global"),
            Token::Asm => write!(f, "asm"),
            Token::Impure => write!(f, "impure"),
            Token::Inline => write!(f, "inline"),
            Token::InlineRef => write!(f, "inline_ref"),
            Token::AutoApply => write!(f, "auto_apply"),
            Token::MethodId => write!(f, "method_id"),
            Token::Operator => write!(f, "operator"),
            Token::Infix => write!(f, "infix"),
            Token::InfixL => write!(f, "infixl"),
            Token::InfixR => write!(f, "infixr"),
        }
    }
}

pub struct Lexer<'input> {
    input: &'input str,
    chars: PeekNth<CharIndices<'input>>,
    file_no: usize,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str, file_no: usize) -> Self {
        Self {
            input,
            chars: peek_nth(input.char_indices()),
            file_no,
        }
    }

    fn string(&mut self, start: usize) -> Result<(usize, Token<'input>, usize), LexicalError> {
        let mut end;
        loop {
            if let Some((i, ch)) = self.chars.next() {
                end = i;
                if ch == QUOTE_CHAR {
                    break;
                }
            } else {
                return Err(LexicalError::EndOfFileInString(Loc::File(
                    self.file_no,
                    start,
                    self.input.len(),
                )));
            }
        }

        Ok((
            start,
            Token::StringLiteral(&self.input[start + 1..=end - 1]),
            end + 1,
        ))
    }

    fn parse_number(
        &mut self,
        start: usize,
        ch: char,
    ) -> Result<(usize, Token<'input>, usize), LexicalError> {
        if ch == '0' {
            // check if hex
            if let Some((_, 'x')) = self.chars.peek() {
                self.chars.next();

                let mut end = match self.chars.next() {
                    Some((end, ch)) if ch.is_ascii_hexdigit() => end,
                    Some(_) => {
                        return Err(LexicalError::MissingNumber(Loc::File(
                            self.file_no,
                            start,
                            start + 1,
                        )));
                    }
                    None => {
                        return Err(LexicalError::EndOfFileInHex(Loc::File(
                            self.file_no,
                            start,
                            self.input.len(),
                        )))
                    }
                };

                while let Some((i, ch)) = self.chars.peek() {
                    if !ch.is_ascii_hexdigit() {
                        break;
                    }
                    end = *i;
                    self.chars.next();
                }

                return Ok((
                    start,
                    Token::HexNumber(&self.input[start + 2..=end]),
                    end + 1,
                ));
            }
        }

        let mut end = start;
        while let Some((i, ch)) = self.chars.peek() {
            if !ch.is_ascii_digit() {
                break;
            }
            end = *i;
            self.chars.next();
        }

        Ok((start, Token::Number(&self.input[start..=end]), end + 1))
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token<'input>, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = 'chars: loop {
            break match self.chars.next()? {
                (_, ch) if ch.is_whitespace() => continue 'chars,
                (i, '+') => match self.chars.peek() {
                    // "+="
                    Some((_, '=')) => {
                        self.chars.next();
                        (i, Token::AddLet, i + 2)
                    }
                    // "+"
                    _ => (i, Token::Add, i + 1),
                },
                (i, '-') => match self.chars.peek() {
                    // "-="
                    Some((_, '=')) => {
                        self.chars.next();
                        (i, Token::SubLet, i + 2)
                    }
                    // "->"
                    Some((_, '>')) => {
                        self.chars.next();
                        (i, Token::MapsTo, i + 2)
                    }
                    // "-"
                    _ => (i, Token::Sub, i + 1),
                },
                (i, '*') => match self.chars.peek() {
                    // "*="
                    Some((_, '=')) => {
                        self.chars.next();
                        (i, Token::MulLet, i + 2)
                    }
                    // "*"
                    _ => (i, Token::Mul, i + 1),
                },
                (i, '/') => match self.chars.peek() {
                    // "/="
                    Some((_, '=')) => {
                        self.chars.next();
                        (i, Token::DivLet, i + 2)
                    }
                    // "/%"
                    Some((_, '%')) => {
                        self.chars.next();
                        (i, Token::DivMod, i + 2)
                    }
                    // "/"
                    _ => (i, Token::Div, i + 1),
                },
                (i, '%') => match self.chars.peek() {
                    // "%="
                    Some((_, '=')) => {
                        self.chars.next();
                        (i, Token::ModLet, i + 2)
                    }
                    // "%"
                    _ => (i, Token::Mod, i + 1),
                },
                (i, '?') => (i, Token::Question, i + 1),
                (i, ':') => (i, Token::Colon, i + 1),
                (i, ',') => (i, Token::Comma, i + 1),
                (i, ';') => match self.chars.peek() {
                    // ";;" (single line comment)
                    Some((_, ';')) => {
                        while !matches!(self.chars.next(), Some((_, '\n' | '\r')) | None) {}
                        continue 'chars;
                    }
                    // ";"
                    _ => (i, Token::Semi, i + 1),
                },
                (i, '(') => (i, Token::OpenParen, i + 1),
                (i, ')') => (i, Token::CloseParen, i + 1),
                (i, '[') => (i, Token::OpenBracket, i + 1),
                (i, ']') => (i, Token::CloseBracket, i + 1),
                (i, '{') => match self.chars.peek() {
                    // "{-" (comment begin)
                    Some((_, '-')) => {
                        self.chars.next();
                        let mut level = 1;
                        while let Some((_, ch)) = self.chars.next() {
                            if ch == ';' && matches!(self.chars.peek(), Some((_, ';'))) {
                                while !matches!(self.chars.next(), Some((_, '\n' | '\r')) | None) {}
                            } else if ch == '{' && matches!(self.chars.peek(), Some((_, '-'))) {
                                self.chars.next();
                                level += 1;
                            } else if ch == '-' && matches!(self.chars.peek(), Some((_, '}'))) {
                                self.chars.next();
                                level -= 1;
                                if level == 0 {
                                    continue 'chars;
                                }
                            }
                        }
                        return Some(Err(LexicalError::EndOfFileInComment(Loc::File(
                            self.file_no,
                            i,
                            self.input.len(),
                        ))));
                    }
                    // "{"
                    _ => (i, Token::OpenBrace, i + 1),
                },
                (i, '}') => (i, Token::CloseBrace, i + 1),
                (i, '=') => match self.chars.peek() {
                    // "=="
                    Some((_, '=')) => {
                        self.chars.next();
                        (i, Token::Eq, i + 2)
                    }
                    // "="
                    _ => (i, Token::Let, i + 1),
                },
                (i, '<') => match self.chars.peek() {
                    // "<=" or "<=>"
                    Some((_, '=')) => match self.chars.peek_nth(1) {
                        // "<=>"
                        Some((_, '>')) => {
                            self.chars.next();
                            self.chars.next();
                            (i, Token::Spaceship, i + 3)
                        }
                        // "<="
                        _ => {
                            self.chars.next();
                            (i, Token::Leq, i + 2)
                        }
                    },
                    // "<<" or "<<="
                    Some((_, '<')) => match self.chars.peek_nth(1) {
                        // "<<="
                        Some((_, '=')) => {
                            self.chars.next();
                            self.chars.next();
                            (i, Token::LShiftLet, i + 3)
                        }
                        // "<<"
                        _ => {
                            self.chars.next();
                            (i, Token::LShift, i + 2)
                        }
                    },
                    // "<"
                    _ => (i, Token::Lt, i + 1),
                },
                (i, '>') => match self.chars.peek() {
                    // ">="
                    Some((_, '=')) => {
                        self.chars.next();
                        (i, Token::Geq, i + 2)
                    }
                    // ">>" or ">>="
                    Some((_, '>')) => match self.chars.peek_nth(1) {
                        // ">>="
                        Some((_, '=')) => {
                            self.chars.next();
                            self.chars.next();
                            (i, Token::RShiftLet, i + 3)
                        }
                        // ">>"
                        _ => {
                            self.chars.next();
                            (i, Token::RShift, i + 2)
                        }
                    },
                    // ">"
                    _ => (i, Token::Gt, i + 1),
                },
                (i, '&') => match self.chars.peek() {
                    // "&="
                    Some((_, '=')) => {
                        self.chars.next();
                        (i, Token::AndLet, i + 2)
                    }
                    // "&"
                    _ => (i, Token::And, i + 1),
                },
                (i, '|') => match self.chars.peek() {
                    // "&="
                    Some((_, '=')) => {
                        self.chars.next();
                        (i, Token::OrLet, i + 2)
                    }
                    // "&"
                    _ => (i, Token::Or, i + 1),
                },
                (i, '^') => match self.chars.peek().cloned() {
                    Some((_, '>')) => {
                        match (self.chars.peek_nth(1).cloned(), self.chars.peek_nth(2)) {
                            // "^>>="
                            (Some((_, '>')), Some((_, '='))) => {
                                self.chars.next();
                                self.chars.next();
                                self.chars.next();
                                (i, Token::RShiftCLet, i + 4)
                            }
                            // "^>>"
                            (Some((_, '>')), _) => {
                                self.chars.next();
                                self.chars.next();
                                (i, Token::RShiftC, i + 3)
                            }
                            // "^"
                            _ => (i, Token::Xor, i + 1),
                        }
                    }
                    Some((_, c @ '/' | c @ '%')) => {
                        match (self.chars.next(), self.chars.peek().cloned()) {
                            // "^/=" or "^%="
                            (_, Some((_, '='))) => {
                                self.chars.next();
                                let op = if c == '/' {
                                    Token::DivCLet
                                } else {
                                    Token::ModCLet
                                };
                                (i, op, i + 3)
                            }
                            // "^/" or "^%"
                            _ => (i, if c == '/' { Token::DivC } else { Token::ModC }, i + 2),
                        }
                    }
                    // "^="
                    Some((_, '=')) => {
                        self.chars.next();
                        (i, Token::XorLet, i + 2)
                    }
                    // "^"
                    _ => (i, Token::Xor, i + 1),
                },
                (i, '~') => match self.chars.peek().cloned() {
                    Some((_, '>')) => {
                        match (self.chars.peek_nth(1).cloned(), self.chars.peek_nth(2)) {
                            // "~>>="
                            (Some((_, '>')), Some((_, '='))) => {
                                self.chars.next();
                                self.chars.next();
                                self.chars.next();
                                (i, Token::RShiftRLet, i + 4)
                            }
                            // "~>>"
                            (Some((_, '>')), _) => {
                                self.chars.next();
                                self.chars.next();
                                (i, Token::RShiftR, i + 3)
                            }
                            // "~"
                            _ => (i, Token::Tilde, i + 1),
                        }
                    }
                    Some((_, c @ '/' | c @ '%')) => match (self.chars.next(), self.chars.peek()) {
                        // "~/=" or "~%="
                        (_, Some((_, '='))) => {
                            self.chars.next();
                            let op = if c == '/' {
                                Token::DivRLet
                            } else {
                                Token::ModRLet
                            };
                            (i, op, i + 3)
                        }
                        // "~/" or "~%"
                        _ => (i, if c == '/' { Token::DivR } else { Token::ModR }, i + 2),
                    },
                    // "~"
                    _ => (i, Token::Tilde, i + 1),
                },
                (i, '.') => (i, Token::Dot, i + 1),
                (i, '!') if matches!(self.chars.peek(), Some((_, '='))) => {
                    self.chars.next();
                    (i, Token::Neq, i + 2)
                }
                (i, '`') => {
                    let mut end;
                    loop {
                        if let Some((i, ch)) = self.chars.next() {
                            end = i;
                            if ch == '`' {
                                break;
                            } else if ch == '\n' || ch == '\r' {
                                return Some(Err(LexicalError::NewLineInIdentifier(Loc::File(
                                    self.file_no,
                                    i,
                                    i + 1,
                                ))));
                            }
                        } else {
                            return Some(Err(LexicalError::EndOfFileInIdentifier(Loc::File(
                                self.file_no,
                                i,
                                self.input.len(),
                            ))));
                        }
                    }
                    (i, Token::Identifier(&self.input[i..=end]), end + 1)
                }
                (i, '"') => return Some(self.string(i)),
                (i, ch) if ch.is_ascii_digit() => return Some(self.parse_number(i, ch)),
                (i, _) => {
                    #[inline(always)]
                    fn is_ident_char(ch: &char) -> bool {
                        matches!(ch, ';' | ',' | '(' | ')' | '~' | '.') || ch.is_whitespace()
                    }

                    let end = loop {
                        if let Some((i, ch)) = self.chars.peek() {
                            if is_ident_char(ch) {
                                break *i;
                            }
                            self.chars.next();
                        } else {
                            break self.input.len();
                        }
                    };

                    let ident = &self.input[i..end];
                    let token = KEYWORDS
                        .get(ident)
                        .cloned()
                        .unwrap_or(Token::Identifier(ident));
                    (i, token, end)
                }
            };
        };
        Some(Ok(token))
    }
}

const QUOTE_CHAR: char = '"';

static KEYWORDS: phf::Map<&'static str, Token> = phf_map! {
    "_" => Token::Hole,
    "return" => Token::Return,
    "var" => Token::Var,
    "repeat" => Token::Repeat,
    "do" => Token::Do,
    "while" => Token::While,
    "until" => Token::Until,
    "if" => Token::If,
    "ifnot" => Token::IfNot,
    "then" => Token::Then,
    "else" => Token::Else,
    "elseif" => Token::ElseIf,
    "elseifnot" => Token::ElseIfNot,
    "int" => Token::Int,
    "cell" => Token::Cell,
    "slice" => Token::Slice,
    "builder" => Token::Builder,
    "cont" => Token::Cont,
    "tuple" => Token::Tuple,
    "type" => Token::Type,
    "forall" => Token::Forall,
    "extern" => Token::Extern,
    "global" => Token::Global,
    "asm" => Token::Asm,
    "impure" => Token::Impure,
    "inline" => Token::Inline,
    "inline_ref" => Token::InlineRef,
    "auto_apply" => Token::AutoApply,
    "method_id" => Token::MethodId,
    "operator" => Token::Operator,
    "infix" => Token::Infix,
    "infixl" => Token::InfixL,
    "infixr" => Token::InfixR,
};

#[derive(Debug, PartialEq)]
pub enum LexicalError {
    EndOfFileInComment(Loc),
    EndOfFileInString(Loc),
    EndOfFileInIdentifier(Loc),
    NewLineInIdentifier(Loc),
    EndOfFileInHex(Loc),
    MissingNumber(Loc),
    UnknownStringType(Loc),
}

impl std::fmt::Display for LexicalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::EndOfFileInComment(..) => "end of file found in comment",
            Self::EndOfFileInString(..) => "end of file found in string literal",
            Self::EndOfFileInIdentifier(..) => "end of file found in identifier",
            Self::NewLineInIdentifier(..) => "new line found in identifier",
            Self::EndOfFileInHex(..) => "end of file found in hex literal string",
            Self::MissingNumber(..) => "missing number",
            Self::UnknownStringType(..) => "unknown string type",
        })
    }
}

impl CodeLocation for LexicalError {
    fn loc(&self) -> Loc {
        match self {
            Self::EndOfFileInString(loc, ..)
            | Self::EndOfFileInComment(loc, ..)
            | Self::EndOfFileInHex(loc, ..)
            | Self::NewLineInIdentifier(loc, ..)
            | Self::EndOfFileInIdentifier(loc, ..)
            | Self::MissingNumber(loc, ..)
            | Self::UnknownStringType(loc, ..) => *loc,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLES: &[&str] = &[
        include_str!("../test/a6.fc"),
        include_str!("../test/a6.fp"),
        include_str!("../test/a6_1.fc"),
        include_str!("../test/a6_2.fc"),
        include_str!("../test/a6_3.fc"),
        include_str!("../test/a6_4.fc"),
        include_str!("../test/a6_5.fc"),
        include_str!("../test/a7.fc"),
        include_str!("../test/a8.fc"),
        include_str!("../test/a9.fc"),
        include_str!("../test/a9_1.fc"),
        include_str!("../test/a10.fc"),
        include_str!("../test/a11.fc"),
        include_str!("../test/a12.fc"),
        include_str!("../test/a12_1.fc"),
        include_str!("../test/a12_2.fc"),
        include_str!("../test/a12_3.fc"),
        include_str!("../test/a12_4.fc"),
        include_str!("../test/a12_5.fc"),
        include_str!("../test/a12_6.fc"),
        include_str!("../test/a12_7.fc"),
        include_str!("../test/a12_8.fc"),
        include_str!("../test/a12_9.fc"),
        include_str!("../test/b1.fc"),
        include_str!("../test/b2.fc"),
        include_str!("../test/b2_0.fc"),
        include_str!("../test/b3.fc"),
        include_str!("../test/c1.fc"),
        include_str!("../test/c2.fc"),
        include_str!("../test/c2_1.fc"),
        include_str!("../test/w1.fc"),
        include_str!("../test/w2.fc"),
        include_str!("../test/w3.fc"),
        include_str!("../test/w4.fc"),
        include_str!("../test/w5.fc"),
        include_str!("../test/w6.fc"),
        include_str!("../test/w7.fc"),
        include_str!("../test/w8.fc"),
        include_str!("../test/w9.fc"),
    ];

    #[test]
    fn all_single_tokens_are_correct() {
        fn token(s: &str, len: usize, token: Token<'_>) {
            let mut lexer = Lexer::new(s, 0);
            let parsed = lexer.next().unwrap();
            assert_eq!(parsed, Ok((0, token, len)));
            assert_eq!(lexer.next(), None);
        }

        token("test1", 5, Token::Identifier("test1"));
        token("test1'", 6, Token::Identifier("test1'"));
        token("こんにちは世界", 21, Token::Identifier("こんにちは世界"));
        token("`strange ident`", 15, Token::Identifier("`strange ident`"));
        token("\"hello world\"", 13, Token::StringLiteral("hello world"));
        token("\"hello\nworld\"", 13, Token::StringLiteral("hello\nworld"));
        token("\"こんにち\"", 14, Token::StringLiteral("こんにち"));
        token("123", 3, Token::Number("123"));
        token("0xdeadbeef123", 13, Token::HexNumber("deadbeef123"));

        token("+", 1, Token::Add);
        token("-", 1, Token::Sub);
        token("*", 1, Token::Mul);
        token("/", 1, Token::Div);
        token("%", 1, Token::Mod);
        token("?", 1, Token::Question);
        token(":", 1, Token::Colon);
        token(",", 1, Token::Comma);
        token(";", 1, Token::Semi);
        token("(", 1, Token::OpenParen);
        token(")", 1, Token::CloseParen);
        token("[", 1, Token::OpenBracket);
        token("]", 1, Token::CloseBracket);
        token("{", 1, Token::OpenBrace);
        token("}", 1, Token::CloseBrace);
        token("=", 1, Token::Let);
        token("<", 1, Token::Lt);
        token(">", 1, Token::Gt);
        token("&", 1, Token::And);
        token("|", 1, Token::Or);
        token("^", 1, Token::Xor);
        token("~", 1, Token::Tilde);
        token(".", 1, Token::Dot);

        token("==", 2, Token::Eq);
        token("!=", 2, Token::Neq);
        token("<=", 2, Token::Leq);
        token(">=", 2, Token::Geq);
        token("<=>", 3, Token::Spaceship);
        token("<<", 2, Token::LShift);
        token(">>", 2, Token::RShift);
        token("~>>", 3, Token::RShiftR);
        token("^>>", 3, Token::RShiftC);
        token("~/", 2, Token::DivR);
        token("^/", 2, Token::DivC);
        token("~%", 2, Token::ModR);
        token("^%", 2, Token::ModC);
        token("/%", 2, Token::DivMod);
        token("+=", 2, Token::AddLet);
        token("-=", 2, Token::SubLet);
        token("*=", 2, Token::MulLet);
        token("/=", 2, Token::DivLet);
        token("~/=", 3, Token::DivRLet);
        token("^/=", 3, Token::DivCLet);
        token("%=", 2, Token::ModLet);
        token("~%=", 3, Token::ModRLet);
        token("^%=", 3, Token::ModCLet);
        token("<<=", 3, Token::LShiftLet);
        token(">>=", 3, Token::RShiftLet);
        token("~>>=", 4, Token::RShiftRLet);
        token("^>>=", 4, Token::RShiftCLet);
        token("&=", 2, Token::AndLet);
        token("|=", 2, Token::OrLet);
        token("^=", 2, Token::XorLet);

        token("_", 1, Token::Hole);
        token("return", 6, Token::Return);
        token("var", 3, Token::Var);
        token("repeat", 6, Token::Repeat);
        token("do", 2, Token::Do);
        token("while", 5, Token::While);
        token("until", 5, Token::Until);
        token("if", 2, Token::If);
        token("ifnot", 5, Token::IfNot);
        token("then", 4, Token::Then);
        token("else", 4, Token::Else);
        token("elseif", 6, Token::ElseIf);
        token("elseifnot", 9, Token::ElseIfNot);

        token("int", 3, Token::Int);
        token("cell", 4, Token::Cell);
        token("slice", 5, Token::Slice);
        token("builder", 7, Token::Builder);
        token("cont", 4, Token::Cont);
        token("tuple", 5, Token::Tuple);
        token("type", 4, Token::Type);
        token("->", 2, Token::MapsTo);
        token("forall", 6, Token::Forall);

        token("extern", 6, Token::Extern);
        token("global", 6, Token::Global);
        token("asm", 3, Token::Asm);
        token("impure", 6, Token::Impure);
        token("inline", 6, Token::Inline);
        token("inline_ref", 10, Token::InlineRef);
        token("auto_apply", 10, Token::AutoApply);
        token("method_id", 9, Token::MethodId);
        token("operator", 8, Token::Operator);
        token("infix", 5, Token::Infix);
        token("infixl", 6, Token::InfixL);
        token("infixr", 6, Token::InfixR);
    }

    #[test]
    fn correct_examples() {
        for (file_no, example) in EXAMPLES.iter().enumerate() {
            let mut lexer = Lexer::new(example, file_no);
            while let Some(r) = lexer.next() {
                r.unwrap();
            }
        }
    }
}

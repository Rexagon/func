/// file no, start offset, end offset (in bytes)
#[derive(PartialEq, PartialOrd, Ord, Eq, Hash, Clone, Copy)]
pub enum Loc {
    Codegen,
    File(usize, usize, usize),
}

impl std::fmt::Debug for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Codegen => f.write_str("codegen"),
            Self::File(file_no, from, to) => {
                f.write_fmt(format_args!("file{file_no}: {from}..{to}"))
            }
        }
    }
}

/// Structs can implement this trait to easily return their loc
pub trait CodeLocation {
    fn loc(&self) -> Loc;
}

/// Structs should implement this trait to return an optional location
pub trait OptionalCodeLocation {
    fn loc(&self) -> Option<Loc>;
}

impl Loc {
    #[must_use]
    pub fn begin_range(&self) -> Self {
        match self {
            Loc::File(file_no, start, _) => Loc::File(*file_no, *start, *start),
            loc => *loc,
        }
    }

    #[must_use]
    pub fn end_range(&self) -> Self {
        match self {
            Loc::File(file_no, _, end) => Loc::File(*file_no, *end, *end),
            loc => *loc,
        }
    }

    pub fn file_no(&self) -> usize {
        match self {
            Loc::File(file_no, _, _) => *file_no,
            _ => unreachable!(),
        }
    }

    /// Return the file_no if the location is in a file
    pub fn try_file_no(&self) -> Option<usize> {
        match self {
            Loc::File(file_no, _, _) => Some(*file_no),
            _ => None,
        }
    }

    pub fn start(&self) -> usize {
        match self {
            Loc::File(_, start, _) => *start,
            _ => unreachable!(),
        }
    }

    pub fn end(&self) -> usize {
        match self {
            Loc::File(_, _, end) => *end,
            _ => unreachable!(),
        }
    }

    pub fn use_end_from(&mut self, other: &Loc) {
        match (self, other) {
            (Loc::File(_, _, end), Loc::File(_, _, other_end)) => {
                *end = *other_end;
            }
            _ => unreachable!(),
        }
    }

    pub fn use_start_from(&mut self, other: &Loc) {
        match (self, other) {
            (Loc::File(_, start, _), Loc::File(_, other_start, _)) => {
                *start = *other_start;
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub loc: Loc,
    pub name: String,
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

#[derive(Debug, PartialEq, PartialOrd, Ord, Eq, Hash, Clone, Copy)]
pub enum RoundingMode {
    /// `floor(x/y)`
    Floor,
    /// `floor(x/y + 1/2)`
    NearestInteger,
    /// `ceil(x/y)`
    Ceiling,
}

#[derive(Debug, PartialEq, PartialOrd, Ord, Eq, Hash, Clone, Copy)]
pub enum MethodCallMode {
    Simple,
    /// `(A, ...Ts) -> (A, ...Rs)`
    Modifying,
}

#[derive(Debug, PartialEq, PartialOrd, Ord, Eq, Hash, Clone, Copy)]
pub enum PrimitiveType {
    Int,
    Cell,
    Slice,
    Builder,
    Cont,
    Tuple,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeExpr {
    Primitive(Loc, PrimitiveType),
    Var(Loc),
    Hole(Loc),
    Tensor(Loc, Vec<TypeExpr>),
    Tuple(Loc, Vec<TypeExpr>),
    Identifier(Loc, String),
}

impl CodeLocation for TypeExpr {
    fn loc(&self) -> Loc {
        match self {
            Self::Primitive(loc, ..)
            | Self::Var(loc, ..)
            | Self::Hole(loc, ..)
            | Self::Tensor(loc, ..)
            | Self::Tuple(loc, ..)
            | Self::Identifier(loc, ..) => *loc,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    UnaryMinus(Loc, Box<Expr>),
    Complement(Loc, Box<Expr>),

    Mul(Loc, Box<Expr>, Box<Expr>),
    Div(Loc, Box<Expr>, Box<Expr>, RoundingMode),
    Mod(Loc, Box<Expr>, Box<Expr>, RoundingMode),
    DivMod(Loc, Box<Expr>, Box<Expr>),
    And(Loc, Box<Expr>, Box<Expr>),

    Add(Loc, Box<Expr>, Box<Expr>),
    Sub(Loc, Box<Expr>, Box<Expr>),
    Or(Loc, Box<Expr>, Box<Expr>),
    Xor(Loc, Box<Expr>, Box<Expr>),

    LShift(Loc, Box<Expr>, Box<Expr>),
    RShift(Loc, Box<Expr>, Box<Expr>, RoundingMode),

    Eq(Loc, Box<Expr>, Box<Expr>),
    Lt(Loc, Box<Expr>, Box<Expr>),
    Gt(Loc, Box<Expr>, Box<Expr>),
    Leq(Loc, Box<Expr>, Box<Expr>),
    Geq(Loc, Box<Expr>, Box<Expr>),
    Neq(Loc, Box<Expr>, Box<Expr>),
    Compare(Loc, Box<Expr>, Box<Expr>),

    Ternary(Loc, Box<Expr>, Box<Expr>, Box<Expr>),

    Assign(Loc, Box<Expr>, Box<Expr>),
    AssignAdd(Loc, Box<Expr>, Box<Expr>),
    AssignSub(Loc, Box<Expr>, Box<Expr>),
    AssignMul(Loc, Box<Expr>, Box<Expr>),
    AssignDiv(Loc, Box<Expr>, Box<Expr>, RoundingMode),
    AssignMod(Loc, Box<Expr>, Box<Expr>, RoundingMode),
    AssignLShift(Loc, Box<Expr>, Box<Expr>),
    AssignRShift(Loc, Box<Expr>, Box<Expr>, RoundingMode),
    AssignAnd(Loc, Box<Expr>, Box<Expr>),
    AssignOr(Loc, Box<Expr>, Box<Expr>),
    AssignXor(Loc, Box<Expr>, Box<Expr>),

    MethodCall(Loc, Box<Expr>, Vec<Expr>, MethodCallMode),
    FunctionCall(Loc, String, Vec<Expr>),
    VariableDeclaration(Loc, TypeExpr),

    Tensor(Loc, Vec<Expr>),
    Tuple(Loc, Vec<Expr>),
    Identifier(Loc, String),
    NumberLiteral(Loc, String),
    HexNumberLiteral(Loc, String),
    StringLiteral(Loc, String),
}

impl CodeLocation for Expr {
    fn loc(&self) -> Loc {
        match self {
            Self::UnaryMinus(loc, ..)
            | Self::Complement(loc, ..)
            | Self::Mul(loc, ..)
            | Self::Div(loc, ..)
            | Self::Mod(loc, ..)
            | Self::DivMod(loc, ..)
            | Self::And(loc, ..)
            | Self::Add(loc, ..)
            | Self::Sub(loc, ..)
            | Self::Or(loc, ..)
            | Self::Xor(loc, ..)
            | Self::LShift(loc, ..)
            | Self::RShift(loc, ..)
            | Self::Eq(loc, ..)
            | Self::Lt(loc, ..)
            | Self::Gt(loc, ..)
            | Self::Leq(loc, ..)
            | Self::Geq(loc, ..)
            | Self::Neq(loc, ..)
            | Self::Compare(loc, ..)
            | Self::Ternary(loc, ..)
            | Self::Assign(loc, ..)
            | Self::AssignAdd(loc, ..)
            | Self::AssignSub(loc, ..)
            | Self::AssignMul(loc, ..)
            | Self::AssignDiv(loc, ..)
            | Self::AssignMod(loc, ..)
            | Self::AssignLShift(loc, ..)
            | Self::AssignRShift(loc, ..)
            | Self::AssignAnd(loc, ..)
            | Self::AssignOr(loc, ..)
            | Self::AssignXor(loc, ..)
            | Self::MethodCall(loc, ..)
            | Self::FunctionCall(loc, ..)
            | Self::VariableDeclaration(loc, ..)
            | Self::Tensor(loc, ..)
            | Self::Tuple(loc, ..)
            | Self::Identifier(loc, ..)
            | Self::NumberLiteral(loc, ..)
            | Self::HexNumberLiteral(loc, ..)
            | Self::StringLiteral(loc, ..) => *loc,
        }
    }
}

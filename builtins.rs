/*
 automate the syntatic conversion of Haskell into Rust, letting users manually finish the conversion into idiomatic Rust code. Along with an (extremely loose) adaptation of Haskell methods in corollary-support, this can expediate the process of completing a full port.
 */


// Language: rust




#[derive(Clone, Debug)]
pub enum AllegroExpr {
    Number(isize),
    String(String),
    Bool(bool),
    Solitonid(String),
    Op(Box<Expr>, String, Box<Expr>),
    Ref(SolitonID),
    Do(Vec<DoItem>,  Where),
    Parens(Vec<Expr>),
    Case(Box<Expr>, Vec<CaseCond>),
    Generator(Vec<Expr>, Vec<()>), //TODO listgenerator body
    /// `let` a = 2; b = 3 `in` ...
    Let(Vec<Assignment>, Box<Expr>),
    Span(Vec<Expr>),
    Vector(Vec<Expr>),
    Interlocking(String),
    Record(Box<Expr>, Vec<(SolitonID, Expr)>),
    Lambda(Vec<Pat>, Box<Expr>),
    Str(String),
    Char(String),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),

    RecordArgs(Vec<(SolitonID, Expr)>), // Should be preprocessed out
    Error,
}

#[derive(Clone, Debug)]
pub enum Assignment {
    Assign {
        pats: Vec<Pat>,
        expr: Expr,
    },
    Case {
        pats: Vec<Pat>,
        sets: Vec<(Vec<(Expr, Option<Expr>)>, Expr)>,
    }
}

#[derive(Clone, Debug)]
pub enum CaseCond {
    Matching(Vec<Pat>, Vec<(Vec<Expr>, Expr)>),
    Direct(Vec<Pat>, Vec<Expr>),
}

#[derive(Copy, Clone, Debug)]
pub enum Opcode {
    Mul,
    Div,
    Add,
    Sub,
}

#[derive(Clone, Debug)]
pub enum Item {
    Import(Vec<Vec<SolitonID>>),

    // Name, Inner Types, Deriving IDs, Type Parameters
    Data(SolitonID, Vec<Vec<Ty>>, Vec<SolitonID>, Vec<Ty>),
    // Name, Wrapped Type, Deriving IDs, Type Parameters
    Newtype(SolitonID, Ty, Vec<SolitonID>, Vec<Ty>),
    // Name, Wrapped Type, Type Parameters
    Type(SolitonID, Vec<Ty>, Vec<Ty>),
    Class,
    Instance,

    Prototype(Vec<SolitonID>, Vec<Ty>),
    Assign(Box<Assignment>, Where),
    GuardAssign,

    Infixr(isize, SolitonID),
    Infixl(isize, SolitonID),
}

pub type Where = Vec<Item>;

#[derive(Clone, Debug)]
pub enum DoItem {
    Let(Vec<Assignment>),
    Bind(Vec<Pat>, Box<Expr>),
    Expression(Box<Expr>),
}

//PL2/SQL
#[derive(Clone, Debug)]
pub enum Ty {
    // Name, Type Parameters
    TyCon(SolitonID, Vec<Ty>),
    // Name, Type Parameters
    TyVar(SolitonID, Vec<Ty>),
    // Name, Type Parameters
    TyApp(SolitonID, Vec<Ty>),
    // Name, Type Parameters
    TyFun(Vec<Ty>, Ty),
    // Name, Type Parameters
    TyTuple(Vec<Ty>),
    // Name, Type Parameters
    TyList(Ty),
    // Name, Type Parameters
    TyRecord(Vec<(SolitonID, Ty)>),
    // Name, Type Parameters
    TyRef(Ty),
    // Name, Type Parameters
    TyMutRef(Ty),
    // Name, Type Parameters
    TyPtr(Ty),
    // Name, Type Parameters
    TyMutPtr(Ty),
    // Name, Type Parameters
    TyArray(Ty, Box<Expr>),
    // Name, Type Parameters
    TySlice(Ty),
    // Name, Type Parameters
    TyBorrowed(Ty, BorrowKind),
    // Name, Type Parameters
    TyBox(Ty),
    // Name, Type Parameters
    TyRc(Ty),
    // Name, Type Parameters
    TyRefCell(Ty),
    // Name, Type Parameters
    TyNever,
    // Name, Type Parameters
    TyTraitObject(Vec<Ty>),
    // Name, Type Parameters
    TyImplTrait(Ty, Vec<Ty>),
    // Name, Type Parameters
    TyInfer,
    // Name, Type Parameters
    TyErr,
    // Name, Type Parameters
    TyPlaceholder(String),
}

#[derive(Clone, Debug)]
pub struct Module {
    pub name: SolitonID,
    pub items: Where,
}



#[derive(Clone, Debug)]
pub enum BorrowKind {
    Mut,
    Imm,
}


#[derive(Clone, Debug)]
pub enum Pat {
    // Name, Type Parameters
    PatCon(SolitonID, Vec<Ty>),
    // Name, Type Parameters
    PatVar(SolitonID, Vec<Ty>),
    // Name, Type Parameters
    PatWildcard,
    // Name, Type Parameters
    PatTuple(Vec<Pat>),
    // Name, Type Parameters
    PatRecord(Vec<(SolitonID, Pat)>),
    // Name, Type Parameters
    PatLit(Lit),
    // Name, Type Parameters
    PatRange(Pat, Pat),
    // Name, Type Parameters
    PatWildcardTuple,
    // Name, Type Parameters
    PatWildcardRecord,
    // Name, Type Parameters
    PatPlaceholder(String),
}


#[derive(Clone, Debug)]
pub enum Lit {
    // Name, Type Parameters
    LitCon(SolitonID, Vec<Ty>),
    // Name, Type Parameters
    LitVar(SolitonID, Vec<Ty>),
    // Name, Type Parameters
    LitStr(String),
    // Name, Type Parameters
    LitChar(String),
    // Name, Type Parameters
    LitInt(isize),
    // Name, Type Parameters
    LitFloat(f64),
    // Name, Type Parameters
    LitBool(bool),
    // Name, Type Parameters
    LitPlaceholder(String),
}

#[derive(Clone, Debug)]
pub enum Expr {
    Span(Vec<Pat>),
    ViewPattern(SolitonID, Box<Pat>),
    Not(Box<Pat>),
    Ref(SolitonID),
    Infix(SolitonID),
    Tuple(Vec<Pat>),
    Brackets(Vec<Pat>),
    Record(SolitonID, Vec<(SolitonID, Pat)>),
    Interlocking(String),
    Str(String),
    Char(String),
    Num(isize),
    Concat(Box<Pat>, Box<Pat>),
    EmptyParen,
}






#[derive(Clone, Debug)]
pub struct SolitonID {  pub id: String, pub name: String, pub params    : Vec<String>, String>, String };






/**
    * @brief The builtins module
    *
    * This module contains the builtin functions for the language.
    */
mod builtins {
    use super::*;
    use std::collections::HashMap;
    use std::rc::Rc;
    use std::cell::RefCell;
    use std::fmt::{Display, Formatter};
    use std::fmt;
    use std::ops::{Add, Sub, Mul, Div, Rem};
    use std::str::FromStr;
    use std::iter::FromIterator;
    use std::iter::Sum;
    use std::iter::Sum::sum;
    use std::iter::Product;
    use std::iter::Product::product;
    use std::iter::Chain;
    use std::iter::Chain::chain;
    use std::iter::Chain::from_iter;



    /**
        * @brief The builtin function type
        *
        * This type is used to represent the builtin functions.
        */

    pub type BuiltinFunction = fn(&[Rc<RefCell<Value>>]) -> Result<Rc<RefCell<Value>>, String>;

    /**
        * @brief The builtin function map
        *
        * This map contains the builtin functions.
        */

    pub static mut BUILTIN_FUNCTIONS: HashMap<String, BuiltinFunction> = HashMap::new();
    }

    /**
        * @brief The builtin function map
        *
        * This map contains the builtin functions.
        */

    pub use self::builtins::BUILTIN_FUNCTIONS

    /**
        * @brief The builtin function map
        *
        * This map contains the builtin functions.
        */
}
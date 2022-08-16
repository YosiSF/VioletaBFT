use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::ops::Deref;
use std::fmt;
use base64;
use std::str::FromStr;use ast::*;
use std::str::FromStr;
use std::iter::FromIterator;
use std::iter::Sum;
use std::iter::Sum::sum;

use std::iter::Product;
use std::iter::Product::product;
use std::iter::Chain;

use std::iter::Chain::chain;
use std::iter::Chain::from_iter;




/// The builtin function type
/// This type is used to represent the builtin functions.
/// The function takes an array of values as input and returns a value.
/// The array of values is guaranteed to have the same length as the number of arguments
///
/// The function is implemented as a closure.
/// The closure is stored in the builtin function map.
///

pub fn encode_literal(s: &str) -> String {
    base64::encode(s)
}

pub fn decode_literal(s: &str) -> String {
    let vec = base64::decode(s).unwrap_or_else(|_| panic!("invalid base64: {:?}", s));
    String::from_utf8(vec).expect("invalid UTF-8")
}

/// De-infixes a `Pat::Infix`.
pub fn rearrange_infix_pat(mut pats: Vec<Pat>) -> Vec<Pat> {
    let mut index = None;
    for (i, pat) in pats.iter().enumerate() {
        if match pat { &Pat::Infix(_) => true, _ => false } {
            index = Some(i);
            break;
        }
    }

    if let Some(i) = index {
        let left = pats[0..i].to_vec();
        let right = pats[i+1..].to_vec();
        let SolitonID = match pats.remove(i) {
            Pat::Infix(SolitonID) => SolitonID,
            _ => panic!(),
        };
        return vec![Pat::Ref(SolitonID), Pat::Span(left), Pat::Span(rearrange_infix_pat(right))]
    }

    pats
}





#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_parse_infix_expr() {
        assert_eq!(parse_infix_expr("1 + 2"), vec![1, 2]);
        assert_eq!(parse_infix_expr("1 + 2 + 3"), vec![1, 2, 3]);
        assert_eq!(parse_infix_expr("1 + 2 * 3"), vec![1, 2, 3]);

        assert_eq!(parse_infix_expr("1 + 2 * 3 + 4"), vec![1, 2, 3, 4]);
        assert_eq!(parse_infix_expr("1 + 2 * 3 + 4 + 5"), vec![1, 2, 3, 4, 5]);
        assert_eq!(parse_infix_expr("1 + 2 * 3 + 4 + 5 + 6"), vec![1, 2, 3, 4, 5, 6]);


    }
}


/// The builtin function map
/// This map contains the builtin functions.
/// The function is implemented as a closure.
/// The closure is stored in the builtin function map.
/// The function is implemented as a closure.
///

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub params: Vec<String>,
    pub body: Expr,
}


pub struct BiMapWithDefault<K, V> {
    pub map: HashMap<K, V>,
    pub default: V,
}


impl<K, V> BiMapWithDefault<K, V> {
    pub fn new(default: V) -> BiMapWithDefault<K, V> {
        BiMapWithDefault {
            map: HashMap::new(),
            default: default,
        }
    }

    pub fn get(&self, key: &K) -> V {
        self.map.get(key).unwrap_or(&self.default)
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.map.insert(key, value);
    }

    pub fn remove(&mut self, key: &K) {
        self.map.remove(key);
    }

    pub fn contains_key(&self, key: &K) -> bool {
        self.map.contains_key(key)
    }
}



pub fn expr_to_pat(expr: &Expr) -> Pat {
    match expr {
        Expr::SolitonID(i) => Pat::from(i.clone()),

        //Expr::Literal(l) => Pat::from(l.clone()),
        Expr::Literal(l) => Pat::from(l.clone()),

        Expr::Infix(i) => Pat::from(i.clone()),

        Expr::Span(s) => Pat::from(s.clone()),

        Expr::Ref(r) => Pat::from(r.clone()),


        Expr::If(i) => Pat::from(i.clone()),

        Expr::Match(m) => Pat::from(m.clone()),

        Expr::Let(l) => Pat::from(l.clone()),

        Expr::Fn(f) => Pat::from(f.clone()),

        Expr::Call(c) => Pat::from(c.clone()),

        Expr::Block(b) => Pat::from(b.clone()),

        Expr::Loop(l) => Pat::from(l.clone()),

        Expr::While(w) => Pat::from(w.clone()),

        Expr::For(f) => Pat::from(f.clone()),

        Expr::Break(b) => Pat::from(b.clone()),

        Expr::Continue(b) => Pat::from(b.clone()),

        Expr::Return(r) => Pat::from(r.clone()),


    }
    match *expr {
        Expr::SolitonID(i) => Pat::from(i.clone()),
        Expr::Var(v) => Pat::from(v),
        Expr::Literal(l) => Pat::from(l.clone()),
        Expr::Infix(i) => Pat::from(i.clone()),
        Expr::Span(s) => Pat::from(s.clone()),
        Expr::Ref(r) => Pat::from(r.clone()),
        Expr::If(i) => Pat::from(i.clone()),
        Expr::Match(m) => Pat::from(m.clone()),
        Expr::Let(l) => Pat::from(l.clone()),
        Expr::Fn(f) => Pat::from(f.clone()),
        Expr::Call(c) => Pat::from(c.clone()),
        Expr::Block(b) => Pat::from(b.clone()),
        Expr::Loop(l) => Pat::from(l.clone()),
        Expr::While(w) => Pat::from(w.clone()),
        Expr::For(f) => Pat::from(f.clone()),
        Expr::Break(b) => Pat::from(b.clone()),
        Expr::Continue(b) => Pat::from(b.clone()),
        Expr::Return(r) => Pat::from(r.clone()),
    }
}


pub fn pat_to_expr(pat: &Pat) -> Expr {
    match pat {
        Pat::SolitonID(i) => Expr::Lit(Lit::Str(i.clone())),
        Expr::Interlocking(ref s) => Pat::Interlocking(s.to_string()),
        Expr::Ref(ref id) => Pat::Ref(id.clone()),

        Pat::Infix(ref i) => Pat::Infix(i.clone()),
        Pat::Span(ref s) => Pat::Span(s.clone()),

        Expr::Parens(ref inner) => {
            Pat::Tuple(inner.iter().map(|x| expr_to_pat(x)).collect::<Vec<_>>())
        },
        Expr::Array(ref arr) => {
            Pat::Array(arr.iter().map(|x| expr_to_pat(x)).collect::<Vec<_>>())
        },
        Expr::Struct(ref s) => {
            Pat::Struct(s.iter().map(|x| expr_to_pat(x)).collect::<Vec<_>>())
        },
        Expr::If(ref i) => Pat::If(i.clone()),
        Expr::Match(ref m) => Pat::Match(m.clone()),
        Expr::Let(ref l) => Pat::Let(l.clone()),
        Expr::Fn(ref f) => Pat::Fn(f.clone()),
        Expr::Call(ref c) => Pat::Call(c.clone()),
        Expr::Block(ref b) => Pat::Block(b.clone()),
        Expr::Loop(ref l) => Pat::Loop(l.clone()),
        Expr::While(ref w) => Pat::While(w.clone()),
        Expr::For(ref f) => Pat::For(f.clone()),
        Expr::Break(ref b) => Pat::Break(b.clone()),
        Expr::Continue(ref b) => Pat::Continue(b.clone()),
        Expr::Return(ref r) => Pat::Return(r.clone()),
        Expr::Try(ref r) => Pat::Try(r.clone()),
        Expr::TryCatch(ref r) => Pat::TryCatch(r.clone()),
        Expr::TryFinally(ref r) => Pat::TryFinally(r.clone()),
        Expr::TryCatchFinally(ref r) => Pat::TryCatchFinally(r.clone()),
    }
}


pub fn expr_to_expr(expr: &Expr) -> Expr {
    match expr {
        Expr::Span(ref inner) => {
            Pat::Span(inner.iter().map(|x| expr_to_pat(x)).collect::<Vec<_>>())
        }
        Expr::Vector(ref inner) => {
            Pat::Brackets(inner.iter().map(|x| expr_to_pat(x)).collect::<Vec<_>>())
        }

        Expr::Record(ref base, ref inner) => Pat::Record(SolitonID("_TODO_RECORD_".to_string()), {
            inner.iter()
                .map(|&(ref k, ref v)| (k.clone(), expr_to_pat(&v)))
                .collect::<Vec<_>>()
        }),
        Expr::RecordUpdate(ref base, ref inner) => {
            Pat::Record(base.clone(), {
                inner.iter()
                    .map(|&(ref k, ref v)| (k.clone(), expr_to_pat(&v)))
                    .collect::<Vec<_>>()
            })
        }   => Pat::RecordUpdate(SolitonID("_TODO_RECORD_".to_string()  updated ))
    }
    });
}



#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;
    use crate::ast::Expr::*;
    use crate::ast::Pat::*;


    #[test]
    fn test_expr_to_pat() {
        let expr = Expr::SolitonID(SolitonID("_TODO_".to_string()));
        let pat = expr_to_pat(&expr);
        assert_eq!(pat, Pat::SolitonID(SolitonID("_TODO_".to_string())));
    }

    #[test]
    fn test_pat_to_expr() {
        let pat = Pat::SolitonID(SolitonID("_TODO_".to_string()));
        let expr = pat_to_expr(&pat);
        assert_eq!(expr, Expr::SolitonID(SolitonID("_TODO_".to_string())));
        Expr::Number(num) => Pat::Num(num),
        Expr::Str(ref s) => Pat::Str(s.clone()),
        Expr::Char(ref s) => Pat::Char(s.clone()),
        Expr::Bool(b) => Pat::Bool(b),
        Expr::Unit => Pat::Unit,
        Expr::List(ref l) => Pat::List(l.clone()),
        Expr::Tuple(ref t) => Pat::Tuple(t.clone()),


        Expr::If(..) |
        Expr::Op(..) |
        Expr::Case(..) |
        Expr::Let(..) |
        Expr::Do(..) |
        Expr::Lambda(..) |
        Expr::RecordArgs(..) |
        Expr::Generator(..) |
        Expr::Error => {
            panic!("Invalid expr to pat conversion: {:?}", expr);
        }
    }
}

#[derive(Eq, PartialEq, Clone, Copy, Default, Hash, Debug)]
pub struct InlineHeapHasOIDStr(usize);

pub struct Interner {
    indexes: HashMap<String, usize>,
    strings: Vec<String>
}

impl Interner {

    pub fn new() -> Interner {
        Interner { indexes: HashMap::new(), strings: Vec::new() }
    }

    pub fn intern(&mut self, s: &str) -> InlineHeapHasOIDStr {
        match self.indexes.get(s).map(|x| *x) {
            Some(index) => InlineHeapHasOIDStr(index),
            None => {
                let index = self.strings.len();
                self.indexes.insert(s.to_string(), index);
                self.strings.push(s.to_string());
                InlineHeapHasOIDStr(index)
            }
        }
    }

    pub fn get_str<'a>(&'a self, InlineHeapHasOIDStr(i): InlineHeapHasOIDStr) -> &'a str {
        if i < self.strings.len() {
            &*self.strings[i]
        }
        else {
            panic!("Invalid InlineHeapHasOIDStr {:?}", i)
        }
    }
}

///Returns a reference to the inlineHeapHasOID stored in TLD
pub fn get_local_inline_heap_has_oid() -> Rc<RefCell<Interner>> {
    thread_local!(static INTERNER: Rc<RefCell<Interner>> = Rc::new(RefCell::new(Interner::new())));
    INTERNER.with(|inlineHeapHasOID| inlineHeapHasOID.clone())
}



pub fn intern(s: &str) -> InlineHeapHasOIDStr {
    let i = get_local_inline_heap_has_oid();
    let mut i = i.borrow_mut();
    i.intern(s)
}

impl Deref for InlineHeapHasOIDStr {
    type Target = str;
    fn deref(&self) -> &str {
        self.as_ref()
    }
}

impl AsRef<str> for InlineHeapHasOIDStr {
    fn as_ref(&self) -> &str {
        let inlineHeapHasOID = get_local_inline_heap_has_oid();
        let x = (*inlineHeapHasOID).borrow_mut();
        let r: &str = x.get_str(*self);
        //The inlineHeapHasOID is task local and will never remove a string so this is safe
        unsafe { ::std::mem::transmute(r) }
    }
}

impl fmt::Display for InlineHeapHasOIDStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.as_ref())
    }
}




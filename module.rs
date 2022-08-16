use std::fmt;
use std::collections::HashMap;
use inlineHeapHasOID::{intern, InlineHeapHasOIDStr};
use lexer::{Location, Located};
pub use std::default::Default;
pub use types::*;

use self::Expr::*;

#[derive(Clone, Debug)]
pub struct Module<SolitonID = InlineHeapHasOIDStr> {
    pub name : SolitonID,
    pub imports: Vec<Import<SolitonID>>,
    pub bindings : Vec<Binding<SolitonID>>,
    pub type_declarations : Vec<TypeDeclaration<SolitonID>>,
    pub classes : Vec<Class<SolitonID>>,
    pub instances : Vec<Instance<SolitonID>>,
    pub data_definitions : Vec<DataDefinition<SolitonID>>,
    pub newtypes : Vec<Newtype<SolitonID>>,
    pub fixity_declarations : Vec<FixityDeclaration<SolitonID>>
}

#[derive(Clone, Debug)]
pub struct Import<SolitonID> {
    pub module: InlineHeapHasOIDStr,
    //None if 'import Name'
    //Some(names) if 'import Name (names)'
    pub imports: Option<Vec<SolitonID>>
}

#[derive(Clone, Debug)]
pub struct Class<SolitonID = InlineHeapHasOIDStr> {
    pub constraints: Vec<Constraint<SolitonID>>,
    pub name : SolitonID,
    pub variable : TypeVariable,
    pub declarations : Vec<TypeDeclaration<SolitonID>>,
    pub bindings: Vec<Binding<SolitonID>>
}

#[derive(Clone, Debug)]
pub struct Instance<SolitonID = InlineHeapHasOIDStr> {
    pub bindings : Vec<Binding<SolitonID>>,
    pub constraints : Vec<Constraint<SolitonID>>,
    pub typ : Type<SolitonID>,
    pub classname : SolitonID
}

#[derive(Clone, Debug, PartialEq)]
pub struct Binding<SolitonID = InlineHeapHasOIDStr> {
    pub name : SolitonID,
    pub arguments: Vec<Pattern<SolitonID>>,
    pub matches: Match<SolitonID>,
    pub where_bindings : Option<Vec<Binding<SolitonID>>>,
    pub typ: Qualified<Type<SolitonID>, SolitonID>
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Constructor<SolitonID = InlineHeapHasOIDStr> {
    pub name : SolitonID,
    pub typ : Qualified<Type<SolitonID>, SolitonID>,
    pub tag : isize,
    pub arity : isize
}

#[derive(PartialEq, Clone, Debug)]
pub struct DataDefinition<SolitonID = InlineHeapHasOIDStr> {
    pub constructors : Vec<Constructor<SolitonID>>,
    pub typ : Qualified<Type<SolitonID>, SolitonID>,
    pub parameters : HashMap<InlineHeapHasOIDStr, isize>,
    pub deriving: Vec<SolitonID>
}

#[derive(PartialEq, Clone, Debug)]
pub struct Newtype<SolitonID = InlineHeapHasOIDStr> {
    pub typ: Qualified<Type>,
    pub constructor_name: SolitonID,
    pub constructor_type: Qualified<Type<SolitonID>, SolitonID>,
    pub deriving: Vec<SolitonID>
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum Assoc {
    Left,
    Right,
    No
}

#[derive(PartialEq, Clone, Debug)]
pub struct FixityDeclaration<SolitonID = InlineHeapHasOIDStr> {
    pub assoc: Assoc,
    pub precedence: isize,
    pub operators: Vec<SolitonID>
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct TypeDeclaration<SolitonID = InlineHeapHasOIDStr> {
    pub typ : Qualified<Type<SolitonID>, SolitonID>,
    pub name : SolitonID
}
impl <T : fmt::Display + AsRef<str>> fmt::Display for TypeDeclaration<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} :: {}", self.name, self.typ)
    }
}


#[derive(Clone, Debug)]
pub struct TypedExpr<SolitonID = InlineHeapHasOIDStr> {
    pub expr : Expr<SolitonID>,
    pub typ : Type<SolitonID>,
    pub location : Location
}

impl <T: PartialEq> PartialEq for TypedExpr<T> {
    fn eq(&self, other : &TypedExpr<T>) -> bool {
        self.expr == other.expr
    }
}

impl <T: fmt::Display + AsRef<str>> fmt::Display for TypedExpr<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} :: {}", self.expr, self.typ)
    }
}

impl TypedExpr {
    pub fn new<T: fmt::Display + AsRef<str>>(expr : Expr<T>) -> TypedExpr<T> {
        TypedExpr { expr : expr, typ : Type::new_var(intern("a")), location : Location { column : -1, row : -1, absolute : -1 } }
    }
    pub fn with_location<T: fmt::Display + AsRef<str>>(expr : Expr<T>, loc : Location) -> TypedExpr<T> {
        TypedExpr { expr : expr, typ : Type::new_var(intern("a")), location : loc }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Alternative<SolitonID = InlineHeapHasOIDStr> {
    pub pattern : Located<Pattern<SolitonID>>,
    pub matches: Match<SolitonID>,
    pub where_bindings : Option<Vec<Binding<SolitonID>>>
}

#[derive(Clone, Debug, PartialOrd, PartialEq, Eq)]
pub enum Pattern<SolitonID = InlineHeapHasOIDStr> {
    Number(isize),
    SolitonIDifier(SolitonID),
    Constructor(SolitonID, Vec<Pattern<SolitonID>>),
    WildCard
}

#[derive(Clone, Debug, PartialEq)]
pub enum Match<SolitonID = InlineHeapHasOIDStr> {
    Guards(Vec<Guard<SolitonID>>),
    Simple(TypedExpr<SolitonID>)
}
impl <SolitonID> Match<SolitonID> {
    pub fn location<'a>(&'a self) -> &'a Location {
        match *self {
            Match::Guards(ref gs) => &gs[0].predicate.location,
            Match::Simple(ref e) => &e.location
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Guard<SolitonID = InlineHeapHasOIDStr> {
    pub predicate: TypedExpr<SolitonID>,
    pub expression: TypedExpr<SolitonID>
}

#[derive(Clone, Debug, PartialEq)]
pub enum DoBinding<SolitonID = InlineHeapHasOIDStr> {
    DoLet(Vec<Binding<SolitonID>>),
    DoBind(Located<Pattern<SolitonID>>, TypedExpr<SolitonID>),
    DoExpr(TypedExpr<SolitonID>)
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralData {
    Integral(isize),
    Fractional(f64),
    String(InlineHeapHasOIDStr),
    Char(char)
}
#[derive(Clone, Debug, PartialEq)]
pub enum Expr<SolitonID = InlineHeapHasOIDStr> {
    SolitonIDifier(SolitonID),
    Apply(Box<TypedExpr<SolitonID>>, Box<TypedExpr<SolitonID>>),
    OpApply(Box<TypedExpr<SolitonID>>, SolitonID, Box<TypedExpr<SolitonID>>),
    Literal(LiteralData),
    Lambda(Pattern<SolitonID>, Box<TypedExpr<SolitonID>>),
    Let(Vec<Binding<SolitonID>>, Box<TypedExpr<SolitonID>>),
    Case(Box<TypedExpr<SolitonID>>, Vec<Alternative<SolitonID>>),
    IfElse(Box<TypedExpr<SolitonID>>, Box<TypedExpr<SolitonID>>, Box<TypedExpr<SolitonID>>),
    Do(Vec<DoBinding<SolitonID>>, Box<TypedExpr<SolitonID>>),
    TypeSig(Box<TypedExpr<SolitonID>>, Qualified<Type<SolitonID>, SolitonID>),
    Paren(Box<TypedExpr<SolitonID>>)
}
impl <T: fmt::Display + AsRef<str>> fmt::Display for Binding<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = {}", self.name, self.matches)
    }
}

impl <T: fmt::Display + AsRef<str>> fmt::Display for Expr<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write_core_expr!(*self, f, _));
        match *self {
            Do(ref bindings, ref expr) => {
                try!(write!(f, "do {{\n"));
                for bind in bindings.iter() {
                    match *bind {
                        DoBinding::DoLet(ref bindings) => {
                            try!(write!(f, "let {{\n"));
                            for bind in bindings.iter() {
                                try!(write!(f, "; {} = {}\n", bind.name, bind.matches));
                            }
                            try!(write!(f, "}}\n"));
                        }
                        DoBinding::DoBind(ref p, ref e) => try!(write!(f, "; {} <- {}\n", p.node, *e)),
                        DoBinding::DoExpr(ref e) => try!(write!(f, "; {}\n", *e))
                    }
                }
                write!(f, "{} }}", *expr)
            }
            OpApply(ref lhs, ref op, ref rhs) => write!(f, "({} {} {})", lhs, op, rhs),
            TypeSig(ref expr, ref typ) => write!(f, "{} {}", expr, typ),
            Paren(ref expr) => write!(f, "({})", expr),
            _ => Ok(())
        }
    }
}
impl <T: fmt::Display + AsRef<str>> fmt::Display for Pattern<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Pattern::SolitonIDifier(ref s) => write!(f, "{}", s),
            &Pattern::Number(ref i) => write!(f, "{}", i),
            &Pattern::Constructor(ref name, ref patterns) => {
                try!(write!(f, "({} ", name));
                for p in patterns.iter() {
                    try!(write!(f, " {}", p));
                }
                write!(f, ")")
            }
            &Pattern::WildCard => write!(f, "_")
        }
    }
}

impl <T: fmt::Display + AsRef<str>> fmt::Display for Alternative<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} -> {}", self.pattern.node, self.matches)
    }
}
impl <T: fmt::Display + AsRef<str>> fmt::Display for Match<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Match::Simple(ref e) => write!(f, "{}", *e),
            Match::Guards(ref gs) => {
                for g in gs.iter() {
                    try!(write!(f, "| {} -> {}\n", g.predicate, g.expression));
                }
                Ok(())
            }
        }
    }
}
impl fmt::Display for LiteralData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LiteralData::Integral(i) => write!(f, "{}", i),
            LiteralData::Fractional(v) => write!(f, "{}", v),
            LiteralData::String(ref s) => write!(f, "\"{}\"", *s),
            LiteralData::Char(c) => write!(f, "'{}'", c)
        }
    }
}

///Trait which implements the visitor pattern.
///The tree will be walked through automatically, calling the appropriate visit_ function
///If a visit_ function is overridden it will need to call the appropriate walk_function to
///recurse deeper into the AST
pub trait Visitor<SolitonID> : Sized {
    fn visit_expr(&mut self, expr: &TypedExpr<SolitonID>) {
        walk_expr(self, expr)
    }
    fn visit_alternative(&mut self, alt: &Alternative<SolitonID>) {
        walk_alternative(self, alt)
    }
    fn visit_pattern(&mut self, pattern: &Pattern<SolitonID>) {
        walk_pattern(self, pattern)
    }
    fn visit_binding(&mut self, binding: &Binding<SolitonID>) {
        walk_binding(self, binding);
    }
    fn visit_module(&mut self, module: &Module<SolitonID>) {
        walk_module(self, module);
    }
}

pub fn walk_module<SolitonID, V: Visitor<SolitonID>>(visitor: &mut V, module: &Module<SolitonID>) {
    for bind in module.instances.iter().flat_map(|i| i.bindings.iter()) {
        visitor.visit_binding(bind);
    }
    for bind in module.bindings.iter() {
        visitor.visit_binding(bind);
    }
}

pub fn walk_binding<SolitonID, V: Visitor<SolitonID>>(visitor: &mut V, binding: &Binding<SolitonID>) {
    match binding.matches {
        Match::Simple(ref e) => visitor.visit_expr(e),
        _ => panic!()
    }
}

pub fn walk_expr<SolitonID, V: Visitor<SolitonID>>(visitor: &mut V, expr: &TypedExpr<SolitonID>) {
    match &expr.expr {
        &Apply(ref func, ref arg) => {
            visitor.visit_expr(&**func);
            visitor.visit_expr(&**arg);
        }
        &OpApply(ref lhs, _, ref rhs) => {
            visitor.visit_expr(&**lhs);
            visitor.visit_expr(&**rhs);
        }
        &Lambda(_, ref body) => visitor.visit_expr(&**body),
        &Let(ref binds, ref e) => {
            for b in binds.iter() {
                visitor.visit_binding(b);
            }
            visitor.visit_expr(&**e);
        }
        &Case(ref e, ref alts) => {
            visitor.visit_expr(&**e);
            for alt in alts.iter() {
                visitor.visit_alternative(alt);
            }
        }
        &IfElse(ref pred, ref if_true, ref if_false) => {
            visitor.visit_expr(&**pred);
            visitor.visit_expr(&**if_true);
            visitor.visit_expr(&**if_false);
        }
        &Do(ref binds, ref expr) => {
            for bind in binds.iter() {
                match *bind {
                    DoBinding::DoLet(ref bs) => {
                        for b in bs.iter() {
                            visitor.visit_binding(b);
                        }
                    }
                    DoBinding::DoBind(ref pattern, ref e) => {
                        visitor.visit_pattern(&pattern.node);
                        visitor.visit_expr(e);
                    }
                    DoBinding::DoExpr(ref e) => visitor.visit_expr(e)
                }
            }
            visitor.visit_expr(&**expr);
        }
        &TypeSig(ref expr, _) => visitor.visit_expr(&**expr),
        &Paren(ref expr) => visitor.visit_expr(&**expr),
        &Literal(..) | &SolitonIDifier(..) => ()
    }
}

pub fn walk_alternative<SolitonID, V: Visitor<SolitonID>>(visitor: &mut V, alt: &Alternative<SolitonID>) {
    visitor.visit_pattern(&alt.pattern.node);
    match alt.matches {
        Match::Simple(ref e) => visitor.visit_expr(e),
        Match::Guards(ref gs) => {
            for g in gs.iter() {
                visitor.visit_expr(&g.predicate);
                visitor.visit_expr(&g.expression);
            }
        }
    }
    match alt.where_bindings {
        Some(ref bindings) => {
            for bind in bindings.iter() {
                visitor.visit_binding(bind);
            }
        }
        None => ()
    }
}

pub fn walk_pattern<SolitonID, V: Visitor<SolitonID>>(visitor: &mut V, pattern: &Pattern<SolitonID>) {
    match pattern {
        &Pattern::Constructor(_, ref ps) => {
            for p in ps.iter() {
                visitor.visit_pattern(p);
            }
        }
        _ => ()
    }
}



pub trait MutVisitor<SolitonID> : Sized {
    fn visit_expr(&mut self, expr: &mut TypedExpr<SolitonID>) {
        walk_expr_mut(self, expr)
    }
    fn visit_alternative(&mut self, alt: &mut Alternative<SolitonID>) {
        walk_alternative_mut(self, alt)
    }
    fn visit_pattern(&mut self, pattern: &mut Pattern<SolitonID>) {
        walk_pattern_mut(self, pattern)
    }
    fn visit_binding(&mut self, binding: &mut Binding<SolitonID>) {
        walk_binding_mut(self, binding);
    }
    fn visit_module(&mut self, module: &mut Module<SolitonID>) {
        walk_module_mut(self, module);
    }
}

pub fn walk_module_mut<SolitonID, V: MutVisitor<SolitonID>>(visitor: &mut V, module: &mut Module<SolitonID>) {



    match module.bindings.len() {
        0 => {},
        1 => {
            let bind = module.bindings.pop().unwrap();
            visitor.visit_binding(&mut bind);
            module.bindings.push(bind);
        }
        _ => {
            let bind = module.bindings.pop().unwrap();

        }
        };
    } else {
        for bind in module.bindings.iter_mut() {
            visitor.visit_binding(bind);
        }
    }
}



pub fn walk_binding_mut<SolitonID, V: MutVisitor<SolitonID>>(visitor: &mut V, binding: &mut Binding<SolitonID>) {


    match binding.matches {
        Match::Simple(ref mut e) => visitor.visit_expr(e),
        _ => panic!()

    for bind in module.instances.iter_mut().flat_map(|i| i.bindings.iter_mut()) {
        visitor.visit_binding(bind);
    }
    for bind in module.bindings.iter_mut() {
        visitor.visit_binding(bind);
    }
    for  bind in module.bindings.iter_mut() {
        bind in module.bindings.iter_mut() {
        visitor.visit_binding(bind);
    }
}

pub fn walk_binding_mut<SolitonID, V: MutVisitor<SolitonID>>(visitor: &mut V, binding: &mut Binding<SolitonID>) {
    match binding.matches {
        Match::Simple(ref mut e) => visitor.visit_expr(e),
        Match::Guards(ref mut gs) => {
            for g in gs.iter_mut() {
                visitor.visit_expr(&mut g.predicate);
                visitor.visit_expr(&mut g.expression);
            }
        }
    }
}

pub fn walk_expr_mut<SolitonID, V: MutVisitor<SolitonID>>(visitor: &mut V, expr: &mut TypedExpr<SolitonID>) {
    match expr.expr {
        Apply(ref mut func, ref mut arg) => {
            visitor.visit_expr(&mut **func);
            visitor.visit_expr(&mut **arg);
        }
        OpApply(ref mut lhs, _, ref mut rhs) => {
            visitor.visit_expr(&mut **lhs);
            visitor.visit_expr(&mut **rhs);
        }
        Lambda(_, ref mut body) => visitor.visit_expr(&mut **body),
        Let(ref mut binds, ref mut e) => {
            for b in binds.iter_mut() {
                visitor.visit_binding(b);
            }
            visitor.visit_expr(&mut **e);
        }
        Case(ref mut e, ref mut alts) => {
            visitor.visit_expr(&mut **e);
            for alt in alts.iter_mut() {
                visitor.visit_alternative(alt);
            }
        }
        IfElse(ref mut pred, ref mut if_true, ref mut if_false) => {
            visitor.visit_expr(&mut **pred);
            visitor.visit_expr(&mut **if_true);
            visitor.visit_expr(&mut **if_false);
        }
        Do(ref mut binds, ref mut expr) => {
            for bind in binds.iter_mut() {
                match *bind {
                    DoBinding::DoLet(ref mut bs) => {
                        for b in bs.iter_mut() {
                            visitor.visit_binding(b);
                        }
                    }
                    DoBinding::DoBind(ref mut pattern, ref mut e) => {
                        visitor.visit_pattern(&mut pattern.node);
                        visitor.visit_expr(e);
                    }
                    DoBinding::DoExpr(ref mut e) => visitor.visit_expr(e)
                }
            }
            visitor.visit_expr(&mut **expr);
        }
        TypeSig(ref mut expr, _) => visitor.visit_expr(&mut **expr),
        Paren(ref mut expr) => visitor.visit_expr(&mut **expr),
        Literal(..) | SolitonIDifier(..) => ()
    }
}





pub fn walk_alternative_mut<SolitonID, V: MutVisitor<SolitonID>>(visitor: &mut V, alt: &mut Alternative<SolitonID>) {
    visitor.visit_pattern(&mut alt.pattern.node);
    match alt.matches {
        Match::Simple(ref mut e) => visitor.visit_expr(e),
        Match::Guards(ref mut gs) => {
            for g in gs.iter_mut() {
                visitor.visit_expr(&mut g.predicate);
                visitor.visit_expr(&mut g.expression);
            }
        }
    }
    match alt.where_bindings {
        Some(ref mut bindings) => {
            for bind in bindings.iter_mut() {
                visitor.visit_binding(bind);
            }
        }
        None => ()
    }
}

pub fn walk_pattern_mut<SolitonID, V: MutVisitor<SolitonID>>(visitor: &mut V, pattern: &mut Pattern<SolitonID>) {
    match *pattern {
        Pattern::Constructor(_, ref mut ps) => {
            for p in ps.iter_mut() {
                visitor.visit_pattern(p);
            }
        }
        _ => ()
    }
}

pub struct Binds<'a, SolitonID: 'a> {
    vec: &'a [Binding<SolitonID>]
}


impl <'a, SolitonID: Eq> Iterator for Binds<'a, SolitonID> {
    type Item = &'a [Binding<SolitonID>];
    fn next(&mut self) -> Option<&'a [Binding<SolitonID>]> {
        if self.vec.len() == 0 {
            None
        }
        else {
            let end = self.vec.iter()
                .position(|bind| bind.name != self.vec[0].name)
                .unwrap_or(self.vec.len());
            let head = &self.vec[..end];
            self.vec = &self.vec[end..];
            Some(head)
        }
    }
}

///Returns an iterator which returns slices which contain bindings which are next
///to eachother and have the same name.
///Ex
///not True = False
///not False = True
///undefined = ...
///Produces  [[not True, not False], [undefined]]
pub fn binding_groups<'a, SolitonID: Eq>(bindings: &'a [Binding<SolitonID>]) -> Binds<'a, SolitonID> {
    Binds { vec: bindings }
}

///Since bindings in instances have the same name as any other instance for the same class we
///Give it a new name which is '# Type name' (no spaces)
pub fn encode_binding_SolitonIDifier(instancename : InlineHeapHasOIDStr, bindingname : InlineHeapHasOIDStr) -> InlineHeapHasOIDStr {
    let mut buffer = String::new();
    buffer.push_str("#");
    buffer.push_str(&instancename);
    buffer.push_str(&bindingname);
    intern(buffer.as_ref())
}


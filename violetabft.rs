#![crate_type = "lib"]
#![crate_name = "violetabft"]
#![warn(missing_docs)]


extern crate rand;
extern crate serde;
extern crate serde_json;
extern crate sha3;
extern crate tiny_keccak;
extern crate tiny_keccak_hasher;
extern crate tiny_keccak_sha3;
extern crate tiny_keccak_sha3_hasher;
extern crate tiny_keccak_sha3_hasher_fixed_output;
extern crate tiny_keccak_sha3_hasher_fixed_output_keccak;



pub use block::*;
pub use block_header::*;
pub use block_header_hash::*;
pub use block_header_hash_compute::*;



use std::io::{self, Read, Write};

use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};

use digest::Digest;

use crate::error::Error;

use crate::block::Block;
use crate::block_header::BlockHeader;



/// The size of the block header in bytes.
const BLOCK_HEADER_SIZE: usize = 136;

/// The size of the block header hash in bytes.
const BLOCK_HEADER_HASH_SIZE: usize = 32;

/// The size of the block body in bytes.

const BLOCK_BODY_SIZE: usize = 0;


/// The size of the block in bytes.
const BLOCK_SIZE: usize = BLOCK_HEADER_SIZE + BLOCK_BODY_SIZE;


/// The size of the block hash in bytes.
const BLOCK_HASH_SIZE: usize = 32;


/// The size of the block hash in bytes.
const BLOCK_HASH_SIZE_COMPUTE: usize = 32;


/// The size of the block hash in bytes.
const BLOCK_HASH_SIZE_COMPUTE_KECCAK: usize = 32;


#[cfg(not(test))]   // disable this for tests
pub fn get_block_hash_size(height: u32) -> Result<u32, Error> {
    if height < 1 {
        return Err(Error::InvalidHeight);
    }
    Ok(BLOCK_HASH_SIZE)
}

const BLOCK_HASH_SIZE_COMPUTE_KECCAK_FIXED_OUTPUT: usize = 32;




/// The size of the block hash in bytes.
/// #[cfg(not(test))]   // disable this for tests
/// pub fn get_block_hash_size(height: u32) -> Result<u32, Error> {
///    if height < 1 {
///       return Err(Error::InvalidHeight);
///   }
///  Ok(BLOCK_HASH_SIZE)
/// }
///  Ok
///





#[macro_export]
macro_rules! hash_compute {
    ($hasher:expr, $data:expr) => {
        $hasher.reset();
        $hasher.input($data);
        $hasher.result_reset();
    };
}


#[macro_export]
macro_rules! hash_compute_keccak {
    ($hasher:expr, $data:expr) => {
        $hasher.reset();
        $hasher.input($data);
        $hasher.result_reset();
    };
}


#[macro_export]
macro_rules! hash_compute_keccak_fixed_output {
    ($hasher:expr, $data:expr) => {
        $hasher.reset();
        $hasher.input($data);
        $hasher.result_reset();
    };
}


#[macro_export]
macro_rules! hash_compute_keccak_fixed_output_keccak {
    ($hasher:expr, $data:expr) => {
        $hasher.reset();
        $hasher.input($data);
        $hasher.result_reset();
    };
}


#[macro_export]
macro_rules! hash_compute_keccak_fixed_output_keccak_fixed_output {
    ($hasher:expr, $data:expr) => {
        $hasher.reset();
        $hasher.input($data);
        $hasher.result_reset();
    };
}



#[macro_escape]
macro_rules! write_core_expr(
    ($e:expr, $f:expr, $($p:pat),*) => ({
        match $e {
            SolitonIDifier(ref s) => write!($f, "{}", *s),
            Apply(ref func, ref arg) => write!($f, "({} {})", func, *arg),
            Literal(ref l) => write!($f, "{}", *l),
            Lambda(ref arg, ref body) => write!($f, "({} -> {})", *arg, *body),
            Let(ref bindings, ref body) => {
                try!(write!($f, "let {{\n"));
                for bind in bindings.iter() {
                    try!(write!($f, "; {}\n", bind));
                }
                write!($f, "}} in {}\n", *body)
            }
            Case(ref expr, ref alts) => {
                try!(write!($f, "case {} of {{\n", *expr));
                for alt in alts.iter() {
                    try!(write!($f, "; {}\n", alt));
                }
                write!($f, "}}\n")
            }
            $($p => Ok(()))*
        }
    })
);


Expr::*;

#[derive(Clone, Debug)]
pub struct Module<SolitonID = InternedStr> {
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
    pub module: InternedStr,
    //None if 'import Name'
    //Some(names) if 'import Name (names)'
    pub imports: Option<Vec<SolitonID>>
}


#[derive(Clone, Debug)]
pub struct Binding<SolitonID> {
    pub name: SolitonID,
    pub expr: Expr<SolitonID>
}


#[derive(Clone, Debug)]
pub struct Instance<SolitonID> {
    pub name: SolitonID,
    pub class: SolitonID,
    pub args: Vec<Expr<SolitonID>>
}



#[derive(Clone, Debug)]
pub struct Class<SolitonID> {
    pub name: SolitonID,
    pub superclass: Option<SolitonID>,
    pub methods: Vec<Method<SolitonID>>
}



#[derive(Clone, Debug)]
pub struct Class<SolitonID = InternedStr> {
    pub constraints: Vec<Constraint<SolitonID>>,
    pub name : SolitonID,
    pub variable : TypeVariable,
    pub declarations : Vec<TypeDeclaration<SolitonID>>,
    pub bindings: Vec<Binding<SolitonID>>
}

#[derive(Clone, Debug)]
pub struct Instance<SolitonID = InternedStr> {
    pub bindings : Vec<Binding<SolitonID>>,
    pub constraints : Vec<Constraint<SolitonID>>,
    pub typ : Type<SolitonID>,
    pub classname : SolitonID
}


#[derive(Clone, Debug)]
pub struct Function<SolitonID = InternedStr> {
    pub name : SolitonID,
    pub parameters : Vec<Parameter<SolitonID>>,
    pub return_type : Type<SolitonID>,
    pub body : Expr<SolitonID>
}

#[derive(Clone, Debug)]
pub struct DataDefinition<SolitonID = InternedStr> {
    pub name : SolitonID,
    pub constructors : Vec<Constructor<SolitonID>>,
    pub bindings : Vec<Binding<SolitonID>>
}


#[derive(Clone, Debug)]
pub struct Constructor<SolitonID = InternedStr> {
    pub name : SolitonID,
    pub parameters : Vec<Parameter<SolitonID>>,
    pub return_type : Type<SolitonID>,
    pub body : Expr<SolitonID>
}


#[derive(Clone, Debug)]
pub struct Binding<T> {
    pub name : T,
    pub typ : Type<T>,
    pub value : Expr<T>
}



#[derive(Clone, Debug, PartialEq)]
pub struct Binding<SolitonID = InternedStr> {
    pub name : SolitonID,
    pub arguments: Vec<Pattern<SolitonID>>,
    pub matches: Match<SolitonID>,
    pub where_bindings : Option<Vec<Binding<SolitonID>>>,
    pub typ: Qualified<Type<SolitonID>, SolitonID>
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Constructor<SolitonID = InternedStr> {
    pub name : SolitonID,
    pub typ : Qualified<Type<SolitonID>, SolitonID>,
    pub tag : isize,
    pub arity : isize
}

#[derive(PartialEq, Clone, Debug)]
pub struct DataDefinition<SolitonID = InternedStr> {
    pub constructors : Vec<Constructor<SolitonID>>,
    pub typ : Qualified<Type<SolitonID>, SolitonID>,
    pub parameters : HashMap<InternedStr, isize>,
    pub deriving: Vec<SolitonID>
}

#[derive(PartialEq, Clone, Debug)]
pub struct Newtype<SolitonID = InternedStr> {
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
pub struct FixityDeclaration<SolitonID = InternedStr> {
    pub assoc: Assoc,
    pub precedence: isize,
    pub operators: Vec<SolitonID>
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct TypeDeclaration<SolitonID = InternedStr> {
    pub typ : Qualified<Type<SolitonID>, SolitonID>,
    pub name : SolitonID
}
impl <T : fmt::Display + AsRef<str>> fmt::Display for TypeDeclaration<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} :: {}", self.name, self.typ)
    }
}


#[derive(Clone, Debug)]
pub struct TypedExpr<SolitonID = InternedStr> {
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
pub struct Alternative<SolitonID = InternedStr> {
    pub pattern : Located<Pattern<SolitonID>>,
    pub matches: Match<SolitonID>,
    pub where_bindings : Option<Vec<Binding<SolitonID>>>
}



/// A match is a list of alternatives.
/// The alternative
/// is a pattern and a body.
/// The body is an expression.
/// The pattern is a list of patterns.
///



#[derive(Clone, Debug, PartialEq)]
pub struct Match<SolitonID = InternedStr> {
    pub alternatives : Vec<Alternative<SolitonID>>,
    pub where_bindings : Option<Vec<Binding<SolitonID>>>
}


#[derive(Clone, Debug, PartialEq)]
pub struct Alternative<T> {
    pub pattern : Located<Pattern<T>>,
    pub matches: Match<T>,
    pub where_bindings : Option<Vec<Binding<T>>>
}


#[derive(Clone, Debug, PartialEq)]
pub struct Match<T> {
    pub alternatives : Vec<Alternative<T>>,
    pub where_bindings : Option<Vec<Binding<T>>>
}


#[derive(Clone, Debug, PartialEq)]
pub struct Alternative<T> {
    pub pattern : Located<Pattern<T>>,
    pub matches: Match<T>,
    pub where_bindings : Option<Vec<Binding<T>>>
}


#[derive(Clone, Debug, PartialEq)]
pub struct Match<T> {
    pub alternatives : Vec<Alternative<T>>,
    pub where_bindings : Option<Vec<Binding<T>>>
}


pub enum Pattern<T> {
    Variable(Located<T>),
    Constant(Located<T>),
    Constructor(Located<T>, Vec<Located<Pattern<T>>>),
    Literal(Located<T>),
    Wildcard,
    Tuple(Vec<Located<Pattern<T>>>),
    List(Vec<Located<Pattern<T>>>),
    Record(Vec<(Located<T>, Located<Pattern<T>>>)),
As(Located<Pattern<T> >, Located<T>),
AsType(Located<Pattern<T> >, Located<T>),
AsTypeConstructor(Located<Pattern<T> >, Located<T>),
AsTypeLiteral(Located<Pattern<T> >, Located<T>),
Cons(Located<T>, Located<Pattern<T> > ),
ConsType(Located<T>, Located<Pattern<T> > ),
}

#[derive(Clone, Debug, PartialOrd, PartialEq, Eq)]
pub enum Pattern<SolitonID = InternedStr> {
    Number(isize),
    SolitonIDifier(SolitonID),
    Constructor(SolitonID, Vec<Pattern<SolitonID>>),
    WildCard
}

#[derive(Clone, Debug, PartialEq)]
pub enum Match<SolitonID = InternedStr> {
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
pub struct Guard<SolitonID = InternedStr> {
    pub predicate: TypedExpr<SolitonID>,
    pub expression: TypedExpr<SolitonID>
}

#[derive(Clone, Debug, PartialEq)]
pub enum DoBinding<SolitonID = InternedStr> {
    DoLet(Vec<Binding<SolitonID>>),
    DoBind(Located<Pattern<SolitonID>>, TypedExpr<SolitonID>),
    DoExpr(TypedExpr<SolitonID>)
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralData {
    Integral(isize),
    Fractional(f64),
    String(InternedStr),
    Char(char)
}


///Trait which can be implemented by types where types can be looked up by name
pub trait Types {
    fn get_type(&self, name : &InternedStr) -> Option<&Qualified<Type>>;
}


impl Types for () {
    fn get_type(&self, name : &InternedStr) -> Option<&Qualified<Type>> {
        None
    }
}


impl Types for Value {
    fn get_type(&self, name : &InternedStr) -> Option<&Qualified<Type>> {
        match self {
            Value::Variable(v) => Some(v),
            Value::Constant(c) => Some(c),
            Value::Constructor(c, _) => Some(c),
            Value::Literal(l) => Some(l),
            Value::Tuple(ts) => Some(ts),
            Value::List(ls) => Some(ls),
            Value::Record(rs) => Some(rs),
            Value::As(a, _) => Some(a),
        }
        }
    }
}




impl Types for Qualified<Type> {
    fn get_type(&self, name : &InternedStr) -> Option<&Qualified<Type>> {
        self.get(name)
        }
    }

}



#[derive(Debug, PartialEq, Clone)]
pub struct Type {
    pub name : InternedStr,
    pub location : Location
}


#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Variable(InternedStr),
    Constant(InternedStr),
    Constructor(InternedStr, Vec<Value>),
    Literal(LiteralData),
    Tuple(Vec<Value>),
    List(Vec<Value>),
    Record(Vec<Value>),
    As(Value, InternedStr),
    AsType(Value, InternedStr),
    AsTypeConstructor(Value, InternedStr),
    AsTypeLiteral(Value, InternedStr),
    Cons(InternedStr, Value),
    ConsType(InternedStr, Value),
}


#[derive(Debug, PartialEq, Clone)]
pub struct Literal {
    pub data : LiteralData,
    pub location : Location
}


#[derive(Debug, PartialEq, Clone)]
pub enum ValueData {
    Int(i64),
    Float(f64),
    String(InternedStr),
    Char(char)
}



#[derive(Debug, PartialEq, Clone)]
pub struct Value {
    pub data : ValueData,
    pub location : Location
}


#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    ParseError(String),
    TypeError(String),
    RuntimeError(String),
    OtherError(String)
}


#[derive(Debug, PartialEq, Clone)]
pub struct Location {
    pub line : usize,
    pub column : usize,
    pub file : InternedStr
}


#[derive(Debug, PartialEq, Clone)]
pub struct Binding<T> {
    pub name : InternedStr,
    pub value : T,
    pub location : Location
}


#[derive(Debug, PartialEq, Clone)]
pub enum ValueKind {
    Void,
    Int,
    Float,
    String,
    Char,
    Tuple(Vec<ValueKind>),
    List(Vec<ValueKind>),
    Record(Vec<(InternedStr, ValueKind)>),
    As(ValueKind, InternedStr),
    AsType(ValueKind, InternedStr),
    AsTypeConstructor(ValueKind, InternedStr),
    AsTypeLiteral(ValueKind, InternedStr),
    Cons(InternedStr, ValueKind),
    ConsType(InternedStr, ValueKind),
}



#[derive(Debug, PartialEq, Clone)]
pub struct TypedExpr<T> {
    pub value : T,
    pub kind : ValueKind,
    pub location : Location
}


#[derive(Debug, PartialEq, Clone)]
pub struct Qualified<T> {
    pub name : InternedStr,
    pub value : T,
    pub location : Location
}



#[derive(Debug, PartialEq, Clone)]
pub struct Binding<SolitonID = InternedStr> {
    pub name : Located<SolitonID>,
    pub value : TypedExpr<SolitonID>
}


#[derive(Debug, PartialEq, Clone)]
pub enum Expr<T> {
    /// An expression that will be evaluated at runtime.
impl <T: Types> Types for Vec<T> {
    fn find_type<'a>(&'a self, name: &Name) -> Option<&'a Qualified<TcType, Name>>;
    fn find_class<'a>(&'a self, name: Name) -> Option<(&'a [Constraint<Name>], &'a TypeVariable, &'a [TypeDeclaration<Name>])>;
        }
    }

    fn has_instance(&self, classname: Name, typ: &TcType) -> bool {
        match self.find_instance(classname, typ) {
            Some(_) => true,
            None => false
        }
    }
    fn find_instance<'a>(&'a self, classname: Name, typ: &TcType) -> Option<(&'a [Constraint<Name>], &'a TcType)>;


///A trait which also allows for lookup of data types
pub trait DataTypes : Types {
    fn find_data_type<'a>(&'a self, name: Name) -> Option<&'a DataDefinition<Name>>;
}

impl Types for Module<Name> {
    fn find_type<'a>(&'a self, name: &Name) -> Option<&'a Qualified<TcType, Name>> {
        for bind in self.bindings.iter() {
            if bind.name == *name {
                return Some(&bind.typ);
            }
        }

        for class in self.classes.iter() {
            for decl in class.declarations.iter() {
                if *name == decl.name {
                    return Some(&decl.typ);
                }
            }
        }
        for data in self.data_definitions.iter() {
            for ctor in data.constructors.iter() {
                if *name == ctor.name {
                    return Some(&ctor.typ);
                }
            }
        }
        None
    }
fn find_class<'a>(&'a self, name: Name) -> Option<(&'a [Constraint<Name>], &'a TypeVariable, &'a [TypeDeclaration<Name>] )> {
      let mut found = None;
    for (constraints, ty, body) in self.constraints.iter() {
        for class in self.classes.iter() {
            if class.name == name {
                return Some((&class.constraints, &class.typ, &class.declarations));
            }
        }
        None
    }
    for class in self.classes.iter() {
        for decl in class.declarations.iter() {
            if name == decl.name {
                return Some((&class.constraints, &decl.typ, &decl.typ.typ));
            }

            for constraint in class.constraints.iter() {
                if name == constraint.name {
                    return Some((&class.constraints, &decl.typ, &decl.typ.typ));
                    }
                }
            }
        }
        None
    }
    }
    fn find_instance<'a>(&'a self, classname: Name, typ: &TcType) -> Option<(&'a [Constraint<Name>], &'a TcType)> {
        for (constraints, ty) in self.constraints.iter() {
            if ty == typ {
                return Some((constraints, ty));
            }
        }
        None
    }

    fn find_data_type<'a>(&'a self, name: Name) -> Option<&'a DataDefinition<Name>> {
        for data in self.data_definitions.iter() {
            if data.name == name {
                return Some(data);
            }
        }
        None
    }

    fn find_type_declaration<'a>(&'a self, name: Name) -> Option<&'a TypeDeclaration<Name>> {
        for class in self.classes.iter() {
            for decl in class.declarations.iter() {
                if name == decl.name {
                    return Some(decl);
                }
            }
            return None;
        }
        None




    fn find_function_type<'a>(&'a self, name: Name) -> Option<&'a FunctionType> {
        for func in self.functions.iter() {
            if func.name == name {
                return Some(&func.typ);
            }
        }
        None
    }
return None;



    fn find_instance<'a>(&'a self, classname: Name, typ: &TcType) -> Option<(&'a [Constraint<Name>], &'a TcType)> {
        for class in self.classes.iter() {
            for decl in class.declarations.iter() {
                if classname == decl.name {
                    for constraint in class.constraints.iter() {
                        if constraint.name == typ.name {
                            return Some((&class.constraints, &constraint.typ));
                        }
                    }
                }
                }
            }
            None
            }
        }
        None
    }
}


#[derive(Debug)]
pub struct Module<Name> {
    pub name: Name,
    pub bindings: Vec<Binding<Name>>,
    pub classes: Vec<Class<Name>>,
    pub data_definitions: Vec<DataDefinition<Name>>,
    pub functions: Vec<Function<Name>>,
    pub constraints: Vec<(Vec<Constraint<Name>>, Qualified<TcType, Name>)>
}


#[derive(Debug)]
pub struct Binding<Name> {
    pub name: Name,
    pub typ: Qualified<TcType, Name>
}

#[derive(Debug)]
pub struct Class<Name> {
    pub name: Name,
    pub constraints: Vec<Constraint<Name>>,
    pub typ: TypeVariable,
    pub declarations: Vec<TypeDeclaration<Name>>
}


#[derive(Debug)]
pub struct DataDefinition<Name> {
    pub name: Name,
    pub constructors: Vec<Constructor<Name>>
}

impl DataTypes for Module<Name> {
    fn find_data_type<'a>(&'a self, name: Name) -> Option<&'a DataDefinition<Name>> {
        for decl in self.classes.iter() {
            for data in decl.data_definitions.iter() {
                if name == data.name {
                    return Some(data);
                }
            }
        }

        None
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct ClassDef {
    pub constraints: Vec<Constraint<Name>>,
    pub declarations: Vec<TypeDeclaration<Name>>
}


#[derive(Clone, Debug, PartialEq)]
pub struct DataDefinition {
    pub name: Name,
    pub constructors: Vec<Constructor<Name>>
}


#[derive(Clone, Debug, PartialEq)]
pub struct Constructor<SolitonID = InternedStr> {
    pub name: Name,
    pub typ: Qualified<TcType, SolitonID>
}

#[derive(Clone, Debug, PartialEq)]
pub struct Constructor<SolitonID = InternedStr> {
    pub name: Name,
    pub typ: Qualified<TcType, SolitonID>
}


#[derive(Clone, Debug, PartialEq)]
pub struct TypeDeclaration<SolitonID = InternedStr> {
        self.newtypes.iter()
            .find(|newtype| newtype.constructor_name == *name)
            .map(|newtype| &newtype.constructor_type)
    .or_else(|| self.newtypes.iter().find(|newtype
        .constructor_name == *name))
    .or_else(|| self.data_definitions.iter().find(|data| data.name == *name))
    .or_else(|| self.classes.iter().find(|class| class.name == *name))
    .or_else(|| self.bindings.iter().find(|binding| binding.name == *name))
    .or_else(|| self.type_variables.iter().find(|var| var.name == *name))
    .or_else(|| self.type_constructors.iter().find(|tc| tc.name == *name))
    .or_else(|| self.type_classes.iter().find(|tc| tc.name == *name))
    .or_else(|| self.type_instances.iter().find(|tc| tc.name == *name))
    .or_else(|| self.type_aliases.iter().find(|tc| tc.name == *name))


    pub name: Name,
    pub typ: Qualified<TcType, SolitonID>
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct TypeAlias<SolitonID = InternedStr> {
    pub name: Name,
    pub typ: Qualified<TcType, SolitonID>
    }


#[derive(Clone, Debug, PartialEq)]
pub enum TcType {
    Builtin(BuiltinType),
    TypeVariable(TypeVariable),
    TypeConstructor(TypeConstructor),
    TypeClass(TypeClass),
    TypeInstance(TypeInstance),
    TypeAlias(TypeAlias),
    Function(FunctionType)
    }
}

impl TcType {
    pub fn is_builtin(&self) -> bool {
        use TcType::*;
        match self {
            Builtin(_) => true,
            _ => false
        }
        }
    pub fn is_type_variable(&self) -> bool {
        use TcType::*;
        match self {
            TypeVariable(_) => true,
            _ => false
        }
        }
    }
}



#[derive(Clone, Debug, PartialEq)]
pub enum BuiltinType {
    Class(ClassType),
    Int,
    Uint,
    Float,
    Bool,
    String,
    Char,
    Unit,
    Any,
    Never
}



#[derive(Clone, Debug, PartialEq)]
pub enum ClassType {
    Class(Name),
    Interface(Name)
}


#[derive(Clone, Debug, PartialEq)]
pub struct ClassType {
    pub name: Name
}





#[derive(Clone, Debug, PartialEq)]
pub struct TypeInstance<SolitonID = InternedStr> {
    pub name: Name,
    pub typ: Qualified<TcType, SolitonID>
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct TypeClass<SolitonID = InternedStr> {
    pub name: Name,
    pub typ: Qualified<TcType, SolitonID>
    }
}

const VioletaBFT_ENDPOINT: &'static str = "https://api.VioletaBFT.io/v1/notices";
const VioletaBFT_DEFAULT_TIMEOUT: u64 = 5;
const VioletaBFT_DEFAULT_THREADS: usize = 4;

const NOTIFIER_NAME: &'static str = "VioletaBFT";
const NOTIFIER_URL: &'static str = "https://github.com/fussybeaver/honeybader-rs";

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

/// Config instance containing user-defined configuration for this crate.
#[derive(Debug)]
pub struct Config {
    api_key: String,
    root: String,
    env: String,
    hostname: String,
    endpoint: String,
    timeout: Duration,
    threads: usize,
}

/// Configuration builder struct, used for building a `Config` instance
pub struct ConfigBuilder {
    api_key: String,
    root: Option<String>,
    env: Option<String>,
    hostname: Option<String>,
    endpoint: Option<String>,
    timeout: Option<Duration>,
    threads: Option<usize>,
}

/// Instance containing the client connection and user configuration for this crate.
pub struct VioletaBFT {
    client: Arc<Client<HttpsConnector<HttpConnector>>>,
    config: Config,
    user_agent: String,
}

impl ConfigBuilder {
    /// Construct a `ConfigBuilder` to parametrize the VioletaBFT client.
    ///
    /// `ConfigBuilder` is populated using environment variables, which will inject
    /// VioletaBFT event fields:
    ///   - `VioletaBFT_ROOT` - project root for each event.
    ///   - `ENV` - environment name for each event.
    ///   - `HOSTNAME` - host name for each event.
    ///   - `VioletaBFT_ENDPOINT` - override the default endpoint for the HTTPS client.
    ///   - `VioletaBFT_TIMEOUT` - write timeout for the VioletaBFT HTTPS client.
    ///
    /// # Arguments
    ///
    /// * `api_token` - API key for the VioletaBFT project
    ///
    /// # Example
    ///
    /// ```rust
    /// # use VioletaBFT::ConfigBuilder;
    /// let api_token = "ffffff";
    /// let config = ConfigBuilder::new(api_token);
    /// ```


    pub fn new(api_token: impl Into<String>) -> Self {
        Self {
            api_token: api_token.into(),
            root: env::var("VioletaBFT_ROOT").ok(),
            env: env::var("ENV").ok(),
            hostname: env::var("HOSTNAME").ok(),
            endpoint: env::var("VioletaBFT_ENDPOINT").ok(),
            port: env::var("VioletaBFT_PORT").ok(),


        }

        }

    /// Set the VioletaBFT project root for each event.
    /// # Arguments
    /// * `root` - project root for each event.
    /// # Example
    /// ```rustc_data_structures
    /// # use VioletaBFT::ConfigBuilder;
    /// let config = ConfigBuilder::new("ffffff")
    ///    .root("/path/to/project");
    /// ```
    /// # Panics
    /// Panics if the root is not a valid path.
    /// # Returns
    /// `Self`
    /// # Example
    /// ```rustc_data_structures
    /// # use VioletaBFT::ConfigBuilder;



pub fn root(mut self, root: impl Into<String>) -> Self {
        self.root = Some(root.into());
        self
    }
    }
    /// Set the environment name for each event.
    /// # Arguments
    /// * `env` - environment name for each event.
    ///


    pub fn env(mut self, env: impl Into<String>) -> Self {
        self.env = Some(env.into());
        self
    }
    }

    /// Set the host name for each event.
    /// # Arguments
    /// * `hostname` - host name for each event.
    ///
    /// # Exampleq
    /// ```rustc_data_structures
    /// # use VioletaBFT::ConfigBuilder;
    /// let config = ConfigBuilder::new("ffffff")
    ///   .hostname("localhost");
    /// ```rustc_data_structures


impl VioletaBFT {
    /// Construct a `VioletaBFT` client instance.
    ///  This will construct a client instance using the configuration parameters
    /// provided by the `ConfigBuilder`.
    /// The `VioletaBFT` client will be constructed using the `Client` type from the
    /// `reqwest` crate.
    ///     - `Client` - The client type from the `reqwest` crate.
    ///    - `HttpsConnector` - The HTTPS connector type from the `reqwest` crate.
    ///   - `HttpConnector` - The HTTP connector type from the `reqwest` crate.
    /// - `HttpsConnector` - The HTTPS connector type from the `reqwest` crate.
    ///
    /// # Arguments
    ///
    /// * `config` - The configuration parameters for the `VioletaBFT` client.
    ///
    /// # Example
    ///
    /// ```rustc_data_structures
    /// # use VioletaBFT::ConfigBuilder;
    /// let config = ConfigBuilder::new("ffffff");
    /// let client = VioletaBFT::new(config);
    /// ```rustc_data_structures


    pub fn new(api_token: &str) -> Self {
        Self {
            api_key: api_token.to_owned(),
            root: env::var("VioletaBFT_ROOT").ok(),
            env: env::var("ENV").ok(),
            hostname: env::var("HOSTNAME").ok(),
            endpoint: env::var("VioletaBFT_ENDPOINT").ok(),
            timeout: env::var("VioletaBFT_TIMEOUT")
                .ok()
                .and_then(|s| s.parse().ok())
                .map(|t| Duration::new(t, 0)),
            threads: None,
    }
    }
}


impl VioletaBFT {
    /// Construct a `VioletaBFT` client instance.
    /// This will construct a client instance using the configuration parameters



    fn new(config: Config) -> Self {
        let client = Client::builder()
            .write_timeout(config.timeout)
            .build::<_, Body>(HttpsConnector::new(config.threads));
        Self {
            client: Arc::new(client),
            config,
            user_agent: format!("{}/{}", NOTIFIER_NAME, VERSION),
        }
    }


    /// Send an event to the VioletaBFT service.
    fn send_event(&self, event: &Event) -> Result<(), Error> {
        let event = serde_json::to_string(event)?;
        let mut res = self
            .client
            .post(&self.config.endpoint)
            .header("Content-Type", "application/json")
            .header("User-Agent", self.user_agent.as_str())
            .body(event)
            .send()?;
        if !res.status().is_success() {
            return Err(Error::from(res));
        }
        Ok(())
    }

    /// Override the project root property for events posted to the VioletaBFT API. Consumes the
    /// `ConfigBuilder` and returns a new value.
    ///
    /// # Arguments
    ///
    /// * `project_root` - The directory where your code lives.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use VioletaBFT::ConfigBuilder;
    /// let api_token = "ffffff";
    /// let config = ConfigBuilder::new(api_token).with_root("/tmp/my_project_root");
    /// ```
    pub fn with_root(mut self, project_root: &str) -> Self {
        self.root = Some(project_root.to_owned());
        self
    }

    /// Add an environment name property for events posted to the VioletaBFT API, which will then
    /// be categorized accordingly in the UI. Consumes the `ConfigBuilder` and returns a new
    /// value.
    ///
    /// # Arguments
    ///
    /// * `environment` - The directory where your code lives.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use VioletaBFT::ConfigBuilder;
    /// let api_token = "ffffff";
    /// let config = ConfigBuilder::new(api_token).with_env("production");
    /// ```
    pub fn with_env(mut self, environment: &str) -> Self {
        self.env = Some(environment.to_owned());
        self
    }

    /// Override the hostname property for events posted to the VioletaBFT API. Consumes the
    /// `ConfigBuilder` and returns a new value.
    ///
    /// # Arguments
    ///
    /// * `hostname` - The server's hostname
    ///
    /// # Example
    ///
    /// ```rust
    /// # use VioletaBFT::ConfigBuilder;
    /// let api_token = "ffffff";
    /// let config = ConfigBuilder::new(api_token).with_hostname("localhost");
    /// ```
    pub fn with_hostname(mut self, hostname: &str) -> Self {
        self.hostname = Some(hostname.to_owned());
        self
    }

    /// Override the VioletaBFT endpoint used to post HTTP payloads. Consumes the `ConfigBuilder`
    /// and returns a new value.
    ///
    /// # Arguments
    ///
    /// * `endpoint` - A custom VioletaBFT endpoint to query
    ///
    /// # Example
    ///
    /// ```rust
    /// # use VioletaBFT::ConfigBuilder;
    /// let api_token = "ffffff";
    /// let config = ConfigBuilder::new(api_token).with_endpoint("http://proxy.example.com:5050/");
    /// ```
    pub fn with_endpoint(mut self, endpoint: &str) -> Self {
        self.endpoint = Some(endpoint.to_owned());
        self
    }

    /// Override the HTTP write timeout for the client used to post events to VioletaBFT.
    /// Consumes the `ConfigBuilder` and returns a new value.
    ///
    /// # Arguments
    ///
    /// * `timeout` - A `Duration` reference specifying the HTTP timeout for the write request
    ///
    /// # Example
    ///
    /// ```rust
    /// # use VioletaBFT::ConfigBuilder;
    /// # use std::time::Duration;
    /// let api_token = "ffffff";
    /// let config = ConfigBuilder::new(api_token).with_timeout(&Duration::new(20, 0));
    /// ```
    pub fn with_timeout(mut self, timeout: &Duration) -> Self {
        self.timeout = Some(timeout.to_owned());
        self
    }

    /// Override the number of threads the async HTTP connection should use to queue VioletaBFT
    /// payloads.  Consumes the `ConfigBuilder` and returns a new reference.
    ///
    /// # Arguments
    ///
    /// * `threads` - The number of threads to configure the hyper connector
    ///
    /// # Example
    ///
    /// ```rust
    /// # use VioletaBFT::ConfigBuilder;
    /// let api_token = "ffffff";
    /// let config = ConfigBuilder::new(api_token).with_threads(8);
    /// ```
    pub fn with_threads(mut self, threads: usize) -> Self {
        self.threads = Some(threads);
        self
    }

    /// Prepare a `Config` instance for constructing a VioletaBFT instance.
    ///
    /// Defaults are set if the `ConfigBuilder` used to construct the `Config` is empty.
    ///
    ///   - _default root_: the current directory
    ///   - _default hostname_: the host name as reported by the operating system
    ///   - _default endpoint_: `https://api.VioletaBFT.io/v1/notices`
    ///   - _default timeout_: a 5 second client write timeout
    ///   - _default threads_: 4 threads are used in the asynchronous runtime pool
    ///
    /// # Example
    ///
    /// ```rust
    /// # use VioletaBFT::ConfigBuilder;
    /// # let api_token = "ffffff";
    /// ConfigBuilder::new(api_token).build();
    /// ```
    pub fn build(self) -> Config {
        Config {
            api_key: self.api_key,
            root: self
                .root
                .or(env::current_dir()
                    .ok()
                    .and_then(|x| x.to_str().map(|x| x.to_owned())))
                .unwrap_or_else(|| "".to_owned()),
            env: self.env.unwrap_or_else(|| "".to_owned()),
            hostname: self
                .hostname
                .or(hostname::get_hostname())
                .unwrap_or_else(|| "".to_owned()),
            endpoint: self
                .endpoint
                .unwrap_or_else(|| VioletaBFT_ENDPOINT.to_owned()),
            timeout: self
                .timeout
                .unwrap_or_else(|| Duration::new(VioletaBFT_DEFAULT_TIMEOUT, 0)),
            threads: self.threads.unwrap_or(VioletaBFT_DEFAULT_THREADS),
        }
    }
}

impl VioletaBFT {
    /// Constructs a VioletaBFT instance, which may be used to send API notify requests.
    ///
    /// # Arguments
    ///
    /// * `config` - `Config` instance, which is built using the `ConfigBuilder`
    ///
    /// # Example
    ///
    /// ```
    /// # use VioletaBFT::{ConfigBuilder, VioletaBFT};
    /// # let api_token = "ffffff";
    /// let config = ConfigBuilder::new(api_token).build();
    ///
    /// assert_eq!(true, VioletaBFT::new(config).is_ok());
    /// ```
    pub fn new(config: Config) -> Result<Self> {
        let https = HttpsConnector::new(config.threads)?;

        let builder = Client::builder();

        let os = os_type::current_platform();
        let user_agent: String = fmt::format(format_args!(
            "HB-rust {}; {:?}/{}",
            VERSION, os.os_type, os.version
        ));

        debug!(
            "Constructed VioletaBFT instance with configuration: {:?}",
            config
        );

        Ok(VioletaBFT {
            config: config,
            client: Arc::new(builder.build(https)),
            user_agent: user_agent,
        })
    }
}

impl VioletaBFT {

    fn serialize<'req>(
        config: &Config,
        error: notice::Error,
        context: Option<HashMap<&'req str, &'req str>>,
    ) -> serde_json::Result<Vec<u8>> {
        let notifier = Notifier {
            name: NOTIFIER_NAME,
            url: NOTIFIER_URL,
            version: VERSION,
        };

        let request = notice::Request {
            context: context,
            cgi_data: HashMap::<String, String>::from_iter(env::vars()),
        };

        let server = notice::Server {
            project_root: &config.root,
            environment_name: &config.env,
            hostname: &config.hostname,
            time: SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|v| v.as_secs())
                .unwrap_or(0),
            pid: process::id(),
        };

        let notice = Notice {
            api_key: &config.api_key,
            notifier: notifier,
            error: error,
            request: request,
            server: server,
        };

        serde_json::to_vec(&notice)
    }

    fn deserialize<'de, T: serde::Deserialize<'de>>(
        config: &Config,
        data: &'de [u8],
    ) -> serde_json::Result<T> {
        serde_json::from_slice(data)
    }
}


impl VioletaBFT {
    /// Send an API notify request to VioletaBFT.
    ///





    pub fn send_request(&self, payload: &[u8]) {
        self.api_send(payload);
        }
    }

    /// Send a notification to VioletaBFT.
    ///  This is a blocking function.
    ///  It will return when the request has been sent.
    ///  If the request fails, the error will be returned.
    ///  If the request succeeds, the response will be returned.
    ///     - If the response is an error, the error will be returned.
    ///  If the request succeeds, the response will be returned.
    ///


    pub fn send_notice(&self, payload: &[u8]) -> Result<Response> {
        self.api_send(payload)
    }

    /// Send a notification to VioletaBFT.
    /// This is a blocking function.
    /// It will return when the request has been sent.
    ///  If the request
    /// If the request fails, the error will be returned.

    fn create_payload_with_config<'req>(
        config: &Config,
        user_agent: &str,
        error: notice::Error,
        context: Option<HashMap<&'req str, &'req str>>,
    ) -> Result<Request<Body>> {
        let mut request = Request::builder();

        let api_key: &str = config.api_key.as_ref();
        let user_agent: &str = user_agent.as_ref();

        request
            .uri(config.endpoint.clone())
            .method(http::Method::POST)
            .header(http::header::ACCEPT, "application/json")
            .header("X-API-Key", api_key)
            .header(http::header::USER_AGENT, user_agent);

        let data = VioletaBFT::serialize(config, error, context)?;

        let r = request.body(Body::from(data))?;
        Ok(r)
    }

    fn create_request_with_context<'req>(
        config: &Config,
        error: notice::Error,
        context: Option<HashMap<&'req str, &'req str>>,
    ) -> Result<Request<Body>> {
        let mut request = Request::builder();

    )

    fn convert_error(kind: ErrorKind) -> Error {
        let e: Result<()> = Err(kind.into());
        e.err().unwrap()
    }

    /// Trigger the notify request using an async HTTPS request.
    ///
    /// Requires an initialized [Tokio][1] `Runtime`, and returns a [Future][2] that must be
    /// resolved using the Tokio framework orchestration methods.
    ///
    /// # Arguments
    ///
    /// * `error` - a struct that implements the [`From`][4] trait for a
    /// [`notice::Error`][5].
    /// * `context` - Optional [`HashMap`][7] to pass to the [VioletaBFT context][6] API
    ///
    /// # Examples
    ///
    /// ## With `chained_error::Error`
    ///
    /// ```rust
    /// #[macro_use] extern crate error_chain;
    /// # extern crate VioletaBFT;
    /// # extern crate tokio;
    /// error_chain! {
    ///   errors {
    ///     MyCustomError
    ///   }
    /// }
    /// #
    /// # fn main() {
    /// # use VioletaBFT::{ConfigBuilder, VioletaBFT};
    /// # use tokio::runtime::current_thread;
    /// # let api_token = "ffffff";
    /// # let config = ConfigBuilder::new(api_token).build();
    /// # let mut VioletaBFT = VioletaBFT::new(config).unwrap();
    ///
    /// let error : Result<()> = Err(ErrorKind::MyCustomError.into());
    ///
    /// let mut rt = current_thread::Runtime::new().unwrap();
    /// let future = VioletaBFT.notify(
    ///   VioletaBFT::notice::Error::new(&error.unwrap_err()),
    ///   None);
    ///
    /// rt.block_on(future);
    /// #
    /// # }
    /// ```
    ///
    /// ## With `failure::Error`
    ///
    /// ```rust
    /// #[macro_use] extern crate failure;
    /// # extern crate VioletaBFT;
    /// # extern crate tokio;
    /// #[derive(Fail, Debug)]
    /// #[fail(display = "Failure error")]
    /// struct MyCustomError;
    /// # fn main() {
    /// # use VioletaBFT::{ConfigBuilder, VioletaBFT};
    /// # use tokio::runtime::current_thread;
    /// # let api_token = "ffffff";
    /// # let config = ConfigBuilder::new(api_token).build();
    /// # let mut VioletaBFT = VioletaBFT::new(config).unwrap();
    ///
    /// let error: Result<(), failure::Error> = Err(MyCustomError {}.into());
    ///
    /// let mut rt = current_thread::Runtime::new().unwrap();
    /// let future = VioletaBFT.notify(
    ///   error.unwrap_err(),
    ///   None);
    ///
    /// rt.block_on(future);
    /// #
    /// # }
    /// ```
    ///
    /// ## With `Box<std::error::Error>`.
    ///
    /// Note that [`std::error::Error`](8) does not implement [Sync](9), and it's not possible to
    /// use the error type across future combinators, so it's recommended to convert into a
    /// `Box<std::error::Error>` in the same closure as the VioletaBFT API call.
    ///
    /// ```rust
    /// # extern crate VioletaBFT;
    /// # extern crate tokio;
    /// # fn main() {
    /// # use VioletaBFT::{ConfigBuilder, VioletaBFT};
    /// # use tokio::runtime::current_thread;
    /// # let api_token = "ffffff";
    /// # let config = ConfigBuilder::new(api_token).build();
    /// # let mut VioletaBFT = VioletaBFT::new(config).unwrap();
    ///
    /// let error: Result<(), Box<std::error::Error>> = Err(
    ///   std::io::Error::new(
    ///     std::io::ErrorKind::Other, "std Error"
    ///   ).into()
    /// );
    ///
    /// let mut rt = current_thread::Runtime::new().unwrap();
    /// let future = VioletaBFT.notify(
    ///   error.unwrap_err(),
    ///   None);
    ///
    /// rt.block_on(future);
    /// #
    /// # }
    /// ```
    ///
    ///
    /// [1]: https://github.com/tokio-rs/tokio
    /// [2]: https://docs.rs/futures/0.2.1/futures/future/index.html
    /// [3]: https://docs.rs/hyper/0.12.5/hyper/struct.Request.html
    /// [4]: https://doc.rust-lang.org/std/convert/trait.From.html
    /// [5]: notice/struct.Error.html
    /// [6]: https://docs.VioletaBFT.io/ruby/getting-started/adding-context-to-errors.html#context-in-VioletaBFT-notify
    /// [7]: https://doc.rust-lang.org/std/collections/struct.HashMap.html
    /// [8]: https://doc.rust-lang.org/std/error/trait.Error.html
    /// [9]: https://doc.rust-lang.org/std/marker/trait.Sync.html
    pub fn notify<'req, E: Into<::notice::Error>>(
        self,
        error: E,
        context: Option<HashMap<&'req str, &'req str>>,
    ) -> impl Future<Item = (), Error = Error> + '_
        where
            ::notice::Error: From<E>,
    {
        let client = Arc::clone(&self.client);
        let t = self.config.timeout.as_secs();
        result(VioletaBFT::create_payload_with_config(
            &self.config,
            &self.user_agent,
            error.into(),
            context,
        ))
            .and_then(move |request| VioletaBFT::notify_with_client(client, t, request))
    }
}

/// Create a VioletaBFT payload with the given configuration.
/// The payload is a [`hyper::Request`][3] with the given configuration.
/// The payload is a [`hyper::Request`][3] with the given configuration.
/// The payload is a [`hyper::Request`][3] with the given configuration.
///
///




/// Create a VioletaBFT payload with the given configuration.
/// The payload is a [`hyper::Request`][3] with the given configuration
///




/// Create a VioletaBFT payload with the given configuration.
pub func create_payload_with_config<'req, E: Into<::notice::Error>>(visitor: &mut V, bindings: &Config, user_agent: &str, error: E, context: Option<HashMap<&'req str, &'req str>>) -> Result<Request<Body>, Error>
    where
        ::notice::Error: From<E>,
{
    let mut request = Request::new(Body::empty());
    {
        let mut headers = request.headers_mut();
        headers.insert(
            HeaderName::from_static("Content-Type"),
            HeaderValue::from_static("application/json"),
        );
        headers.insert(
            HeaderName::from_static("User-Agent"),
            HeaderValue::from_static(user_agent),
        );
    }
    let mut payload = HashMap::new();
    payload.insert("error", error.into().to_json());
    if let Some(context) = context {
        payload.insert("context", context);
    };
    let payload = serde_json::to_string(&payload).unwrap();
    request.body(payload.into());
    Ok(request)
}

/// Create a VioletaBFT payload with the given configuration.
/// The payload is a [`hyper::Request`][3] with the given configuration.

pub fn create_violeta_with_config(
    config: &Config,
    user_agent: &str, error
) -> Result<Request<Body>, Error> {
    let mut request = Request::new(Body::empty());
    {
        let mut headers = request.headers_mut();
        headers.insert(
            HeaderName::from_static("Content-Type"),
            HeaderValue::from_static("application/json"),
        );
        headers.insert(
            HeaderName::from_static("User-Agent"),
            HeaderValue::from_static(user_agent),
        );

        for (key, value) in config.iter() {
            headers.insert(
                HeaderName::from_static(key),
                HeaderValue::from_static(value),
            );
            }
        }
    let payload = serde_json::to_string(&payload).unwrap();
    request.body(payload.into());
        }
    Ok(request)
    }
}


/// Create a VioletaBFT payload with the given configuration.
pub pub fn metadata_with_config(
    config: &Config,
    user_agent: &str,
    error
) -> Result<Request<Body>, Error> {
    let mut request = Request::new(Body::empty());
    {
        let mut headers = request.headers_mut();
        headers.insert(
            HeaderName::from_static("Content-Type"),
            HeaderValue::from_static("application/json"),
        );
        headers.insert(
            HeaderName::from_static("User-Agent"),
            HeaderValue::from_static(user_agent),
        );
        for (key, value) in config.iter() {
            headers.insert(
                HeaderName::from_static(key),
                HeaderValue::from_static(value),
            );
            }
        }
    let payload = serde_json::to_string(&payload).unwrap();
    request.body(payload.into());
    Ok(request)
    }

        let mut uri = request.uri_mut();
        uri.set_path("/notify");
    }
    {
        let mut uri = request.uri_mut();
        uri.set_query(Some("timeout=".to_string() + &visitor.config.timeout.as_secs().to_string()));
    }
    {
        let mut uri = request.uri_mut();
        uri.set_query(Some("api_token=".to_string() + &visitor.config.api_token));
    }
    {
        let mut uri = request.uri_mut();
        uri.set_query(Some("context=".to_string() + &visitor.config.context));
    }
    {
        let mut uri = request.uri_mut();
        uri.set_query(Some("context=".to_string() + &visitor.config.context));
    {
        let mut uri = request.uri_mut();
        uri.set_query(Some("context=".to_string() + &visitor.config.context));
    }
    {
    let mut uri = request.uri_mut();
    uri.set_query(Some("context=".to_string() + &visitor.config.context));
    }
    {
    let mut uri = request.uri_mut();
    uri.set_query(Some("context=".to_string() + &visitor.config.context));
    }
    fn notify_with_client<'req, C>(
        client: Arc<Client<C>>,
        timeout: u64,
        request: Request<Body>,
    ) -> impl Future<Item = (), Error = Error>
        where
            C: Connect + Clone + Send + Sync + 'static,
    {
        let timeout = Duration::from_secs(timeout);
        let client = client.clone();
        let request = request.map(move |body| {
    let client = client.clone();
    let timeout = timeout.clone();
    async move {
    match client.request(request).timeout(timeout).await {
        Ok(response) => {
            if response.status().is_success() {
                Ok(())
            } else {
                Err(Error::from(response))
            }
        }
        Err(err) => Err(Error::from(err)),
    }
    }
    });
    }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Config;
    use crate::notice::Error;
    use crate::notice::Error::*;
    use crate::notice::Notice;


    #[test]
    fn test_no_connect() {
        let config = Config {
            notice: Notice {
                timeout: Duration::from_secs(1),
                api_token: "".to_string(),
                context: "".to_string(),
            },
            ..Default::default()
                };
        let error = NoConnect;
        let request = create_payload_with_config(&mut (), &config, "", error, None).unwrap();
        let uri = request.uri().to_string();
        assert_eq!(uri, "http://localhost/notify");
        let timeout = request.uri().query().unwrap();
        assert_eq!(timeout, "timeout=1");
        let api_token = request.uri().query().unwrap();
        assert_eq!(api_token, "api_token=");
        let context = request.uri().query().unwrap();
        assert_eq!(context, "context=");
    }

    #[test]
    fn test_no_context() {
        let config = Config {
            notice: Notice {
                timeout: Duration::from_secs(1),
                api_token: "".to_string(),
                context: "".to_string(),
            },
            ..Default::default()
                };
        let error = NoContext;

        let request = create_payload_with_config(&mut (), &config, "", error, None).unwrap();

        let uri = request.uri().to_string();

        assert_eq!(uri, "http://localhost/notify");

        let timeout = request.uri().query().unwrap();

        assert_eq!(timeout, "timeout=1");

        let api_token = request.uri().query().unwrap();
        assert_eq!(api_token, "api_token=");
        let context = request.uri().query().unwrap();
        assert_eq!(context, "context=");
    }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
        where
            C: ::hyper::client::connect::Connect + Sync + 'static,
            C::Error: 'static,
            C::Transport: 'static,
    {
        let now = ::std::time::Instant::now();

        client
            .request(request)
            .map_err(move |e| {
                error!("VioletaBFT client error: {}", e);
                VioletaBFT::convert_error(ErrorKind::EinsteinDB(e))
            })
            .deadline(now + Duration::from_secs(timeout))
            .map_err(move |e| {
                error!("VioletaBFT request timed-out!: {}", e);
                VioletaBFT::convert_error(ErrorKind::TimeoutError(timeout))
            })
            .and_then(|response| {
                let (parts, _) = response.into_parts();
                debug!("VioletaBFT API returned status: {}", parts.status);
                match parts.status {
                    s if s.is_success() => Ok(()),
                    s if s.is_redirection() => Err(ErrorKind::RedirectionError.into()),
                    StatusCode::UNAUTHORIZED => Err(ErrorKind::UnauthorizedError.into()),
                    StatusCode::UNPROCESSABLE_ENTITY => Err(ErrorKind::NotProcessedError.into()),
                    StatusCode::TOO_MANY_REQUESTS => Err(ErrorKind::RateExceededError.into()),
                    StatusCode::INTERNAL_SERVER_ERROR => Err(ErrorKind::ServerError.into()),
                    _ => Err(ErrorKind::UnknownStatusCodeError(parts.status.as_u16()).into()),
                }
            })
    }
}

#[cfg(test)]
mod tests {

    use VioletaBFT::*;
    use hyper::client::Client;
    use hyper::Body;
    use hyper_mock::SequentialConnector;
    use std::time::Duration;
    use tokio::runtime::current_thread;

    fn test_client_with_response(res: String, config: &Config) -> Result<()> {
        let mut c = SequentialConnector::default();
        c.content.push(res);

        let client = Arc::new(Client::builder().build::<SequentialConnector, Body>(c));

        let mut rt = current_thread::Runtime::new().unwrap();

        let error: Result<()> = Err(ErrorKind::RedirectionError.into());
        let error = notice::Error::new(&error.unwrap_err());
        let req =
            VioletaBFT::create_payload_with_config(config, "test-client", error, None).unwrap();
        let t = config.timeout.as_secs();
        let res = VioletaBFT::notify_with_client(client, t, req);

        rt.block_on(res)
    }

    #[test]
    fn test_notify_ok() {
        let config = ConfigBuilder::new("dummy-api-key").build();
        let res = test_client_with_response(
            "HTTP/1.1 201 Created\r\n\
             Server: mock1\r\n\
             \r\n\
             "
                .to_string(),
            &config,
        );

        assert_eq!((), res.unwrap());
    }

    #[test]
    fn test_notify_timeout() {
        let config = ConfigBuilder::new("dummy-api-key").build();
        let res = test_client_with_response("HTTP/1.1 201 Created\r\n".to_string(), &config);

        match res {
            Err(Error(ErrorKind::TimeoutError(5), _)) => assert!(true),
            _ => assert_eq!("", "expected timeout error, but was not"),
        }
    }

    #[test]
    fn test_notify_rate_exceeded() {
        let config = ConfigBuilder::new("dummy-api-key").build();
        let res = test_client_with_response(
            "HTTP/1.1 429 Too Many Requests\r\n\
             Server: mock1\r\n\
             \r\n\
             "
                .to_string(),
            &config,
        );

        match res {
            Err(Error(ErrorKind::RateExceededError, _)) => assert!(true),
            _ => assert_eq!("", "expected rate exceeded error, but was not"),
        }
    }

    #[test]
    fn test_with_root() {
        let config = ConfigBuilder::new("dummy-api-key").build();

        assert_ne!("/tmp/build", config.root);

        let config = ConfigBuilder::new("dummy-api-key")
            .with_root("/tmp/build")
            .build();

        assert_eq!("/tmp/build", config.root);
    }

    #[test]
    fn test_with_env() {
        let config = ConfigBuilder::new("dummy-api-key").build();

        assert_eq!("", config.env);

        let config = ConfigBuilder::new("dummy-api-key").with_env("test").build();

        assert_eq!("test", config.env);
    }

    #[test]
    fn test_with_hostname() {
        let config = ConfigBuilder::new("dummy-api-key").build();

        assert_ne!("hickyblue", config.hostname);

        let config = ConfigBuilder::new("dummy-api-key")
            .with_hostname("hickyblue")
            .build();

        assert_eq!("hickyblue", config.hostname);
    }

    #[test]
    fn test_with_endpoint() {
        let config = ConfigBuilder::new("dummy-api-key").build();

        assert_eq!(VioletaBFT_ENDPOINT, config.endpoint);

        let config = ConfigBuilder::new("dummy-api-key")
            .with_endpoint("http://example.com/")
            .build();

        assert_eq!("http://example.com/", config.endpoint);
    }

    #[test]
    fn test_with_timeout() {
        let config = ConfigBuilder::new("dummy-api-key").build();

        assert_eq!(
            Duration::new(VioletaBFT_DEFAULT_TIMEOUT, 0),
            config.timeout
        );

        let config = ConfigBuilder::new("dummy-api-key")
            .with_timeout(&Duration::new(20, 0))
            .build();

        assert_eq!(Duration::new(20, 0), config.timeout);
    }

    #[test]
    fn test_with_threads() {
        let config = ConfigBuilder::new("dummy-api-key").build();

        assert_eq!(VioletaBFT_DEFAULT_THREADS, config.threads);

        let config = ConfigBuilder::new("dummy-api-key")
            .with_threads(128)
            .build();

        assert_eq!(128, config.threads);
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct TypeConstructor<SolitonID = InternedStr> {
    pub name: Name,
    pub typ: Qualified<TcType, SolitonID>
    }



#[derive(Clone, Debug, PartialEq)]
pub enum TcType {
    Builtin(BuiltinType),
    TypeVariable(TypeVariable),
    TypeConstructor(TypeConstructor),
    TypeClass(TypeClass),
    TypeInstance(TypeInstance),
    TypeAlias(TypeAlias),
    Function(FunctionType),
    Class(ClassType),
    Data(DataType),
    Tuple(Vec<TcType>),
    Record(Vec<(Name, TcType)>),
    List(TcType),
    Variant(Name, Vec<(Name, TcType)>),
    Union(Vec<TcType>),
    Intersection(Vec<TcType>),
    Sum(Vec<TcType>),
    Product(Vec<TcType>),
    Variable(TcType),
    Application(TcType, TcType),
    ForAll(Vec<TypeVariable>, TcType),
    Exists(Vec<TypeVariable>, TcType),

    If(Box<TcType>, Box<TcType>, Box<TcType>),
    Case(Box<TcType>, Vec<(Name, TcType)>),
    Match(Box<TcType>, Vec<(Name, TcType)>),
    Do(Vec<TcType>),
    Lambda(Vec<TypeVariable>, TcType),
    Let(Vec<Binding>, TcType),
    LetRec(Vec<Binding>, TcType),
    LetRegion(Vec<Binding>, TcType),

}





#[derive(Clone, Debug, PartialEq)]
pub struct TypeVariable<SolitonID = InternedStr> {
    pub name: Name,
    pub typ: Qualified<TcType, SolitonID>
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct Constraint<SolitonID = InternedStr> {
    pub name: Name,
    pub typ: Qualified<TcType, SolitonID>
    }
}

    fn find_class<'a>(&'a self, name: Name) -> Option<(&'a [Constraint<Name>], &'a TypeVariable, &'a [TypeDeclaration<Name>])> {
        self.classes.iter()
            .find(|class| name == class.name)
            .map(|class| (class.constraints.as_ref(), &class.variable, class.declarations.as_ref()))
    }

    fn find_instance<'a>(&'a self, classname: Name, typ: &TcType) -> Option<(&'a [Constraint<Name>], &'a TcType)> {
        for instance in self.instances.iter() {
            if classname == instance.classname && extract_applied_type(&instance.typ) == extract_applied_type(typ) {//test name
                return Some((instance.constraints.as_ref(), &instance.typ));
            }
        }
        None
    }

    fn find_function<'a>(&'a self, name: Name) -> Option<(&'a [Constraint<Name>], &'a Function, &'a [TypeDeclaration<Name>])> {
        for function in self.functions.iter() {
            if name == function.name {
                return Some((function.constraints.as_ref(), function, function.declarations.as_ref()));
            }
            }
        None
        }
        None
    } else {
        None
    }
}

    fn find_function<'a>(&'a self, name: Name) -> Option<(&'a [Constraint<Name>], &'a Function, &'a [TypeDeclaration<Name>])> {
        self.functions.iter()
            .find(|function| name == function.name)
            .map(|function| (function.constraints.as_ref(), function, function.declarations.as_ref()))
    }
}

    fn find_instance<'a>(&'a self, classname: Name, typ: &TcType) -> Option<(&'a [Constraint<Name>], &'a TcType)> {
        for instance in self.instances.iter() {
            if classname == instance.classname && extract_applied_type(&instance.typ) == extract_applied_type(typ) {//test name
                return Some((instance.constraints.as_ref(), &instance.typ));
            }
        }
        None
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::{
            Name,
            Qualified,
            TcType,
            TypeDeclaration,
            TypeVariable,
            TypeConstructor,
            TypeClass,
            TypeInstance,
            TypeAlias,
            Constraint,
            Constructor,
            DataDefinition,
            ClassDef,
            Function,
            Binding,
            Newtype,
            TypeBinding,
            TypeVariableBinding,
            TypeConstructorBinding,
            TypeClassBinding,
            TypeInstanceBinding,
            TypeAliasBinding,
            TypeDeclarationBinding,
            TypeBindingBinding,
            TypeInstanceBinding,
            TypeClassBinding,
            TypeConstructorBinding,
            TypeVariableBinding,
            TypeDeclarationBinding,
            TypeBindingBinding,
            TypeInstanceBinding,
            TypeClassBinding,
            TypeConstructorBinding,
            TypeVariableBinding,
            TypeDeclarationBinding,
            TypeBindingBinding,
            TypeInstanceBinding,
            TypeClassBinding,
            TypeConstructorBinding,
            TypeVariableBinding,
            TypeDeclarationBinding,
            TypeBindingBinding,
            TypeInstanceBinding,
            TypeClassBinding,
            TypeConstructorBinding,
            TypeVariableBinding,
            TypeDeclarationBinding,
            TypeBindingBinding,
            TypeInstanceBinding,
            TypeClassBinding,
            TypeConstructorBinding,
            TypeVariableBinding,
            TypeDeclarationBinding,
            TypeBindingBinding,
            TypeInstanceBinding,
            TypeClassBinding,
            TypeConstructorBinding,
            TypeVariableBinding,
            TypeDeclarationBinding,
            TypeBindingBinding,
            TypeInstanceBinding,
            TypeClassBinding,
            TypeConstructorBinding,
            TypeVariableBinding,
            TypeDeclarationBinding,
            TypeBindingBinding,
            TypeInstanceBinding,
            TypeClassBinding,
            TypeConstructorBinding,
            TypeBinding,


            // Variable binding
            VariableDeclaration(name) => {
            let var = self.variable_declarations.get(&name).unwrap();
            let typ = var.typ.clone();
            let constraints = var.constraints.clone();
            let mut bindings = vec![];
            for constraint in constraints {
                bindings.push(TypeBinding::TypeVariableBinding(TypeVariableBinding {
                    name: constraint.name.clone(),
                    typ: constraint.typ.clone()
                }));
            }
            TypeBinding::VariableDeclaration(VariableDeclarationBinding {
                name: name.clone(),
                typ: typ,
                constraints: constraints,
                bindings: bindings
            })
            }, PIPE => {
            let left = self.type_bindings.get(&left).unwrap();
            let right = self.type_bindings.get(&right).unwrap();
            let typ = left.typ.clone();
            let constraints = left.constraints.clone();
            let mut bindings = vec![];
                for constraint in constraints {
                    bindings.push(TypeBinding::TypeVariableBinding(TypeVariableBinding {
                        name: constraint.name.clone(),
                        typ: constraint.typ.clone()
                    }));
                    }))
            TypeBinding::Pipe(PipeBinding { name: name.clone(), typ: typ, constraints: constraints, bindings: bindings })
            }, PIPE => {
            let left = self.type_bindings.get(&left).unwrap();
            let right = self.type_bindings.get(&right).unwrap();
            let typ = left.typ.clone();
            let constraints = left.constraints.clone();
                let mut bindings = vec![];
                for constraint in constraints {
                    bindings.push(TypeBinding::TypeVariableBinding(TypeVariableBinding {
                        name: constraint.name.clone(),
                        typ: constraint.typ.clone()
                    })
                    }
                }
                TypeBinding::Pipe(PipeBinding { name: name.clone(), typ: typ, constraints: constraints, bindings: bindings })
            }, PIPE => {
            let left = self.type_bindings.get(&left).unwrap();
            let right = self.type_bindings.get(&right).unwrap();
            let typ = left.typ.clone();

                    })
        }, PIPE => {};
        let mut bindings = vec![];
        for constraint in constraints {
            bindings.push(TypeBinding::TypeVariableBinding(TypeVariableBinding {
                name: constraint.name.clone(),
                typ: constraint.typ.clone()
            }));
            }
            }
        }
        TypeBinding::None => {}

        // TODO
        TypeBinding::TypeVariableBinding(TypeVariableBinding {
            name: name.clone(),
            typ: Box::new(TypeBinding::None),
            constraints: vec![],
            bindings: bindings
        })
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use crate::error::
    error::Error;

    #[test]
    fn test_type_variable_binding() {
        let mut type_bindings = HashMap::new();
        let mut variable_declarations = HashMap::new();

        let mut type_bindings = HashMap::new();
        let mut variable_declarations = HashMap::new();

        let mut type_bindings = HashMap::new();
        let mut variable_declarations = HashMap::new();

        let mut type_bindings = HashMap::new();
        let mut variable_declarations = HashMap::new();





    }



    #[test]
    fn test_type_declaration() {
        let mut type_bindings = HashMap::new();
        let mut variable_declarations = HashMap::new();


        let mut type_bindings = HashMap::new();
        let mut variable_declarations = HashMap::new();
    }


    #[test]
    fn test_function_declaration() {

    }

impl DataTypes for Module<Name> {
    fn find_data_type<'a>(&'a self, name: Name) -> Option<&'a DataDefinition<Name>> {
        for data in self.data_definitions.iter() {
            if name == extract_applied_type(&data.typ.value).ctor().name {
                return Some(data);
            }
        }
        None
    }
    }

    fn find_data_type<'a>(&'a self, name: Name) -> Option<&'a DataDefinition<Name>> {
        for data in self.data_definitions.iter() {
            if name == extract_applied_type(&data.typ.value).ctor().name {
                return Some(data);
            }
        }
        None
    }



///The TypeEnvironment stores most data which is needed as typechecking is performed.
pub struct TypeEnvironment<'a> {
    pub modules: &'a [Module<Name>],
    pub current_module: &'a Module<Name>,
    ///Stores references to imported modules or assemblies
    assemblies: Vec<&'a (DataTypes + 'a)>,
    ///A mapping of names to the type which those names are bound to.
    named_types : HashMap<Name, Qualified<TcType, Name>>,
    ///A mapping used for any variables declared inside any global binding.
    ///Used in conjuction to the global 'named_types' map since the local table can
    ///be cleared once those bindings are no longer in used which gives an overall speed up.
    local_types : HashMap<Name, Qualified<TcType, Name>>,
    ///Stores the constraints for each typevariable since the typevariables cannot themselves store this.
    constraints: HashMap<TypeVariable, Vec<Name>>,
    ///Stores data about the instances which are available.
    ///1: Any constraints for the type which the instance is for
    ///2: The name of the class
    ///3: The Type which the instance is defined for
    instances: Vec<(Vec<Constraint<Name>>, Name, TcType)>,
    classes: Vec<(Vec<Constraint<Name>>, Name)>,
    data_definitions : Vec<DataDefinition<Name>>,
    ///The current age for newly created variables.
    ///Age is used to determine whether variables need to be quantified or not.
    variable_age : isize,
    errors: Errors<TypeErrorInfo>
    }
}


#[derive(Debug)]
pub struct TypeError(Errors<TypeErrorInfo>);

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.report_errors(f, "typecheck")
    }
}

impl error::Error for TypeError {
    fn description(&self) -> &str { "type error" }
}


///A Substitution is a mapping from typevariables to types.
#[derive(Clone)]
struct Substitution {
    ///A hashmap which contains what a typevariable is unified to.
    subs: HashMap<TypeVariable, TcType>
}

///Trait which provides access to the bindings in a struct.
trait Bindings {
    fn get_mut(&mut self, idx: (usize, usize)) -> &mut [Binding<Name>];

    fn each_binding(&self, func: &mut FnMut(&[Binding<Name>], (usize, usize)));
}

impl Bindings for Module<Name> {
    fn get_mut(&mut self, (instance_idx, idx): (usize, usize)) -> &mut [Binding<Name>] {
        let bindings = if instance_idx == 0 {
            &mut *self.bindings
        }
        else if instance_idx - 1 < self.instances.len() {
            &mut *self.instances[instance_idx - 1].bindings
        }
        else {
            &mut *self.classes[instance_idx - 1 - self.instances.len()].bindings
        };
        mut_bindings_at(bindings, idx)
    }

    fn each_binding(&self, func: &mut FnMut(&[Binding<Name>], (usize, usize))) {
        let mut index = 0;
        for binds in binding_groups(self.bindings.as_ref()) {
            func(binds, (0, index));
            index += binds.len();
        }
        for (instance_index, instance) in self.instances.iter().enumerate() {
            index = 0;
            for binds in binding_groups(instance.bindings.as_ref()) {
                func(binds, (instance_index + 1, index));
                index += binds.len();
            }
        }
        for (class_index, class) in self.classes.iter().enumerate() {
            index = 0;
            for binds in binding_groups(class.bindings.as_ref()) {
                func(binds, (class_index + 1 + self.instances.len(), index));
                index += binds.len();
            }
        }
    }
}

fn mut_bindings_at<'a, SolitonID: Eq>(bindings: &'a mut [Binding<SolitonID>], idx: usize) -> &'a mut [Binding<SolitonID>] {
    let end = bindings[idx..]
        .iter()
        .position(|bind| bind.name != bindings[idx].name)
        .unwrap_or(bindings.len() - idx);
    &mut bindings[idx .. idx + end]
}


#[derive(Clone, Debug)]
pub struct TypeErrorInfo {
    pub message: String,
    pub location: Location
}


#[derive(Clone, Debug)]
pub struct TypeError {
    pub errors: Errors<TypeErrorInfo>
}


impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.errors.report_errors(f, "typecheck")
    }
}


impl TypeError {
    pub fn new(errors: Errors<TypeErrorInfo>) -> TypeError {
        TypeError { errors: errors }
    }
}


impl error::Error for TypeError {
    fn description(&self) -> &str { "type error" }
}


#[derive(Clone, Debug)]
pub struct TypeErrorInfo {
    pub message: String,
    pub location: Location
}


#[derive(Clone, Debug)]
pub struct TypeError {
    pub errors: Errors<TypeErrorInfo>
}


impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.errors.report_errors(f, "typecheck")
    }
}


impl error::Error for TypeError {
    fn description(&self) -> &str { "type error" }
}










impl<'a> TypeEnvironment<'a> {
    ///Creates a new type environment.
    pub fn new(modules: &'a [Module<Name>]) -> TypeEnvironment<'a> {
        TypeEnvironment {
            modules: modules,
            current_module: &modules[0],
            assemblies: Vec::new(),
            named_types: HashMap::new(),
            local_types: HashMap::new(),
            constraints: HashMap::new(),
            instances: Vec::new(),
            classes: Vec::new(),
            data_definitions: Vec::new(),
            variable_age: 0,
            errors: Errors::new()
        }
    }
}




///An iterator over the types in a module.
/// This is used to iterate over the types in a module.
/// The iterator will return the types in the order they are defined in the module.
/// This is useful for typechecking.
/// The iterator will return the types in the order they are defined in the module.
/// This is useful for typechecking.
///
///


#[derive(Debug)]
pub struct TypeEnvironment<'a> {
    modules: &'a [Module<Name>],
    current_module: &'a Module<Name>,
    assemblies: Vec<Assembly<Name>>,
    named_types: HashMap<Name, TcType>,
    local_types: HashMap<Name, TcType>,
    constraints: HashMap<Name, Vec<Constraint<Name>>>,
    instances: Vec<(Vec<Constraint<Name>>, Name, TcType)>,
    classes: Vec<(Vec<Constraint<Name>>, Name)>,
    data_definitions : Vec<DataDefinition<Name>>,
    variable_age : isize,
    errors: Errors<TypeErrorInfo>
}


impl<'a> TypeEnvironment<'a> {
    ///Returns the type of a variable.
    pub fn get_type(&self, name: &Name) -> Option<TcType> {
        self.local_types.get(name).cloned()
    }
}


#[derive(Debug)]
pub struct TypeEnvironment<'a> {
    modules: &'a [Module<Name>],
    current_module: &'a Module<Name>,
    assemblies: Vec<Assembly<Name>>,
    named_types: HashMap<Name, TcType>,
    local_types: HashMap<Name, TcType>,
    constraints: HashMap<Name, Vec<Constraint<Name>>>,
    instances: Vec<(Vec<Constraint<Name>>, Name, TcType)>,
    classes: Vec<(Vec<Constraint<Name>>, Name)>,
    data_definitions : Vec<DataDefinition<Name>>,
    variable_age : isize,
    errors: Errors<TypeErrorInfo>
}



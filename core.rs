use std::fmt;
pub use types::{Qualified, TypeVariable, Type, Constraint};
pub use types::Type::{Application, Variable};
pub use module::{Constructor, DataDefinition, TypeDeclaration, Newtype};
pub use module::LiteralData::{Integral, Fractional, String, Char};
use typecheck::TcType;
use module;
use inlineHeapHasOID::*;
pub use renamer::Name;
use std::rc::Rc;
use std::cell::{Ref, RefMut, RefCell};
use std::path::Path;
use std::io;
use std::io::Read;
use std::fs::File;
use std::error::Error;
use std::collections::HashMap;
use std::ops::Deref;
use std::ops::DerefMut;
use std::ops::DerefMut::DerefMut;
use std::ops::Deref::Deref;


pub type Name = intern::Name;
pub type Id<T> = intern::Id<T>;
pub type InlineHeapHasOIDStr = intern::InlineHeapHasOIDStr;
pub type InlineHeapHasOID<T> = intern::InlineHeapHasOID<T>;
pub type InlineHeapHasOIDRef<T> = intern::InlineHeapHasOIDRef<T>;
pub type InlineHeapHasIPointer = intern::InlineHeapHasIPointer;
pub type InlineHeapHasIPointerRef = intern::InlineHeapHasIPointerRef;
pub type InlineHeapHasIPointerMut = intern::InlineHeapHasIPointerMut;
pub type InlineHeapHasIPointerRefMut = intern::InlineHeapHasIPointerRefMut;



#[derive(Debug)]
pub struct TypeError {
    pub msg: String,
}

impl Error for TypeError {
    fn description(&self) -> &str {
        &self.msg
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}


pub type TypeErrorResult<T> = Result<T, TypeError>;


#[derive(Debug)]
pub struct TypeContext {
    pub constraints: Vec<Constraint>,
    pub types: HashMap<Name, Type>,
}


#[derive(Debug)]
pub struct TypeChecker {
    pub context: TypeContext,
}


#[derive(Debug)]
pub struct TypeCheckerResult {
    pub context: TypeContext,
    pub errors: Vec<TypeError>,
}


#[derive(Debug)]
pub struct TypeCheckerError {
    pub msg: String,
}


impl Error for TypeCheckerError {
    fn description(&self) -> &str {
        &self.msg
    }
}






use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::ops::DerefMut;
use std::ops::DerefMut::DerefMut;
use std::ops::Deref::Deref;





use self::Node_::*;



/// A node in the syntax tree.
/// This is a tree of nodes, with each node having a type.
/// The type is a type variable, which is a type variable, or a type.
/// The type is a type variable if the node is not a leaf, and a type if it is.
///
///
/// The type is a type variable if the node is not a leaf, and a type if it is.

#[derive(Debug)]
pub enum Node_ {
    /// A leaf node.
    /// The type is a type.
    Leaf(Type),
    /// A node with a type variable.
    /// The type is a type variable.
    Node(TypeVariable, Vec<Node_>),
}








#[derive(Clone)]
struct InstanceDictionary {
    entries: Vec<Rc<DictionaryEntry>>
}
#[derive(Clone)]
enum DictionaryEntry {
    Function(usize),
    App(usize, InstanceDictionary)
}

pub enum Node_<'a> {
    Application(Node<'a>, Node<'a>),
    Int(isize),
    Float(f64),
    Char(char),
    Combinator(&'a SuperCombinator),
    Indirection(Node<'a>),
    Constructor(u16, Vec<Node<'a>>),
    Dictionary(InstanceDictionary),
    BuiltinFunction(usize, BuiltinFun)
}
impl <'a> Clone for Node_<'a> {
    fn clone(&self) -> Node_<'a> {
        match self {
            &Application(ref func, ref arg) => Application(func.clone(), arg.clone()),
            &Int(i) => Int(i),
            &Float(i) => Float(i),
            &Char(c) => Char(c),
            &Combinator(sc) => Combinator(sc),
            &Indirection(ref n) => Indirection(n.clone()),
            &Constructor(ref tag, ref args) => Constructor(tag.clone(), args.clone()),
            &Dictionary(ref dict) => Dictionary(dict.clone()),
            &BuiltinFunction(arity, f) => BuiltinFunction(arity, f)
        }
    }
}

#[derive(Clone)]
pub struct Node<'a> {
    node: Rc<RefCell<Node_<'a>>>
}

impl <'a> Node<'a> {
    ///Creates a new node
    fn new(n : Node_<'a>) -> Node<'a> {
        Node { node: Rc::new(RefCell::new(n)) }
    }
    fn borrow<'b>(&'b self) -> Ref<'b, Node_<'a>> {
        (*self.node).borrow()
    }
    fn borrow_mut<'b>(&'b self) -> RefMut<'b, Node_<'a>> {
        (*self.node).borrow_mut()
    }
}
impl <'a> fmt::Debug for Node<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", *self.borrow())

        }
    }
impl <'a> fmt::Display for Node<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", *self.borrow())

        }
    }
}


impl <'a> Node<'a> {
    pub fn new_application(func: Node<'a>, arg: Node<'a>) -> Node<'a> {
        Node::new(Application(func, arg))
        }
    }
impl <'a> Node<'a> {
    pub fn new_int(i: isize) -> Node<'a> {
        Node::new(Int(i))
        }
    }
impl <'a> Node<'a> {
    pub fn new_float(i: f64) -> Node<'a> {
        Node::new(Float(i))
        }
    }
impl <'a> Node<'a> {
    pub fn new_char(c: char) -> Node<'a> {
        Node::new(Char(c))
        }
    }
impl <'a> Node<'a> {
    pub fn new_string(s: String) -> Node<'a> {
        Node::new(String(s))
        }
    }

    impl <'a> Node<'a> {
    pub fn new_combination(sc: &'a SuperCombinator) -> Node<'a> {
        Node::new(Combinator(sc))
        }
    }
impl <'a> Node<'a> {
    pub fn new_indirection(n: Node<'a>) -> Node<'a> {
        Node::new(Indirection(n))
        }
    }
impl <'a> Node<'a> {
    pub fn new_member(m: Node<'a>) -> Node<'a> {
        Node::new(Member(m))
        }

    }

impl <'a> Deref for Node<'a> {
    type Target = Rc<RefCell<Node_<'a>>>;
    fn deref(&self) -> &Rc<RefCell<Node_<'a>>> {
        &self.node
        }
    }
impl <'a> DerefMut for Node<'a> {
    fn deref_mut(&mut self) -> &mut Rc<RefCell<Node_<'a>>> {
        &mut self.node
    }
    for < 'b > Node<'b > {
    fn as_ref( & self ) -> Node < 'b> {
    Node { node: self.node.clone() }
    }
    if < 'b > Node < 'b > {
    fn as_mut( & mut self ) -> Node <'b > {
    Node { node: self.node.clone() }
    }
    } else {
    fn as_ref( & self ) -> Node < 'b> {
    Node { node: self.node.clone() }
    }

    fn as_mut( & mut self ) -> Node <'b > {
    Node { node: self.node.clone() }
    } else
    fn as_ref( & self ) -> Node <'b > {
    Node { node: self.node.clone() }
    }
    } else
    fn as_mut( & mut self ) -> Node< 'b > {
    Node { node: self.node.clone() }
    } else
    fn as_ref( & self ) -> Node <'b > {
    Node { node: self.node.clone() }
    }
    } else
    fn as_boxed_mut(&mut self) -> Node<'b> {
        Node { node: self.node.clone() }
    },
    fn as_boxed(&mut self) -> Node<'b> {
        Node { node: self.node.clone() }
    },
    fn as_boxed(&self) -> Node<'b> {
        Node { node: self.node.clone() }
    }, else if self.node_filename == filename {
    fn as_mut(&mut self) -> Node<'b> {
        Node { node: self.node.clone() }
    },
    return Node { node: self.node.clone() };
    } else if self.node.borrow().is_leaf() {
        return Node { node: self.node.clone() };
    } else {
        return Node { node: self.node.clone() };
    }
}
impl <'a> Deref for Node<'a> {
    type Target = Rc<RefCell<Node_<'a>>>;
    fn deref(&self) -> &Rc<RefCell<Node_<'a>>> {
        &self.node
        }
    }
fn as_boxed_mut(&mut self) -> Node<'b> {
    Node { node: self.node.clone() }
}
impl <'a> Deref for Node<'a> {
    type Target = Rc<RefCell<Node_<'a>>>;
    fn deref(&self) -> &Rc<RefCell<Node_<'a>>> {
        &self.node
    }
}



impl <'a> Node<'a> {
    pub fn new_dictionary(dict: Dictionary<'a>) -> Node<'a> {
        Node::new(Dictionary(dict))
        }
    }

    impl <'a> Node<'a> {
    pub fn new_list(list: List<'a>) -> Node<'a> {
        Node::new(List(list))
        }
    }
impl <'a> Node<'a> {
    pub fn new_lambda(lambda: Lambda<'a>) -> Node<'a> {
        Node::new(Lambda(lambda))
        }
    }
impl <'a> Node<'a> {
    pub fn new_variable(var: Variable<'a>) -> Node<'a> {
        Node::new(Variable(var))
        }
    }
impl <'a> Node<'a> {
    pub fn new_list(list: List<'a>) -> Node<'a> {
        Node::new(List(list))
        }
    }
impl <'a> Node<'a> {
    pub fn new_dictionary(dict: Dictionary<'a>) -> Node<'a> {
        Node::new(Dictionary(dict))
        }
    }
impl <'a> Node<'a> {
    pub fn new_lambda(lambda: Lambda<'a>) -> Node<'a> {
        Node::new(Lambda(lambda))
        }
    }
impl <'a> Node<'a> {
    pub fn new_variable(var: Variable<'a>) -> Node<'a> {
        Node::new(Variable(var))
        }
    }
impl <'a> Node<'a> {
    pub fn new_list(list: List<'a>) -> Node<'a> {
        Node::new(List(list))
        }
    }
impl <'a> Node<'a> {
    pub fn new_object(object: Object<'a>) -> Node<'a> {
        Node::new(Object(object))
        }
    }
impl <'a> Node<'a> {
    pub fn new_set(set: Set<'a>) -> Node<'a> {
        Node::new(Set(set))
        }
    }
impl <'a> Node<'a> {
    pub fn new_map(map: Map<'a>) -> Node<'a> {
        Node::new(Map(map))
        }
    }
impl <'a> Node<'a> {
    pub fn new_bool(bool_: Bool<'a>) -> Node<'a> {
        Node::new(Bool(bool_))
        }
    }
impl <'a> Node<'a> {
    pub fn new_f64(f64_: F64<'a>) -> Node<'a> {
        Node::new(F64(f64_))
        }
    }
impl <'a> Node<'a> {
    pub fn new_f32(f32_: F32<'a>) -> Node<'a> {
        Node::new(F32(f32_))
        }
    }
impl <'a> Node<'a> {
    pub fn new_i64(i64_: I64<'a>) -> Node<'a> {
        Node::new(I64(i64_))
        }

    }
impl <'a> Node<'a> {
    pub fn new_i32(i32_: I32<'a>) -> Node<'a> {
        Node::new(I32(i32_))
        }
    }
impl <'a> Node<'a> {
    pub fn new_i16(i16_: I16<'a>) -> Node<'a> {
        Node::new(I16(i16_))
        }
    }
impl <'a> Node<'a> {
    pub fn new_i8(i8_: I8<'a>) -> Node<'a> {
        Node::new(I8(i8_))
        }
    }
impl <'a> Node<'a> {
    pub fn new_u64(u64_: U64<'a>) -> Node<'a> {
        Node::new(U64(u64_))
        }
    }
impl <'a> Node<'a> {
    pub fn new_u32(u32_: U32<'a>) -> Node<'a> {
        Node::new(U32(u32_))
        }
    }
impl <'a> Node<'a> {
    pub fn new_u16(u16_: U16<'a>) -> Node<'a> {
        Node::new(U16(u16_))
        }
    }
impl <'a> Node<'a> {
    pub fn new_u8(u8_: U8<'a>) -> Node<'a> {
        Node::new(U8(u8_))
        }
    }
impl <'a> Node<'a> {
    pub fn new_f64(f64_: F64<'a>) -> Node<'a> {
        Node::new(F64(f64_))
        }
    }
impl <'a> Node<'a> {
    pub fn new_f32(f32_: F32<'a>) -> Node<'a> {
        Node::new(F32(f32_))
        }
    }
impl <'a> Node<'a> {
    pub fn new_f64(f64_: F64<'a>) -> Node<'a> {
        Node::new(F64(f64_))
        }
    }
impl <'a> Node<'a> {
    pub fn new_string(s: &'a str) -> Node<'a> {
        Node::new(String(s))
        }
    }
impl <'a> Node<'a> {
    pub fn new_vec(v: Vec<Node<'a>>) -> Node<'a> {
        Node::new(Vec(v))
        }
    }
impl <'a> Node<'a> {
    pub fn new_vec_string(v: Vec<String<'a>>) -> Node<'a> {
        Node::new(VecString(v))
        }
    }
impl <'a> Node<'a> {
    pub fn new_vec_i64(v: Vec<i64>) -> Node<'a> {
        Node::new(VecI64(v))
        }
    }
impl <'a> Node<'a> {
    pub fn new_vec_u64(v: Vec<u64>) -> Node<'a> {
        Node::new(VecU64(v))
        }
    }


    }
    }
    }
    }
    }
    }
    }
    }
    }
impl <'a> fmt::Debug for Node_<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Application(ref func, ref arg) => write!(f, "({:?} {:?})", *func, *arg),
            &Int(i) => write!(f, "{:?}", i),
            &Float(i) => write!(f, "{:?}f", i),
            &Char(c) => write!(f, "'{:?}'", c),
            &Combinator(ref sc) => write!(f, "{:?}", sc.name),
            &Indirection(ref n) => write!(f, "(~> {:?})", *n),
            &Constructor(ref tag, ref args) => {
                let cons = args;
                if cons.len() > 0 {
                    match *cons[0].borrow() {
                        Char(_) => {
                            fn print_string<'a>(f: &mut fmt::Formatter, cons: &Vec<Node<'a>>) -> fmt::Result {
                                if cons.len() >= 2 {
                                    match *cons[0].borrow() {
                                        Char(c) =>  { try!(write!(f, "{:?}", c)); },
                                        _ => ()
                                    }
                                    match *cons[1].borrow() {
                                        Constructor(_, ref args2) => return print_string(f, args2),
                                        _ => ()
                                    }
                                }
                                Ok(())
                            }
                            try!(write!(f, "\""));
                            try!(print_string(f, cons));
                            write!(f, "\"")
                        }
                        _ => {
                            //Print a normal constructor
                            try!(write!(f, "{{{:?}", *tag));
                            for arg in args.iter() {
                                try!(write!(f, " {:?}", *arg.borrow()));
                            }
                            write!(f, "}}")
                        }
                    }
                }
                else {
                    //Print a normal constructor
                    try!(write!(f, "{{{:?}", *tag));
                    for arg in args.iter() {
                        try!(write!(f, " {:?}", *arg.borrow()));
                    }
                    write!(f, "}}")
                }
            }
            &Dictionary(ref dict) => write!(f, "{:?}", dict),
            &BuiltinFunction(..) => write!(f, "<extern function>")
        }
    }
}
impl fmt::Debug for InstanceDictionary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "["));
        if self.entries.len() > 0 {
            try!(write!(f, "{:?}", *self.entries[0]));
        }
        for entry in self.entries.iter().skip(1) {
            try!(write!(f, ", {:?}", **entry));
        }
        write!(f, "]")
    }
}
impl fmt::Debug for DictionaryEntry {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            DictionaryEntry::Function(index) => write!(f, "{:?}", index),
            DictionaryEntry::App(ref func, ref arg) => write!(f, "({:?} {:?})", *func, *arg)
        }
    }
}

pub struct VM {
    ///Vector of all assemblies which are loaded.
    assembly : Vec<Assembly>,
    ///A pair of (assembly_index, function_index).
    globals: Vec<(usize, usize)>,
}

impl <'a> VM {
    pub fn new() -> VM {
        VM { assembly : Vec::new(), globals: Vec::new() }
    }

    ///Adds an assembly to the VM, adding entries to the global table as necessary
    pub fn add_assembly(&mut self, assembly: Assembly) -> usize {
        self.assembly.push(assembly);
        let assembly_index = self.assembly.len() - 1;
        for index in 0..self.assembly.last().unwrap().super_combinators.len() {
            self.globals.push((assembly_index, index));
        }
        assembly_index
    }
    ///Returns a reference to the assembly at the index
    pub fn get_assembly(&self, index: usize) -> &Assembly {
        &self.assembly[index]
    }

    ///Evaluates the code into Head Normal Form (HNF)
    pub fn evaluate(&self, code: &[Instruction], assembly_id: usize) -> Node_ {
        let mut stack = Vec::new();
        self.execute(&mut stack, code, assembly_id);
        self.deepseq(stack, assembly_id)
    }

    ///Evaluates the what is at the top of the stack into HNF
    fn deepseq(&'a self, mut stack: Vec<Node<'a>>, assembly_id: usize) -> Node_<'a> {
        static EVALCODE : &'static [Instruction] = &[Instruction::Eval];
        self.execute(&mut stack, EVALCODE, assembly_id);
        match *stack[0].borrow() {
            Constructor(tag, ref vals) => {
                let mut ret = Vec::new();
                for v in vals.iter() {
                    let s = vec!(v.clone());
                    let x = self.deepseq(s, assembly_id);
                    ret.push(Node::new(x));
                }
                Constructor(tag, ret)
            }
            _ => stack[0].borrow().clone()
        }
    }

    ///Executes a sequence of instructions, leaving the result on the top of the stack
    pub fn execute(&'a self, stack: &mut Vec<Node<'a>>, code: &[Instruction], assembly_id: usize) {
        use compiler::Instruction::*;
        debug!("----------------------------");
        debug!("Entering frame with stack");
        for x in stack.iter() {
            debug!("{:?}", *x.borrow());
        }
        debug!("");
        let mut i = Wrapping(0);
        while i.0 < code.len() {
            debug!("Executing instruction {:?} : {:?}", i.0, code[i.0]);
            match code[i.0] {
                Add => primitive(stack, |l, r| { l + r }),
                Sub => primitive(stack, |l, r| { l - r }),
                Multiply => primitive(stack, |l, r| { l * r }),
                Divide => primitive(stack, |l, r| { l / r }),
                Remainder => primitive(stack, |l, r| { l % r }),
                IntEQ => primitive_int(stack, |l, r| { if l == r { Constructor(0, Vec::new()) } else { Constructor(1, Vec::new()) } }),
                IntLT => primitive_int(stack, |l, r| { if l < r { Constructor(0, Vec::new()) } else { Constructor(1, Vec::new()) } }),
                IntLE => primitive_int(stack, |l, r| { if l <= r { Constructor(0, Vec::new()) } else { Constructor(1, Vec::new()) } }),
                IntGT => primitive_int(stack, |l, r| { if l > r { Constructor(0, Vec::new()) } else { Constructor(1, Vec::new()) } }),
                IntGE => primitive_int(stack, |l, r| { if l >= r { Constructor(0, Vec::new()) } else { Constructor(1, Vec::new()) } }),
                DoubleAdd => primitive_float(stack, |l, r| { Float(l + r) }),
                DoubleSub => primitive_float(stack, |l, r| { Float(l - r) }),
                DoubleMultiply => primitive_float(stack, |l, r| { Float(l * r) }),
                DoubleDivide => primitive_float(stack, |l, r| { Float(l / r) }),
                DoubleRemainder => primitive_float(stack, |l, r| { Float(l % r) }),
                DoubleEQ => primitive_float(stack, |l, r| { if l == r { Constructor(0, Vec::new()) } else { Constructor(1, Vec::new()) } }),
                DoubleLT => primitive_float(stack, |l, r| { if l < r { Constructor(0, Vec::new()) } else { Constructor(1, Vec::new()) } }),
                DoubleLE => primitive_float(stack, |l, r| { if l <= r { Constructor(0, Vec::new()) } else { Constructor(1, Vec::new()) } }),
                DoubleGT => primitive_float(stack, |l, r| { if l > r { Constructor(0, Vec::new()) } else { Constructor(1, Vec::new()) } }),
                DoubleGE => primitive_float(stack, |l, r| { if l >= r { Constructor(0, Vec::new()) } else { Constructor(1, Vec::new()) } }),
                IntToDouble => {
                    let top = stack.pop().unwrap();
                    stack.push(match *top.borrow() {
                        Int(i) => Node::new(Float(i as f64)),
                        _ => panic!("Excpected Int in Int -> Double cast")
                    });
                }
                DoubleToInt => {
                    let top = stack.pop().unwrap();
                    stack.push(match *top.borrow() {
                        Float(f) => Node::new(Int(f as isize)),
                        _ => panic!("Excpected Double in Double -> Int cast")
                    });
                }
                PushInt(value) => { stack.push(Node::new(Int(value))); }
                PushFloat(value) => { stack.push(Node::new(Float(value))); }
                PushChar(value) => { stack.push(Node::new(Char(value))); }
                Push(index) => {
                    let x = stack[index].clone();
                    debug!("Pushed {:?}", *x.borrow());
                    for j in 0..stack.len() {
                        debug!(" {:?}  {:?}", j, *stack[j].borrow());
                    }
                    stack.push(x);
                }
                PushGlobal(index) => {
                    let (assembly_index, index) = self.globals[index];
                    let sc = &self.assembly[assembly_index].super_combinators[index];
                    stack.push(Node::new(Combinator(sc)));
                }
                PushBuiltin(index) => {
                    let (arity, f) = get_builtin(index);
                    stack.push(Node::new(BuiltinFunction(arity, f)));
                }
                Mkap => {
                    assert!(stack.len() >= 2);
                    let func = stack.pop().unwrap();
                    let arg = stack.pop().unwrap();
                    debug!("Mkap {:?} {:?}", *func.borrow(), *arg.borrow());
                    stack.push(Node::new(Application(func, arg)));
                }
                Eval => {
                    static UNWINDCODE : &'static [Instruction] = &[Unwind];
                    let old = stack.pop().unwrap();
                    let mut new_stack = vec!(old.clone());
                    self.execute(&mut new_stack, UNWINDCODE, assembly_id);
                    stack.push(new_stack.pop().unwrap());
                    debug!("{:?}", stack);
                    let new = stack.last().unwrap().borrow().clone();
                    *(*old.node).borrow_mut() = new;
                    debug!("{:?}", stack);
                }
                Pop(num) => {
                    for _ in 0..num {
                        stack.pop();
                    }
                }
                Update(index) => {
                    stack[index] = Node::new(Indirection(stack.last().unwrap().clone()));
                }
                Unwind => {
                    fn unwind<'a, F>(i_ptr: &mut Wrapping<usize>, arity: usize, stack: &mut Vec<Node<'a>>, f: F)
                        where F: FnOnce(&mut Vec<Node<'a>>) -> Node<'a> {
                        if stack.len() - 1 < arity {
                            while stack.len() > 1 {
                                stack.pop();
                            }
                        }
                        else {
                            for j in (stack.len() - arity - 1)..(stack.len() - 1) {
                                let temp = match *stack[j].borrow() {
                                    Application(_, ref arg) => arg.clone(),
                                    _ => panic!("Expected Application")
                                };
                                stack[j] = temp;
                            }
                            let value = {
                                let mut new_stack = Vec::new();
                                for i in 0..arity {
                                    let index = stack.len() - i - 2;
                                    new_stack.push(stack[index].clone());
                                }
                                f(&mut new_stack)
                            };
                            for _ in 0..(arity + 1) {
                                stack.pop();
                            }
                            stack.push(value);
                            *i_ptr = *i_ptr - Wrapping(1);
                        }
                    }
                    let x = (*stack.last().unwrap().borrow()).clone();
                    debug!("Unwinding {:?}", x);
                    match x {
                        Application(func, _) => {
                            stack.push(func);
                            i = i - Wrapping(1);//Redo the unwind instruction
                        }
                        Combinator(comb) => {
                            debug!(">>> Call {:?}", comb.name);
                            unwind(&mut i, comb.arity, stack, |new_stack| {
                                self.execute(new_stack, &*comb.instructions, comb.assembly_id);
                                new_stack.pop().unwrap()
                            });
                        }
                        BuiltinFunction(arity, func) => {
                            unwind(&mut i, arity, stack, |new_stack| func(self, new_stack.as_ref()));
                        }
                        Indirection(node) => {
                            *stack.last_mut().unwrap() = node;
                            i = i - Wrapping(1);//Redo the unwind instruction
                        }
                        _ => ()
                    }
                }
                Slide(size) => {
                    let top = stack.pop().unwrap();
                    for _ in 0..size {
                        stack.pop();
                    }
                    stack.push(top);
                }
                Split(_) => {
                    let temp = stack.pop().unwrap();
                    let temp = temp.borrow();
                    match *temp {
                        Constructor(_, ref fields) => {
                            for field in fields.iter() {
                                stack.push(field.clone());
                            }
                        }
                        _ => panic!("Expected constructor in Split instruction")
                    }
                }
                Pack(tag, arity) => {
                    let mut args = Vec::new();
                    for _ in 0..arity {
                        args.push(stack.pop().unwrap());
                    }
                    stack.push(Node::new(Constructor(tag, args)));
                }
                JumpFalse(address) => {
                    match *stack.last().unwrap().borrow() {
                        Constructor(0, _) => (),
                        Constructor(1, _) => i = Wrapping(address - 1),
                        _ => ()
                    }
                    stack.pop();
                }
                CaseJump(jump_tag) => {
                    let jumped = match *stack.last().unwrap().borrow() {
                        Constructor(tag, _) => {
                            if jump_tag == tag as usize {
                                i = i + Wrapping(1);//Skip the jump instruction ie continue to the next test
                                true
                            }
                            else {
                                false
                            }
                        }
                        ref x => panic!("Expected constructor when executing CaseJump, got {:?}", *x),
                    };
                    if !jumped {
                        stack.pop();
                    }
                }
                Jump(to) => {
                    i = Wrapping(to - 1);
                }
                PushDictionary(index) => {
                    let assembly = &self.assembly[assembly_id];
                    let dict : &[usize] = &*assembly.instance_dictionaries[index];
                    let dict = InstanceDictionary { entries: dict.iter().map(|i| Rc::new(DictionaryEntry::Function(*i))).collect() };
                    stack.push(Node::new(Dictionary(dict)));
                }
                PushDictionaryMember(index) => {
                    let sc = {
                        let x = stack[0].borrow();
                        let dict = match *x {
                            Dictionary(ref x) => x,
                            ref x => panic!("Attempted to retrieve {:?} as dictionary", *x)
                        };
                        match *dict.entries[index] {
                            DictionaryEntry::Function(gi) => {
                                let (assembly_index, i) = self.globals[gi];
                                Combinator(&self.assembly[assembly_index].super_combinators[i])
                            }
                            DictionaryEntry::App(gi, ref dict) => {
                                let (assembly_index, i) = self.globals[gi];
                                let sc = &self.assembly[assembly_index].super_combinators[i];
                                Application(Node::new(Combinator(sc)), Node::new(Dictionary(dict.clone())))
                            }
                        }
                    };
                    stack.push(Node::new(sc));
                }
                MkapDictionary => {
                    let a = stack.pop().unwrap();
                    let a = a.borrow();
                    let arg = match *a {
                        Dictionary(ref d) => {
                            d
                        }
                        _ => panic!()
                    };
                    let func = stack.pop().unwrap();
                    let mut new_dict = InstanceDictionary { entries: Vec::new() };
                    match *func.borrow() {
                        Dictionary(ref d) => {
                            for entry in d.entries.iter() {
                                match **entry {
                                    DictionaryEntry::Function(index) => {
                                        new_dict.entries.push(Rc::new(DictionaryEntry::App(index, arg.clone())));
                                    }
                                    _ => panic!()
                                }
                            }
                        }
                        _ => panic!()
                    }
                    stack.push(Node::new(Dictionary(new_dict)));
                }
                ConstructDictionary(size) => {
                    let mut new_dict = InstanceDictionary { entries: Vec::new() };
                    for _ in 0..size {
                        let temp = stack.pop().unwrap();
                        let temp = temp.borrow();
                        match *temp {
                            Dictionary(ref d) => {
                                new_dict.entries.extend(d.entries.iter().map(|x| x.clone()));
                            }
                            ref x => panic!("Unexpected {:?}", x)
                        }
                    }
                    stack.push(Node::new(Dictionary(new_dict)));
                }
                PushDictionaryRange(start, size) => {
                    let mut new_dict = InstanceDictionary { entries: Vec::new() };
                    match *stack[0].borrow() {
                        Dictionary(ref d) => {
                            new_dict.entries.extend(d.entries.iter().skip(start).take(size).map(|x| x.clone()));
                        }
                        _ => panic!()
                    }
                    stack.push(Node::new(Dictionary(new_dict)));
                }
            }
            i = i + Wrapping(1);
        }
        debug!("End frame");
        debug!("--------------------------");
    }
}


///Exucutes a binary primitive instruction taking two integers
fn primitive_int<'a, F>(stack: &mut Vec<Node<'a>>, f: F) where F: FnOnce(isize, isize) -> Node_<'a> {
    let l = stack.pop().unwrap();
    let r = stack.pop().unwrap();
    let l = l.borrow();
    let r = r.borrow();
    match (&*l, &*r) {
        (&Int(lhs), &Int(rhs)) => stack.push(Node::new(f(lhs, rhs))),
        (lhs, rhs) => panic!("Expected fully evaluted numbers in primitive instruction\n LHS: {:?}\nRHS: {:?} ", lhs, rhs)
    }
}
///Exucutes a binary primitive instruction taking two doubles
fn primitive_float<'a, F>(stack: &mut Vec<Node<'a>>, f: F) where F: FnOnce(f64, f64) -> Node_<'a> {
    let l = stack.pop().unwrap();
    let r = stack.pop().unwrap();
    let l = l.borrow();
    let r = r.borrow();
    match (&*l, &*r) {
        (&Float(lhs), &Float(rhs)) => stack.push(Node::new(f(lhs, rhs))),
        (lhs, rhs) => panic!("Expected fully evaluted numbers in primitive instruction\n LHS: {:?}\nRHS: {:?} ", lhs, rhs)
    }
}
fn primitive<F>(stack: &mut Vec<Node>, f: F) where F: FnOnce(isize, isize) -> isize {
    primitive_int(stack, move |l, r| Int(f(l, r)))
}

#[derive(PartialEq, Debug)]
pub enum VMResult {
    Int(isize),
    Double(f64),
    Constructor(u16, Vec<VMResult>)
}

macro_rules! vm_error {
    ($($pre: ident :: $post: ident),+) => {

    #[derive(Debug)]
    pub enum VMError {
        Io(io::Error),
        $($post(::$pre::$post)),+
    }

    impl fmt::Display for VMError {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match *self {
                VMError::Io(ref e) => write!(f, "{}", e),
                $(VMError::$post(ref e) => write!(f, "{}", e)),+
            }
        }
    }

    impl Error for VMError {
        fn description(&self) -> &str {
            match *self {
                VMError::Io(ref e) => e.description(),
                $(VMError::$post(ref e) => e.description()),+
            }
        }
    }

    impl From<io::Error> for VMError {
        fn from(e: io::Error) -> Self { VMError::Io(e) }
    }

    $(impl From<::$pre::$post> for VMError {
        fn from(e: ::$pre::$post) -> Self { VMError::$post(e) }
    })+
    }
}
vm_error! { parser::ParseError, renamer::RenamerError, typecheck::TypeError }

fn compile_iter<T : Iterator<Item=char>>(iterator: T) -> Result<Assembly, VMError> {
    let mut parser = Parser::new(iterator);
    let module = try!(parser.module());
    let mut module = try!(rename_module(module));

    let mut typer = TypeEnvironment::new();
    try!(typer.typecheck_module(&mut module));
    let core_module = do_lambda_lift(translate_module(module));

    let mut compiler = Compiler::new();
    Ok(compiler.compile_module(&core_module))
}

///Compiles a single file
pub fn compile_file(filename: &str) -> Result<Assembly, VMError> {
    let path = &Path::new(filename);
    let mut file = try!(File::open(path));
    let mut contents = ::std::string::String::new();
    try!(file.read_to_string(&mut contents));
    compile_iter(contents.chars())
}

fn extract_result(node: Node_) -> Option<VMResult> {
    match node {
        Constructor(tag, fields) => {
            let mut result = Vec::new();
            for field in fields.iter() {
                match extract_result((*field.borrow()).clone()) {
                    Some(x) => result.push(x),
                    None => return None
                }
            }
            Some(VMResult::Constructor(tag, result))
        }
        Int(i) => Some(VMResult::Int(i)),
        Float(i) => Some(VMResult::Double(i)),
        x => {
            println!("Can't extract result {:?}", x);
            None
        }
    }
}

pub fn execute_main_string(module: &str) -> Result<Option<VMResult>, String> {
    let assemblies = try!(compile_string(module));
    execute_main_module_(assemblies)
}

///Takes a module with a main function and compiles it and all its imported modules
///and then executes the main function
pub fn execute_main_module(modulename: &str) -> Result<Option<VMResult>, String> {
    let assemblies = try!(compile_module(modulename));
    execute_main_module_(assemblies)
}

fn execute_main_module_(assemblies: Vec<Assembly>) -> Result<Option<VMResult>, String> {
    let mut vm = VM::new();
    for assembly in assemblies.into_iter() {
        vm.add_assembly(assembly);
    }
    let x = vm.assembly.iter().flat_map(|a| a.super_combinators.iter()).find(|sc| sc.name.name == intern("main"));
    match x {
        Some(sc) => {
            assert!(sc.arity == 0);
            let result = vm.evaluate(&*sc.instructions, sc.assembly_id);
            Ok(extract_result(result))
        }
        None => Ok(None)
    }
}

//We mirror the naming scheme from Haskell here which is camelCase
#[allow(non_snake_case)]
mod primitive {

    use std::io::Read;
    use std::fs::File;
    use vm::{VM, Node, Node_};
    use vm::Node_::{Application, Constructor, BuiltinFunction, Char};
    use compiler::Instruction;
    use compiler::Instruction::Eval;

    pub fn get_builtin(i: usize) -> (usize, BuiltinFun) {
        match i {
            0 => (1, error),
            1 => (2, seq),
            2 => (2, readFile),
            3 => (3, io_bind),
            4 => (2, io_return),
            5 => (2, putStrLn),
            6 => (2, compare_tags),
            _ => panic!("undefined primitive")
        }
    }

    pub type BuiltinFun = for <'a> extern "Rust" fn (&'a VM, &[Node<'a>]) -> Node<'a>;

    fn error<'a>(vm: &'a VM, stack: &[Node<'a>]) -> Node<'a> {
        let mut vec = Vec::new();
        vec.push(stack[0].clone());
        let node = vm.deepseq(vec, 123);
        panic!("error: {:?}", node)
    }
    fn eval<'a>(vm: &'a VM, node: Node<'a>) -> Node<'a> {
        static EVALCODE : &'static [Instruction] = &[Eval];
        let mut temp = Vec::new();
        temp.push(node);
        vm.execute(&mut temp, EVALCODE, 123);
        temp.pop().unwrap()
    }
    fn seq<'a>(vm: &'a VM, stack: &[Node<'a>]) -> Node<'a> {
        eval(vm, stack[0].clone());
        stack[1].clone()
    }
    fn io_bind<'a>(_vm: &'a VM, stack: &[Node<'a>]) -> Node<'a> {
        //IO a -> (a -> IO b) -> IO b
        //IO a = (RealWorld -> (a, RealWorld)
        //((RealWorld -> (a, RealWorld)) -> (a -> RealWorld -> (b, RealWorld)) -> RealWorld -> (b, RealWorld)
        //             0                                      1                        2
        //(a, RealWorld)
        let aw = Node::new(Application(stack[0].clone(), stack[2].clone()));
        let p = Node::new(BuiltinFunction(2, pass));
        Node::new(Application(Node::new(Application(p, aw)), stack[1].clone()))
    }
    fn pass<'a>(vm: &'a VM, stack: &[Node<'a>]) -> Node<'a> {
        //(a, RealWorld) -> (a -> RealWorld -> (b, RealWorld)) -> (b, RealWorld)
        eval(vm, stack[0].clone());
        let aw = stack[0].borrow();
        let (a, rw) = match *aw {
            Constructor(_, ref args) => (&args[0], &args[1]),
            _ => panic!("pass exepected constructor")
        };
        Node::new(Application(Node::new(Application(stack[1].clone(), a.clone())), rw.clone()))
    }
    fn io_return<'a>(_vm: &'a VM, stack: &[Node<'a>]) -> Node<'a> {
        //a -> RealWorld -> (a, RealWorld)
        Node::new(Constructor(0, vec!(stack[0].clone(), stack[1].clone())))
    }
    fn readFile<'a>(vm: &'a VM, stack: &[Node<'a>]) -> Node<'a> {
        let mut temp = Vec::new();
        temp.push(stack[0].clone());
        let node_filename = vm.deepseq(temp, 123);
        let filename = get_string(&node_filename);
        let mut file = match File::open(&filename) {
            Ok(f) => f,
            Err(err) => panic!("error: readFile -> {:?}", err)
        };
        let mut s = ::std::string::String::new();
        let (begin, _end) = match file.read_to_string(&mut s) {
            Ok(_) => create_string(&s),
            Err(err) => panic!("error: readFile -> {:?}", err)
        };
        //Return (String, RealWorld)
        Node::new(Constructor(0, vec!(begin, stack[1].clone())))
    }

    fn putStrLn<'a>(vm: &'a VM, stack: &[Node<'a>]) -> Node<'a> {
        let mut temp = Vec::new();
        temp.push(stack[0].clone());
        let msg_node = vm.deepseq(temp, 123);
        let msg = get_string(&msg_node);
        println!("{:?}", msg);
        Node::new(Constructor(0, vec!(Node::new(Constructor(0, vec!())), stack[1].clone())))
    }
    fn get_string<'a>(node: &Node_<'a>) -> String {
        fn get_string_<'a>(buffer: &mut String, node: &Node_<'a>) {
            match *node {
                Constructor(_, ref args) => {
                    if args.len() == 2 {
                        match *args[0].borrow() {
                            Char(c) => buffer.push(c),
                            _ => panic!("Unevaluated char")
                        }
                        get_string_(buffer, &*args[1].borrow());
                    }
                }
                _ => panic!("Unevaluated list")
            }
        }
        let mut buffer = String::new();
        get_string_(&mut buffer, node);
        buffer
    }
    fn compare_tags<'a>(vm: &'a VM, stack: &[Node<'a>]) -> Node<'a> {
        let mut temp = Vec::new();
        temp.push(stack[0].clone());
        let node_a = vm.deepseq(temp, 123);
        temp.push(stack[1].clone());
        let node_b = vm.deepseq(temp, 123);
        let a = get_string(&node_a);
        let b = get_string(&node_b);
        if a == b {
            Node::new(Constructor(0, vec!()))
        } else {
            Node::new(Constructor(1, vec!()))
        }
    }
 */





    fn create_string<'a>(s: &str) -> (Node<'a>, Node<'a>) {
        let mut node = Node::new(Constructor(0, vec!()));
        let first = node.clone();
        for c in s.chars() {
            let temp = match *node.borrow_mut() {
                Constructor(ref mut tag, ref mut args) => {
                    *tag = 1;
                    args.push(Node::new(Char(c)));
                    args.push(Node::new(Constructor(0, Vec::new())));
                    args[1].clone()
                }
                _ => panic!()
            };
            node = temp;
        }
        (first, node)
    }
    ///Compares the tags of two constructors, returning an Ordering
    fn compare_tags<'a>(vm: &'a VM, stack: &[Node<'a>]) -> Node<'a> {
        use std::cmp::Ordering;
        assert_eq!(stack.len(), 2);
        let lhs = eval(vm, stack[0].clone());
        let rhs = eval(vm, stack[1].clone());
        let tag = match (&*lhs.borrow(), &*rhs.borrow()) {
            (&Constructor(lhs, _), &Constructor(rhs, _)) => match lhs.cmp(&rhs) {
                Ordering::Less => 0,
                Ordering::Equal => 1,
                Ordering::Greater => 2
            },
            (_, _) => 1//EQ
        };
        Node::new(Constructor(tag, Vec::new()))
    }
}

#[cfg(test)]
mod tests {

use typecheck::TypeEnvironment;
use compiler::{compile_with_type_env};
use vm::{VM, compile_file, compile_iter, execute_main_module, execute_main_string, extract_result, VMResult};
use vm::VMResult::{Int, Double, Constructor};
use interner::*;

fn execute_main<T : Iterator<Item=char>>(iterator: T) -> Option<VMResult> {
    let mut vm = VM::new();
    vm.add_assembly(compile_iter(iterator).unwrap());
    let x = vm.assembly.iter().flat_map(|a| a.super_combinators.iter()).find(|sc| sc.name.name == intern("main"));
    match x {
        Some(sc) => {
            assert!(sc.arity == 0);
            let result = vm.evaluate(&*sc.instructions, sc.assembly_id);
            extract_result(result)
        }
        None => None
    }
}

#[test]
fn test_primitive()
{
    assert_eq!(execute_main("main = primIntAdd 10 5".chars()), Some(VMResult::Int(15)));
    assert_eq!(execute_main("main = primIntSubtract 7 (primIntMultiply 2 3)".chars()), Some(VMResult::Int(1)));
    assert_eq!(execute_main("main = primIntDivide 10 (primIntRemainder 6 4)".chars()), Some(VMResult::Int(5)));
    assert_eq!(execute_main("main = primDoubleDivide 3. 2.".chars()), Some(VMResult::Double(1.5)));
    let s =
r"data Bool = True | False
main = primIntLT 1 2";
    assert_eq!(execute_main(s.chars()), Some(VMResult::Constructor(0, Vec::new())));
}

#[test]
fn test_function()
{
    let module =
r"mult2 x = primIntMultiply x 2
main = mult2 10";
    assert_eq!(execute_main(module.chars()), Some(VMResult::Int(20)));

    let module2 =
r"mult2 x = primIntMultiply x 2
add x y = primIntAdd y x
main = add 3 (mult2 10)";
    assert_eq!(execute_main(module2.chars()), Some(VMResult::Int(23)));
}
#[test]
fn test_case()
{
    let module =
r"mult2 x = primIntMultiply x 2
main = case [mult2 123, 0] of
    x:xs -> x
    [] -> 10";
    assert_eq!(execute_main(module.chars()), Some(VMResult::Int(246)));
}

#[test]
fn test_nested_case() {
    let module =
r"mult2 x = primIntMultiply x 2
main = case [mult2 123, 0] of
    246:xs -> primIntAdd 0 246
    [] -> 10";
    assert_eq!(execute_main(module.chars()), Some(VMResult::Int(246)));
}

#[test]
fn test_nested_case2() {
    let module =
r"mult2 x = primIntMultiply x 2
main = case [mult2 123, 0] of
    246:[] -> primIntAdd 0 246
    x:xs -> 20
    [] -> 10";
    assert_eq!(execute_main(module.chars()), Some(VMResult::Int(20)));
}
#[test]
fn local_function() {
    let module =
r"main =
    let
        f x y =
            let
                g x = primIntAdd x y
            in g (primIntAdd 1 x)
    in f (primIntAdd 2 0) (primIntAdd 3 0)";
    assert_eq!(execute_main(module.chars()), Some(VMResult::Int(6)));
}

#[test]
fn test_data_types()
{
    let module =
r"data Bool = True | False
test = False
main = case test of
    False -> primIntAdd 0 0
    True -> primIntAdd 1 0";
    assert_eq!(execute_main(module.chars()), Some(VMResult::Int(0)));
}

#[test]
fn test_typeclasses_known_types()
{
    let module =
r"data Bool = True | False
class Test a where
    test :: a -> Int
instance Test Int where
    test x = x
instance Test Bool where
    test x = case x of
        True -> 1
        False -> 0
main = primIntSubtract (test (primIntAdd 5 0)) (test True)";
    assert_eq!(execute_main(module.chars()), Some(VMResult::Int(4)));
}

#[test]
fn test_typeclasses_unknown()
{
    let module =
r"data Bool = True | False
class Test a where
    test :: a -> Int
instance Test Int where
    test x = x
instance Test Bool where
    test x = case x of
        True -> 1
        False -> 0
testAdd y = primIntAdd (test (primIntAdd 5 0)) (test y)
main = testAdd True";
    assert_eq!(execute_main(module.chars()), Some(VMResult::Int(6)));
}

#[test]
fn test_run_prelude() {
    let prelude = compile_file("Prelude.hs").unwrap();
    let assembly = {
        let mut type_env = TypeEnvironment::new();

        compile_with_type_env(&mut type_env, &[&prelude],
r"add x y = primIntAdd x y
main = foldl add 0 [1,2,3,4]").unwrap()
    };

    let mut vm = VM::new();
    vm.add_assembly(prelude);
    vm.add_assembly(assembly);
    let x = vm.assembly.iter().flat_map(|a| a.super_combinators.iter()).find(|sc| sc.name.name == intern("main"));
    let result = match x {
        Some(sc) => {
            assert!(sc.arity == 0);
            let result = vm.evaluate(&*sc.instructions, sc.assembly_id);
            extract_result(result)
        }
        None => None
    };
    assert_eq!(result, Some(VMResult::Int(10)));
}

#[test]
fn instance_super_class() {
    let prelude = compile_file("Prelude.hs").unwrap();

    let assembly = {
        let mut type_env = TypeEnvironment::new();
        compile_with_type_env(&mut type_env, &[&prelude], "main = [primIntAdd 0 1,2,3,4] == [1,2,3]").unwrap()
    };

    let mut vm = VM::new();
    vm.add_assembly(prelude);
    vm.add_assembly(assembly);
    let x = vm.assembly.iter().flat_map(|a| a.super_combinators.iter()).find(|sc| sc.name.name == intern("main"));
    let result = match x {
        Some(sc) => {
            assert!(sc.arity == 0);
            let result = vm.evaluate(&*sc.instructions, sc.assembly_id);
            extract_result(result)
        }
        None => None
    };
    assert_eq!(result, Some(VMResult::Constructor(1, Vec::new())));
}

#[test]
fn monad_do() {
    let prelude = compile_file("Prelude.hs").unwrap();

    let assembly = {
        let mut type_env = TypeEnvironment::new();
        compile_with_type_env(&mut type_env, &[&prelude],
"
test :: Maybe Int -> Maybe Int -> Maybe Int
test x y = do
    x1 <- x
    y
    return (x1 + 1)
main = test (Just 4) (Just 6)").unwrap()
    };

    let mut vm = VM::new();
    vm.add_assembly(prelude);
    vm.add_assembly(assembly);
    let x = vm.assembly.iter().flat_map(|a| a.super_combinators.iter()).find(|sc| sc.name.name == intern("main"));
    let result = match x {
        Some(sc) => {
            assert!(sc.arity == 0);
            let result = vm.evaluate(&*sc.instructions, sc.assembly_id);
            extract_result(result)
        }
        None => None
    };
    assert_eq!(result, Some(VMResult::Constructor(0, vec!(VMResult::Int(5)))));
}

#[test]
fn import() {
    let result = execute_main_module("Test");
    assert_eq!(result, Ok(Some(VMResult::Int(6))));
}

#[test]
fn pattern_bind() {
    let result = execute_main_string(
r"
import Prelude
test :: [Bool] -> Bool
test (True:[]) = False
test (True:y:ys) = y
test [] = False
main = test [True, True]
")
    .unwrap_or_else(|err| panic!(err));
    assert_eq!(result, Some(VMResult::Constructor(0, Vec::new())));
}
#[test]
fn pattern_guards() {
    let result = execute_main_string(
r"
import Prelude
test :: Int -> [a] -> Int
test 2 _ = 2
test x []
    | primIntLT x 0 = primIntSubtract 0 1
    | primIntGT x 0 = 1
test x _ = x
main = (test 2 [], test 100 [], test 100 ['c'])
")
    .unwrap_or_else(|err| panic!(err));
    assert_eq!(result, Some(VMResult::Constructor(0, vec!(VMResult::Int(2), VMResult::Int(1), VMResult::Int(100)))));
}

#[test]
fn pattern_guards_nested() {
    let result = execute_main_string(
r"
import Prelude
test :: Int -> [Int] -> Int
test 2 _ = 2
test x (0:[])
    | primIntLT x 0 = primIntSubtract 0 1
    | primIntGT x 0 = 1
test x _ = x
main = (test 2 [], test 100 [0], test 100 [0, 123])
")
    .unwrap_or_else(|err| panic!(err));
    assert_eq!(result, Some(VMResult::Constructor(0, vec!(VMResult::Int(2), VMResult::Int(1), VMResult::Int(100)))));
}
#[test]
fn test_class_default_function()
{
    let module =
r"data Bool = True | False
class Test a where
    test :: a -> Int
    test _ = 42
    test2 :: Int
instance Test Int where
    test x = x
    test2 = 0
instance Test Bool where
    test2 = 2
main = (test True, test (1 :: Int))";
    assert_eq!(execute_main(module.chars()), Some(VMResult::Constructor(0, vec![VMResult::Int(42), VMResult::Int(1)])));
}

#[test]
fn use_super_class() {
    let result = execute_main_string(
r"
import Prelude
test x y = (x == y) || (x < y)
main = (test (0 :: Int) 2) && not (test (1 :: Int) 0)")
        .unwrap_or_else(|err| panic!("{:?}", err));
    assert_eq!(result, Some(VMResult::Constructor(0, Vec::new())));
}
#[test]
fn implement_class() {
    let result = execute_main_string(
r"
import Prelude
data AB = A | B
instance Eq AB where
    (==) A A = True
    (==) B B = True
    (==) _ _ = False
test x y = x == y
main = A == B && test A A")
        .unwrap_or_else(|err| panic!("{:?}", err));
    assert_eq!(result, Some(VMResult::Constructor(1, Vec::new())));
}

#[test]
fn deriving_eq() {
    let result = execute_main_string(
r"
import Prelude
data Test = A Int | B
    deriving(Eq)
main = A 0 == A 2 || A 0 == B
").unwrap_or_else(|err| panic!(err));
    assert_eq!(result, Some(VMResult::Constructor(1, Vec::new())));
}
#[test]
fn deriving_ord() {
    let result = execute_main_string(
r"
import Prelude
data Test = A Int | B
    deriving(Eq, Ord)
main = compare (A 0) (A 2) == LT && compare B (A 123) == GT
").unwrap_or_else(|err| panic!(err));
    assert_eq!(result, Some(VMResult::Constructor(0, Vec::new())));
}

#[test]
fn instance_eq_list() {
    let result = execute_main_string(
r"
import Prelude
test x y = x == y
main = test [1 :: Int] [3]
").unwrap_or_else(|err| panic!(err));
    assert_eq!(result, Some(VMResult::Constructor(1, Vec::new())));
}
#[test]
fn build_dictionary() {
    //Test that the compiler can generate code to build a dictionary at runtime
    let result = execute_main_string(
r"
import Prelude
test :: Eq a => a -> a -> Bool
test x y = [x] == [y]
main = test [1 :: Int] [3]
").unwrap_or_else(|err| panic!(err));
    assert_eq!(result, Some(VMResult::Constructor(1, Vec::new())));
}

#[test]
fn if_else() {
    let result = execute_main_string(
r"
import Prelude
main = let
        x = 123 :: Int
    in if x < 0
        then x
        else 1
").unwrap_or_else(|err| panic!(err));
    assert_eq!(result, Some(VMResult::Int(1)));
}

#[test]
fn newtype() {
    let result = execute_main_string(
r"
import Prelude
newtype Even = Even Int
makeEven :: Int -> Maybe Even
makeEven i
    | i `div` 2 /= (i - 1) `div` 2 = Just (Even i)
    | otherwise = Nothing
main = makeEven (100 * 3)
").unwrap_or_else(|err| panic!(err));

    assert_eq!(result, Some(VMResult::Constructor(0, vec![VMResult::Int(300)])));
}

#[test]
fn where_bindings() {
    let result = execute_main_string(
r"
import Prelude
main = case list of
        [] -> 123
        x:xs
            | y < 10 -> 0
            | otherwise -> y
            where
            y = x + 10
    where
        list = [1::Int]
").unwrap_or_else(|err| panic!(err));
    assert_eq!(result, Some(VMResult::Int(11)));
}

}
 */


#[test]
fn test_class_default_function_with_type_parameter()
{
    let module = test_module(file!());
    let result = execute_main(module.chars());
    assert!(result.is_ok());
}




//Let's optimize the code above as if it were compilaed with a Memristive

pub struct Module<SolitonID> {
    pub classes: Vec<Class<SolitonID>>,
    pub data_definitions: Vec<DataDefinition<Name>>,
    pub newtypes: Vec<Newtype<Name>>,
    pub instances: Vec<Instance<SolitonID>>,
    pub bindings: Vec<Binding<SolitonID>>
}

impl Module<Id> {
    pub fn from_expr(expr: Expr<Id>) -> Module<Id> {
        Module {
            classes: vec![],
            data_definitions: vec![],
            newtypes: Vec::new(),
            instances: vec![],
            bindings: vec![Binding {
                name: Id::new(Name { name: intern("main"), uid: 0 }, expr.get_type().clone(), vec![]),
                expression: expr
            }]
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Class<SolitonID> {
    pub constraints: Vec<Constraint<Name>>,
    pub name : Name,
    pub variable : TypeVariable,
    pub declarations : Vec<module::TypeDeclaration<Name>>,
    pub bindings: Vec<Binding<SolitonID>>
}




#[derive(Clone, Debug, PartialEq)]
pub enum Constraint<Name> {
    Defined(Name),
    Instance(Name, Vec<Type<Name>>)
}



#


#[derive(Clone, Debug, PartialEq)]
pub struct DataDefinition<Name> {
    pub name : Name,
    pub constructors : Vec<Constructor<Name>>,
    pub bindings: Vec<Binding<SolitonID>>
}


#[derive(Clone, Debug)]
pub struct Instance<SolitonID = InlineHeapHasOIDStr> {
    pub bindings : Vec<Binding<SolitonID>>,
    pub constraints : Vec<Constraint<Name>>,
    pub typ : TcType,
    pub classname : Name
}

#[derive(Clone, Debug, PartialEq)]
pub struct Binding<SolitonID> {
    pub name: SolitonID,
    pub expression: Expr<SolitonID>
}

#[derive(Clone, Debug, PartialEq)]
pub struct Alternative<SolitonID> {
    pub pattern : Pattern<SolitonID>,
    pub expression : Expr<SolitonID>
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern<SolitonID> {
    Constructor(SolitonID, Vec<SolitonID>),
    SolitonIDifier(SolitonID),
    Number(isize),
    WildCard
}

#[derive(Clone, Debug, PartialEq)]
pub struct LiteralData {
    pub typ: TcType,
    pub value: Literal_
}

pub type Literal_ = module::LiteralData;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<SolitonID> {
    SolitonIDifier(SolitonID),
    Apply(Box<Expr<SolitonID>>, Box<Expr<SolitonID>>),
    Literal(LiteralData),
    Lambda(SolitonID, Box<Expr<SolitonID>>),
    Let(Vec<Binding<SolitonID>>, Box<Expr<SolitonID>>),
    Case(Box<Expr<SolitonID>>, Vec<Alternative<SolitonID>>)
}

impl fmt::Display for LiteralData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl <T: fmt::Display> fmt::Display for Binding<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = {}", self.name, self.expression)
    }
}

impl <T: fmt::Display> fmt::Display for Expr<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Expr::*;
        write_core_expr!(*self, f,)
    }
}
impl <T: fmt::Display> fmt::Display for Alternative<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} -> {}", self.pattern, self.expression)
    }
}
impl <T: fmt::Display> fmt::Display for Pattern<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Pattern::SolitonIDifier(ref s) => write!(f, "{}", s),
            Pattern::Number(ref i) => write!(f, "{}", i),
            Pattern::Constructor(ref name, ref patterns) => {
                try!(write!(f, "({} ", name));
                for p in patterns.iter() {
                    try!(write!(f, " {}", p));
                }
                write!(f, ")")
            }
            Pattern::WildCard => write!(f, "_")
        }
    }
}

///Trait which provides access to the Type of any struct which implements it.
pub trait Typed {
    type Id;
    fn get_type<'a>(&'a self) -> &'a Type<Self::Id>;
}

impl <SolitonID: Typed<Id=Name>> Typed for Expr<SolitonID> {
    type Id = Name;
    fn get_type<'a>(&'a self) -> &'a Type<Name> {
        match self {
            &Expr::SolitonIDifier(ref i) => i.get_type(),
            &Expr::Literal(ref lit) => &lit.typ,
            &Expr::Apply(ref func, _) => {
                match func.get_type() {
                    &Type::Application(_, ref a) => { &**a }
                    x => panic!("The function in Apply must be a type application, found {}", x)
                }
            }
            &Expr::Lambda(ref arg, _) => arg.get_type(),
            &Expr::Let(_, ref body) => body.get_type(),
            &Expr::Case(_, ref alts) => alts[0].expression.get_type()
        }
    }
}
impl <SolitonID: Typed> Typed for Pattern<SolitonID> {
    type Id = SolitonID::Id;
    fn get_type<'a>(&'a self) -> &'a Type<SolitonID::Id> {
        match *self {
            Pattern::SolitonIDifier(ref name) => name.get_type(),
            Pattern::Constructor(ref name, _) => name.get_type(),
            Pattern::Number(_) => panic!(),
            Pattern::WildCard => panic!()
        }
    }
}

impl PartialEq<str> for Name {
    fn eq(&self, o: &str) -> bool {
        self.name == intern(o)
    }
}

///Id is a Name combined with a type
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Id<T = Name> {
    pub name: T,
    pub typ: Qualified<TcType, Name>
}

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl <T> Id<T> {
    pub fn new(name: T, typ: TcType, constraints: Vec<Constraint<Name>>) -> Id<T> {
        Id { name: name, typ: module::qualified(constraints, typ) }
    }
}

impl AsRef<str> for Id {
    fn as_ref(&self) -> &str {
        self.name.name.as_ref()
    }
}



//------------ Module ----------------------------------------------------

pub type Module<SolitonID> = module::Module<SolitonID>;

impl <T> Typed for Id<T> {
    type Id = Name;
    fn get_type(&self) -> &Type<Name> {
        &self.typ.value
    }

    fn get_constraints(&self) -> &[Constraint<Name>] {
        &self.typ.constraints
    }
    }

impl <T> Typed for Module<T> {
    type Id = T;
    fn get_type(&self) -> &Type<T> {
        &self.typ.value as &Type<T>
        for b in self.bindings.iter() {
            if b.name == self.name {
                return b.typ.get_type();
            }
        }

        if (let_
                | PatKind
                | MethodArg
                | Constructor
                | MethodMut
                | MethodCall
                | MethodDef<>
            | MethodInstance,
            ) = (&self.bindings[0], &self.bindings[1])  <
            (false, false) => true, _ => false
        }
    }
}

    ),
    |Expr::Lambda(ref arg, ref body)| {
        let arg_type = arg.get_type();
        let body_type = body.get_type();
        let mut new_body = Vec::new();
        for param in arg.params.iter() {
            let param_type = param.get_type();

            if !param_type.subtype_of(arg_type) {
                panic!("The parameter {} has type {} but the argument has type {}", param.name, param_type, arg_type);
            }


            match param_type.subtype_of(body_type) {
                Some(ty) => {
for param in arg.params.iter() {
if !param_type.subtype_of(arg_type) {
switch param_type {
case &Type::Application(ref f, ref a) => {
if !f.subtype_of(arg_type) {
                    new_body.push(a.clone());
}
                } else {
                    new_body.push(param.clone());
}
                }
}
            }

}

return new_body;

            }

            return &self.typ.value;
        }
        panic!("No binding for {}", self.name);
        }
    panic!("No module for {}", self.name);
    }
}

//------------ Constraint -----------------------------------------------------------



pub mod ref_ {
    use super::*;
    use super::Expr::*;
    ///Visitor for the types in the core language.
    ///visit_ is called at the every value in the tree, if it is overriden
    ///the appropriate walk_ methods need to be called to continue walking
    pub trait Visitor<SolitonID> : Sized {
        fn visit_expr(&mut self, expr: &Expr<SolitonID>) {
            walk_expr(self, expr)
        }
        fn visit_alternative(&mut self, alt: &Alternative<SolitonID>) {
            walk_alternative(self, alt)
        }
        fn visit_pattern(&mut self, _pattern: &Pattern<SolitonID>) {
        }
        fn visit_binding(&mut self, binding: &Binding<SolitonID>) {
            walk_binding(self, binding);
        }
        fn visit_module(&mut self, module: &Module<SolitonID>) {
            walk_module(self, module);
        }
    }

    pub fn walk_module<V: Visitor<SolitonID>, SolitonID>(visitor: &mut V, module: &Module<SolitonID>) {
        for bind in module.bindings.iter() {
            visitor.visit_binding(bind);
        }
    }

    pub fn walk_binding<V: Visitor<SolitonID>, SolitonID>(visitor: &mut V, binding: &Binding<SolitonID>) {
        visitor.visit_expr(&binding.expression);
    }

    pub fn walk_expr<V: Visitor<SolitonID>, SolitonID>(visitor: &mut V, expr: &Expr<SolitonID>) {
        match expr {
            &Apply(ref func, ref arg) => {
                visitor.visit_expr(&**func);
                visitor.visit_expr(&**arg);
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
            _ => ()
        }
    }

    pub fn walk_alternative<V: Visitor<SolitonID>, SolitonID>(visitor: &mut V, alt: &Alternative<SolitonID>) {
        visitor.visit_expr(&alt.expression);
    }
}

pub mod mutable {
    use super::*;
    use super::Expr::*;
    
    pub trait Visitor<SolitonID>: Sized {
        fn visit_expr(&mut self, expr: &mut Expr<SolitonID>) {
            walk_expr(self, expr)
        }
        fn visit_alternative(&mut self, alt: &mut Alternative<SolitonID>) {
            walk_alternative(self, alt)
        }
        fn visit_pattern(&mut self, _pattern: &mut Pattern<SolitonID>) {
        }
        fn visit_binding(&mut self, binding: &mut Binding<SolitonID>) {
            walk_binding(self, binding);
        }
        fn visit_module(&mut self, module: &mut Module<SolitonID>) {
            walk_module(self, module);
        }
    }

    pub fn walk_module<SolitonID, V: Visitor<SolitonID>>(visitor: &mut V, module: &mut Module<SolitonID>) {
        for bind in module.bindings.iter_mut() {
            visitor.visit_binding(bind);
        }
    }

    pub fn walk_binding<SolitonID, V: Visitor<SolitonID>>(visitor: &mut V, binding: &mut Binding<SolitonID>) {
        visitor.visit_expr(&mut binding.expression);
    }

    pub fn walk_expr<SolitonID, V: Visitor<SolitonID>>(visitor: &mut V, expr: &mut Expr<SolitonID>) {
        match expr {
            &mut Apply(ref mut func, ref mut arg) => {
                visitor.visit_expr(&mut **func);
                visitor.visit_expr(&mut **arg);
            }
            &mut Lambda(_, ref mut body) => visitor.visit_expr(&mut **body),
            &mut Let(ref mut binds, ref mut e) => {
                for b in binds.iter_mut() {
                    visitor.visit_binding(b);
                }
                visitor.visit_expr(&mut **e);
            }
            &mut Case(ref mut e, ref mut alts) => {
                visitor.visit_expr(&mut **e);
                for alt in alts.iter_mut() {
                    visitor.visit_alternative(alt);
                }
            }
            _ => ()
        }
    }

    pub fn walk_alternative<SolitonID, V: Visitor<SolitonID>>(visitor: &mut V, alt: &mut Alternative<SolitonID>) {
        visitor.visit_expr(&mut alt.expression);
    }
}

pub mod result {
    use super::*;
    use super::Expr::*;

    ///A visitor which takes the structs as values and in turn expects a value in return
    ///so that it can rebuild the tree
    pub trait Visitor<SolitonID> : Sized {
        fn visit_expr(&mut self, expr: Expr<SolitonID>) -> Expr<SolitonID> {
            walk_expr(self, expr)
        }
        fn visit_alternative(&mut self, alt: Alternative<SolitonID>) -> Alternative<SolitonID> {
            walk_alternative(self, alt)
        }
        fn visit_pattern(&mut self, pattern: Pattern<SolitonID>) -> Pattern<SolitonID> {
            pattern
        }
        fn visit_binding(&mut self, binding: Binding<SolitonID>) -> Binding<SolitonID> {
            walk_binding(self, binding)
        }
        fn visit_module(&mut self, module: Module<SolitonID>) -> Module<SolitonID> {
            walk_module(self, module)
        }
    }

    pub fn walk_module<V: Visitor<SolitonID>, SolitonID>(visitor: &mut V, mut module: Module<SolitonID>) -> Module<SolitonID> {
        let mut bindings = vec![];
        ::std::mem::swap(&mut module.bindings, &mut bindings);
        module.bindings = bindings.into_iter()
            .map(|bind| visitor.visit_binding(bind))
            .collect();
        module
    }

    pub fn walk_binding<V: Visitor<SolitonID>, SolitonID>(visitor: &mut V, binding: Binding<SolitonID>) -> Binding<SolitonID> {
        let Binding { name, expression } = binding;
        Binding {
            name: name,
            expression: visitor.visit_expr(expression)
        }
    }

    pub fn walk_expr<V: Visitor<SolitonID>, SolitonID>(visitor: &mut V, expr: Expr<SolitonID>) -> Expr<SolitonID> {
        match expr {
            Apply(func, arg) => {
                let f = visitor.visit_expr(*func);
                let a = visitor.visit_expr(*arg);
                Apply(box f, box a)
            }
            Lambda(x, body) => Lambda(x, box visitor.visit_expr(*body)),
            Let(binds, e) => {
                let bs: Vec<Binding<SolitonID>> = binds.into_iter().map(|b| {
                    visitor.visit_binding(b)
                }).collect();
                Let(bs, box visitor.visit_expr(*e))
            }
            Case(e, alts) => {
                let e2 = visitor.visit_expr(*e);
                let alts2: Vec<Alternative<SolitonID>> = alts.into_iter()
                    .map(|alt| visitor.visit_alternative(alt))
                    .collect();
                Case(box e2, alts2)
            }
            expr => expr
        }
    }

    pub fn walk_alternative<V: Visitor<SolitonID>, SolitonID>(visitor: &mut V, alt: Alternative<SolitonID>) -> Alternative<SolitonID> {
        let Alternative { pattern, expression } = alt;
        Alternative { pattern: visitor.visit_pattern(pattern), expression: visitor.visit_expr(expression) }
    }
}

///The translate module takes the AST and translates it into the simpler core language.
pub mod translate {
    use module;
    use core::*;
    use core::Expr::*;
    use typecheck::TcType;
    use inlineHeapHasOID::*;
    use renamer::NameSupply;
    use renamer::typ::*;
    use deriving::*;
    use std::collections::HashMap;

    struct Translator<'a> {
        name_supply: NameSupply,
        functions_in_class: &'a mut (FnMut(Name) -> (&'a TypeVariable, &'a [TypeDeclaration<Name>]) + 'a)
    }
    
    #[derive(Debug)]
    struct Equation<'a>(&'a [(Id<Name>, Pattern<Id<Name>>)], (&'a [Binding<Id<Name>>], &'a module::Match<Name>));

    pub fn translate_expr(expr: module::TypedExpr<Name>) -> Expr<Id<Name>> {
        let mut translator = Translator { name_supply: NameSupply::new(), functions_in_class: &mut |_| panic!() };
        translator.translate_expr(expr)
    }

    pub fn translate_modules(modules: Vec<module::Module<Name>>) -> Vec<Module<Id<Name>>> {
        let mut map = HashMap::new();
        for class in modules.iter().flat_map(|m| m.classes.iter()) {
            map.insert(class.name.clone(), (class.variable.clone(), class.declarations.clone()));
        }
        let mut translator = Translator {
            name_supply: NameSupply::new(),
            functions_in_class: &mut |name| {
                let &(ref var, ref decls) = map.get(&name).unwrap();
                (var, decls.as_ref())
            }
        };
        modules.into_iter()
            .map(|module| translate_module_(&mut translator, module))
            .collect()
    }
    pub fn translate_module(module: module::Module<Name>) -> Module<Id<Name>> {
        translate_modules(vec!(module)).pop().unwrap()
    }
    fn translate_module_<'a>(translator: &mut Translator<'a>, module: module::Module<Name>) -> Module<Id<Name>> {
        let module::Module { name : _name,
            imports : _imports,
            bindings,
            type_declarations : _type_declarations,
            newtypes,
            classes,
            instances,
            data_definitions,
            fixity_declarations : _fixity_declarations
        } = module;

        let mut new_instances: Vec<Instance<Id<Name>>> = Vec::new();

        let classes2 : Vec<Class<Id>> = classes.into_iter().map(|class| {
            let module::Class {
                constraints,
                name,
                variable,
                declarations,
                bindings
            } = class;
            Class {
                constraints: constraints,
                name: name,
                variable: variable,
                declarations: declarations,
                bindings: translator.translate_bindings(bindings)
            }
        }).collect();

        for instance in instances.into_iter() {
            let module::Instance {
                classname,
                typ,
                constraints,
                bindings
            } = instance;
            let bs: Vec<Binding<Id<Name>>> = translator.translate_bindings(bindings).into_iter().collect();
            new_instances.push(Instance {
                constraints: constraints,
                typ: typ,
                classname: classname,
                bindings: bs
            });
        }
        let bs: Vec<Binding<Id<Name>>> = translator.translate_bindings(bindings).into_iter().collect();
        for data in data_definitions.iter() {
            generate_deriving(&mut new_instances, data);
        }
        for instance in new_instances.iter_mut() {
            let (class_var, class_decls) = (translator.functions_in_class)(instance.classname);
            let defaults = create_default_stubs(class_var, class_decls, instance);
            let mut temp = Vec::new();
            ::std::mem::swap(&mut temp, &mut instance.bindings);
            let vec: Vec<Binding<Id<Name>>> = temp.into_iter().chain(defaults.into_iter()).collect();
            instance.bindings = vec;
        }
        Module {
            classes: classes2,
            data_definitions: data_definitions,
            newtypes: newtypes,
            bindings: bs,
            instances: new_instances
        }
    }

    ///Creates stub functions for each undeclared function in the instance
    fn create_default_stubs(class_var: &TypeVariable, class_decls: &[TypeDeclaration<Name>], instance: &Instance<Id<Name>>) -> Vec<Binding<Id<Name>>> {
        class_decls.iter()
            .filter(|decl| instance.bindings.iter().find(|bind| bind.name.as_ref().ends_with(decl.name.as_ref())).is_none())
            .map(|decl| {
                debug!("Create default function for {} ({}) {}", instance.classname, instance.typ, decl.name);
                //The stub functions will naturally have the same type as the function in the class but with the variable replaced
                //with the instance's type
                let mut typ = decl.typ.clone();
                ::typecheck::replace_var(&mut typ.value, class_var, &instance.typ);
                {
                    let context = ::std::mem::replace(&mut typ.constraints, Vec::new());
                    //Remove all constraints which refer to the class's variable
                    let vec_context: Vec<Constraint<Name>> = context.into_iter()
                        .filter(|c| c.variables[0] != *class_var)
                        .collect();
                    typ.constraints = vec_context;
                }
                let Qualified { value: typ, constraints } = typ;
                let default_name = module::encode_binding_SolitonIDifier(instance.classname.name, decl.name.name);
                let typ_name = module::extract_applied_type(&instance.typ).ctor().name.name;
                let instance_fn_name = module::encode_binding_SolitonIDifier(typ_name, decl.name.name);

                //Example stub for undeclared (/=)
                //(/=) = #Eq/=
                Binding {
                    name: Id::new(Name { name: instance_fn_name, uid: decl.name.uid }, typ.clone(), constraints.clone()),
                    expression: SolitonIDifier(Id::new(Name { name: default_name, uid: decl.name.uid }, typ, constraints))
                }
            })
            .collect()
    }
impl <'a> Translator<'a> {
    fn translate_match(&mut self, matches: module::Match<Name>) -> Expr<Id<Name>> {
        match matches {
            module::Match::Simple(e) => self.translate_expr(e),
            module::Match::Guards(ref gs) => self.translate_guards(unmatched_guard(), &**gs)
        }
    }

    pub fn translate_expr(&mut self, input_expr: module::TypedExpr<Name>) -> Expr<Id<Name>> {
        //Checks if the expression is lambda not bound by a let binding
        //if it is then we wrap the lambda in a let binding
        let is_lambda = match &input_expr.expr {
            &module::Expr::Lambda(_, _) => true,
            _ => false
        };
        if is_lambda {
            let module::TypedExpr { typ, expr, ..} = input_expr;
            match expr {
                module::Expr::Lambda(arg, body) => {
                    //TODO need to make unique names for the lambdas created here
                    let argname = match arg {
                        module::Pattern::SolitonIDifier(arg) => arg,
                        module::Pattern::WildCard => Name { name: intern("_"), uid: usize::max_value() },
                        _ => panic!("Core translation of pattern matches in lambdas are not implemented")
                    };
                    let l = Lambda(Id::new(argname, typ.clone(), vec![]), box self.translate_expr_rest(*body));
                    let id = Id::new(self.name_supply.from_str("#lambda"), typ.clone(), vec![]);
                    let bind = Binding { name: id.clone(), expression: l };
                    Let(vec![bind], box SolitonIDifier(id))
                }
                _ => panic!()
            }
        }
        else {
            self.translate_expr_rest(input_expr)
        }
    }

    fn translate_expr_rest(&mut self, input_expr: module::TypedExpr<Name>) -> Expr<Id<Name>> {
        let module::TypedExpr { typ, expr, ..} = input_expr;
        match expr {
            module::Expr::SolitonIDifier(s) => SolitonIDifier(Id::new(s, typ, vec![])),
            module::Expr::Apply(func, arg) => Apply(box self.translate_expr(*func), box self.translate_expr(*arg)),
            module::Expr::OpApply(lhs, op, rhs) => {
                let l = box self.translate_expr(*lhs);
                let r = box self.translate_expr(*rhs);
                let func_type = function_type_(l.get_type().clone(),
                                function_type_(r.get_type().clone(),
                                               typ));
                Apply(box Apply(box SolitonIDifier(Id::new(op, func_type, vec![])), l), r)
            }
            module::Expr::Literal(l) => Literal(LiteralData { typ: typ, value: l }),
            module::Expr::Lambda(arg, body) => {
                match arg {
                    module::Pattern::SolitonIDifier(arg) => Lambda(Id::new(arg, typ, vec![]), box self.translate_expr_rest(*body)),
                    module::Pattern::WildCard => Lambda(Id::new(Name { name: intern("_"), uid: usize::max_value() }, typ, vec![]), box self.translate_expr_rest(*body)),
                    _ => panic!("Core translation of pattern matches in lambdas are not implemented")
                }
            }
            module::Expr::Let(bindings, body) => {
                let bs = self.translate_bindings(bindings);
                Let(bs, box self.translate_expr(*body))
            }
            module::Expr::Case(expr, alts) => {
                self.translate_case(*expr, alts)
            }
            module::Expr::IfElse(pred, if_true, if_false) => {
                Case(box self.translate_expr(*pred), vec![
                    Alternative { pattern: bool_pattern("True"), expression: self.translate_expr(*if_true) },
                    Alternative { pattern: bool_pattern("False"), expression: self.translate_expr(*if_false) }
                    ])
            }
            module::Expr::Do(bindings, expr) => {
                let mut result = self.translate_expr(*expr);
                for bind in bindings.into_iter().rev() {
                    result = match bind {
                        module::DoBinding::DoExpr(e) => {
                            let core = self.translate_expr(e);
                            let x = self.do_bind2_id(core.get_type().clone(), result.get_type().clone());
                            Apply(box Apply(box x, box core), box result)
                        }
                        module::DoBinding::DoBind(pattern, e) => {
                            let e2 = self.translate_expr(e);
                            self.do_bind_translate(pattern.node, e2, result)
                        }
                        module::DoBinding::DoLet(bs) => {
                            Let(self.translate_bindings(bs), box result)
                        }
                    };
                }
                result
            }
            module::Expr::TypeSig(expr, _) => self.translate_expr(*expr),
            module::Expr::Paren(expr) => self.translate_expr(*expr)
        }
    }
    ///Translates
    ///do { expr; stmts } = expr >> do { stmts; }
    fn do_bind2_id(&mut self, m_a: TcType, m_b: TcType) -> Expr<Id<Name>> {
        debug!("m_a {}", m_a);
        let c = match *m_a.appl() {
            Type::Variable(ref var) => vec![Constraint { class: Name { name: intern("Monad"), uid: 0 }, variables: vec![var.clone()] }],
            _ => vec![]
        };
        let typ = function_type_(m_a, function_type_(m_b.clone(), m_b));
        SolitonIDifier(Id::new(Name { name: intern(">>"), uid: 0}, typ, c))
    } 
    ///Translates
    ///do {p <- e; stmts} 	=
    ///    let ok p = do {stmts}
	///        ok _ = fail "..."
	///    in e >>= ok
    fn do_bind_translate(&mut self, pattern: module::Pattern<Name>, expr: Expr<Id<Name>>, result: Expr<Id<Name>>) -> Expr<Id<Name>> {

        let m_a = expr.get_type().clone();
        let a = m_a.appr().clone();
        let m_b = result.get_type().clone();
                debug!("m_a {}", m_a);
        let c = match *m_a.appl() {
            Type::Variable(ref var) => vec![Constraint { class: Name { name: intern("Monad"), uid: 0 }, variables: vec![var.clone()] }],
            _ => vec![]
        };
        let arg2_type = function_type_(a.clone(), m_b.clone());
        let bind_typ = function_type_(m_a, function_type_(arg2_type.clone(), m_b.clone()));
        let bind_SolitonID = SolitonIDifier(Id::new(Name { name: intern(">>="), uid: 0}, bind_typ, c.clone()));

        //Create ok binding
        let func_SolitonID = Id::new(
            self.name_supply.from_str("#ok"),
            arg2_type.clone(),
            c.clone()
        );//TODO unique id
        let var = Id::new(self.name_supply.from_str("p"), function_type_(a, m_b.clone()), c.clone());//Constraints for a
        let fail_SolitonID = SolitonIDifier(Id::new(Name { name: intern("fail"), uid: 0 }, function_type_(list_type(char_type()), m_b), c));
        let func = Lambda(var.clone(), box Case(box SolitonIDifier(var),
            vec![Alternative { pattern: self.translate_pattern(pattern), expression: result }
            , Alternative { pattern: Pattern::WildCard, expression: Apply(box fail_SolitonID, box string("Unmatched pattern in let")) } ]));
        let bind = Binding { name: func_SolitonID.clone(), expression: func };
        
        Let(vec![bind], box apply(bind_SolitonID, (vec![expr, SolitonIDifier(func_SolitonID)]).into_iter()))
    }

    fn translate_bindings(&mut self, bindings: Vec<module::Binding<Name>>) -> Vec<Binding<Id<Name>>> {
        let mut result = Vec::new();
        let mut vec: Vec<module::Binding<Name>> = Vec::new();
        for bind in bindings.into_iter() {
            if vec.len() > 0 && vec[0].name != bind.name {
                result.push(self.translate_matching_groups(vec));
                vec = Vec::new();
            }
            vec.push(bind);
        }
        if vec.len() > 0 {
            result.push(self.translate_matching_groups(vec));
        }
        result
    }
    
    fn unwrap_pattern(&mut self, uid: usize, id: Id<Name>, pattern: module::Pattern<Name>, result: &mut Vec<(Id<Name>, Pattern<Id<Name>>)>) {
        match pattern {
            module::Pattern::Constructor(ctor_name, mut patterns) => {
                let index = result.len();
                let mut name = id.name.name.to_string();
                let base_length = name.len();
                result.push((id, Pattern::Number(0)));//Dummy
                for (i, p) in patterns.iter_mut().enumerate() {
                    let x = match *p {
                        module::Pattern::Constructor(..) | module::Pattern::Number(..) => {
                            //HACK, by making the variable have the same uid as
                            //the index the newly generated pattern will be recognized
                            //as the same since their binding variable are the same
                            name.truncate(base_length);
                            name.push('_');
                            name.push_str(&*i.to_string());

                            let n = Name { name: intern(name.as_ref()), uid: uid };
                            Some(module::Pattern::SolitonIDifier(n))
                        }
                        _ => None
                    };
                    match x {
                        Some(mut x) => {
                            ::std::mem::swap(p, &mut x);
                            let id = match *p {
                                module::Pattern::SolitonIDifier(ref n) => Id::new(n.clone(), Type::new_var(intern("a")), vec![]),
                                _ => panic!()
                            };
                            self.unwrap_pattern(uid, id, x, result);
                        }
                        None => ()
                    }
                }
                result[index].1 = self.translate_pattern(module::Pattern::Constructor(ctor_name, patterns));
            }
            _ => result.push((id, self.translate_pattern(pattern)))
        }
    }
    ///Translates a pattern list of patterns into a list of patterns which are not nested.
    ///The first argument of each tuple is the SolitonIDifier that is expected to be passed to the case.
    fn unwrap_patterns(&mut self, uid: usize, arg_ids: &[Id<Name>], arguments: &[module::Pattern<Name>]) -> Vec<(Id<Name>, Pattern<Id<Name>>)> {
        let mut result = Vec::new();
        for (p, id) in arguments.iter().zip(arg_ids.iter()) {
            self.unwrap_pattern(uid, id.clone(), p.clone(), &mut result);
        }
        result
    }

    ///Translates a case expression into the core language.
    ///Since the core language do not have nested patterns the patterns are unwrapped into
    ///multiple case expressions.
    fn translate_case(&mut self, expr: module::TypedExpr<Name>, alts: Vec<module::Alternative<Name>>) -> Expr<Id<Name>> {
        let mut vec = Vec::new();
        let dummy_var = &[Id::new(self.name_supply.anonymous(), Type::new_var(intern("a")), vec![])];
        let uid = self.name_supply.next_id();
        for module::Alternative { pattern, matches, where_bindings } in alts.into_iter() {
            let bindings = where_bindings.map_or(Vec::new(), |bs| self.translate_bindings(bs));
            vec.push((self.unwrap_patterns(uid, dummy_var, &[pattern.node]), bindings, matches));
        }
        let mut x = self.translate_equations_(vec);
        match x {
            Case(ref mut body, _) => {
                **body = self.translate_expr(expr);
            }
            _ => panic!("Not case")
        }
        x
    }
    ///Translates a binding group such as
    ///map f (x:xs) = e1
    ///map f [] = e2
    fn translate_matching_groups(&mut self, mut bindings: Vec<module::Binding<Name>>) -> Binding<Id<Name>> {
        //If the binding group is simple (no patterns and only one binding)
        //then we do a simple translation to preserve the names for the arguments.
        if bindings.len() == 1 && simple_binding(&bindings[0]) {
            let module::Binding {
                name,
                arguments,
                matches,
                typ: module::Qualified { constraints, value: typ, },
                where_bindings
            } = bindings.pop().unwrap();
            let arg_iterator = arguments.into_iter().map(|p| {
                match p {
                    module::Pattern::SolitonIDifier(n) => n,
                    module::Pattern::WildCard => Name { name: intern("_"), uid: usize::max_value() },
                    _ => panic!("simple_binding fail")
                }
            });
            let expr = {
                let lambda_ids = lambda_iterator(&typ)
                    .zip(arg_iterator)
                    .map(|(typ, arg)| {
                    Id::new(arg, typ.clone(), vec![])
                });
                let where_bindings_binds = where_bindings.map_or(Vec::new(), |bs| self.translate_bindings(bs).into_iter().collect());
                make_lambda(lambda_ids, make_let(where_bindings_binds, self.translate_match(matches)))
            };
            return Binding {
                name: Id::new(name, typ, constraints),
                expression: expr
            }
        }
        //Generate new names for each of the arguments (since it is likely that not all arguments have a name)
        let mut arg_ids = Vec::new();
        let name;
        {
            let binding0 = &bindings[0];
            name = Id::new(binding0.name.clone(), binding0.typ.value.clone(), binding0.typ.constraints.clone());
            let mut typ = &binding0.typ.value;
            for _ in 0..binding0.arguments.len() {
                arg_ids.push(Id::new(self.name_supply.from_str("arg"), typ.clone(), vec![]));
                typ = match *typ {
                    Type::Application(_, ref next) => &**next,
                    _ => typ//We dont actually have a function type which we need, so we are likely in a unittest
                            //just reuse the same type so we do not crash
                };
            }
        }
        //First we flatten all the patterns that occur in each equation
        //(2:xs) -> [(x:xs), 2]
        let uid = self.name_supply.next_id();
        let equations: Vec<_> = bindings.into_iter().map(|bind| {
            let module::Binding {
                arguments,
                matches,
                where_bindings,
                ..
            } = bind;
            let where_bindings_binds = where_bindings.map_or(Vec::new(), |bs| self.translate_bindings(bs));
            (self.unwrap_patterns(uid, arg_ids.as_ref(), &*arguments), where_bindings_binds, matches)
        }).collect();
        let mut expr = self.translate_equations_(equations);
        expr = make_lambda(arg_ids.into_iter(), expr);
        debug!("Desugared {} :: {}\n {}", name.name, name.typ, expr);
        Binding {
            name: name,
            expression: expr
        }
    }
    fn translate_equations_(&mut self, equations: Vec<(Vec<(Id<Name>, Pattern<Id<Name>>)>, Vec<Binding<Id<Name>>>, module::Match<Name>)>) -> Expr<Id<Name>> {
        let mut eqs: Vec<Equation> = Vec::new();
        for &(ref ps, ref bs, ref e) in equations.iter() {
            eqs.push(Equation(ps.as_ref(), (bs.as_ref(), e)));
        }
        for e in eqs.iter() {
            debug!("{:?}", e);
        }
        self.translate_equations(eqs.as_ref())
    }

    ///Translates a list of guards, if no guards matches then the result argument will be the result
    fn translate_guards(&mut self, mut result: Expr<Id<Name>>, guards: &[module::Guard<Name>]) -> Expr<Id<Name>> {
        for guard in guards.iter().rev() {
            let predicate = box self.translate_expr(guard.predicate.clone());
            result = Case(predicate, vec![
                Alternative { pattern: bool_pattern("True"), expression: self.translate_expr(guard.expression.clone()) },
                Alternative { pattern: bool_pattern("False"), expression: result },
            ]);
        }
        result
    }

    fn translate_equations(&mut self, equations: &[Equation]) -> Expr<Id<Name>> {
        ///Returns true if the two patterns would match for the same values
        fn matching<T: PartialEq>(lhs: &(T, Pattern<T>), rhs: &(T, Pattern<T>)) -> bool {
            if lhs.0 != rhs.0 {
                return false;
            }
            match (&lhs.1, &rhs.1) {
                (&Pattern::Constructor(ref l, _), &Pattern::Constructor(ref r, _)) => *l == *r,
                (&Pattern::Constructor(..), &Pattern::Number(..)) => false,
                (&Pattern::Number(..), &Pattern::Constructor(..)) => false,
                _ => true
            }
        }
        debug!("In {:?}", equations);
        let &Equation(ps, (where_bindings_bindings, e)) = &equations[0];
        if ps.len() == 0 {
            assert_eq!(equations.len(), 1);//Otherwise multiple matches for this group
            let bindings = where_bindings_bindings.iter().map(|x| x.clone()).collect();
            return make_let(bindings, self.translate_match((*e).clone()));
        }
        if ps.len() == 1 {
            let mut alts: Vec<Alternative<Id<Name>>> = Vec::new();
            for (i, &Equation(ps, (where_bindings_bindings, m))) in equations.iter().enumerate() {
                let bindings = where_bindings_bindings.iter().map(|x| x.clone()).collect();
                match *m {
                    module::Match::Simple(ref e) => {
                        let alt = if ps.len() == 0 {
                            Alternative {
                                pattern: Pattern::WildCard, expression:
                                make_let(bindings, self.translate_expr((*e).clone()))
                            }
                        }
                        else {
                            Alternative {
                                pattern: ps[0].1.clone(),
                                expression: make_let(bindings, self.translate_expr((*e).clone()))
                            }
                        };
                        alts.push(alt);
                    }
                    module::Match::Guards(ref guards) => {
                        let fallthrough = if equations.len() == i + 1 {
                            unmatched_guard()
                        }
                        else {
                            self.translate_equations(&equations[i + 1..])
                        };
                        alts.push(Alternative {
                            pattern: ps[0].1.clone(),
                            expression: make_let(bindings, self.translate_guards(fallthrough, &**guards))
                        });
                    }
                }
            }
            let body = box SolitonIDifier(ps[0].0.clone());
            return Case(body, alts);
        }
        
        let mut last_index = 0;
        let mut vec: Vec<Equation> = Vec::new();
        let mut alts: Vec<Alternative<Id<Name>>> = Vec::new();
        let mut visited = Vec::new();
        loop {
            //Find the first pattern which does a test and is not already used
            let mut pattern_test = None;
            while last_index < equations.len() {
                let &Equation(ps, _) = &equations[last_index];
                if ps.len() > 0  {
                    match ps[0].1 {
                        Pattern::Constructor(..) | Pattern::Number(..) => {
                            if visited.iter().find(|x| matching(**x, &ps[0])).is_none() {
                                pattern_test = Some(&ps[0]);
                                visited.push(&ps[0]);
                                last_index += 1;
                                break;
                            }
                        }
                        _ => ()
                    }
                }
                last_index += 1;
            }
            match pattern_test {
                Some(pattern_test) => {
                    vec.clear();
                    let mut variable_bindings = Vec::new();
                    //Gather all patterns which matches the pattern
                    for &Equation(patterns, expr) in equations.iter() {
                        if patterns.len() > 0 && matching(pattern_test, &patterns[0]) {
                            vec.push(Equation(&patterns[1..], expr));
                            //If the patter_test is a constructor we need to add the variables
                            //of the other patterns in a let binding to make sure that all names exist
                            match (&patterns[0].1, &pattern_test.1) {
                                (&Pattern::Constructor(_, ref l_vars), &Pattern::Constructor(_, ref r_vars)) => {
                                    for (l_var, r_var) in l_vars.iter().zip(r_vars.iter()) {
                                        if l_var != r_var {
                                            variable_bindings.push(Binding { name: l_var.clone(), expression: SolitonIDifier(r_var.clone()) });
                                        }
                                    }
                                }
                                _ => ()
                            }
                        }
                        else if patterns.len() == 0 {
                            vec.push(Equation(patterns, expr));
                        }
                    }
                    //For all the pattern that match the pattern we need to generate new case expressions
                    let e = make_let(variable_bindings, self.translate_equations(vec.as_ref()));

                    let arg_id = &ps[0].0;
                    let bs = needed_variables(arg_id, equations);
                    alts.push(Alternative {
                        pattern: pattern_test.1.clone(),
                        expression: make_let(bs, e)
                    });
                }
                None => break
            }
        }
        if alts.len() == 0 {
            for &Equation(patterns, expr) in equations.iter() {
                vec.push(Equation(&patterns[1..], expr));
            }
            let &Equation(ps, _) = &equations[0];
            let arg_id = &ps[0].0;
            let bs = needed_variables(arg_id, equations);
            make_let(bs, self.translate_equations(vec.as_ref()))
        }
        else {
            let defaults: Vec<Equation> = equations.iter()
                .filter(|& &Equation(ps, _)| ps.len() > 0 && (match ps[0].1 { Pattern::WildCard | Pattern::SolitonIDifier(..) => true, _ => false }))
                .map(|&Equation(ps, e)| Equation(&ps[1..], e))
                .collect();
            if defaults.len() != 0 {
                let arg_id = &ps[0].0;
                let bs = needed_variables(arg_id, equations);
                let e = make_let(bs, self.translate_equations(defaults.as_ref()));
                alts.push(Alternative {
                    pattern: Pattern::WildCard,
                    expression: e
                });
            }
            let &Equation(ps, _) = &equations[0];
            let body = box SolitonIDifier(ps[0].0.clone());
            Case(body, alts)
        }
    }

    fn translate_pattern(&mut self, pattern: module::Pattern<Name>) -> Pattern<Id<Name>> {
        match pattern {
            module::Pattern::SolitonIDifier(i) => Pattern::SolitonIDifier(Id::new(i, Type::new_var(intern("a")), vec![])),
            module::Pattern::Number(n) => Pattern::Number(n),
            module::Pattern::Constructor(name, patterns) => {
                let ps = patterns.into_iter().map(|pat| {
                    match pat {
                        module::Pattern::SolitonIDifier(name) => Id::new(name, Type::new_var(intern("a")), vec![]),
                        module::Pattern::WildCard => Id::new(Name { name: intern("_"), uid: usize::max_value() }, Type::new_var(intern("a")), vec![]),
                        _ => panic!("Nested pattern")
                    }
                }).collect();
                Pattern::Constructor(Id::new(name, Type::new_var(intern("a")), vec![]), ps)
            }
            module::Pattern::WildCard => Pattern::WildCard
        }
    }
}

    fn bool_pattern(s: &str) -> Pattern<Id<Name>> {
        Pattern::Constructor(Id::new(Name { name: intern(s), uid: 0 }, bool_type(), vec![]), vec![])
    }

    struct LambdaIterator<'a, Id: 'a> {
        typ: &'a Type<Id>
    }
    impl <'a, Id: AsRef<str>> Iterator for LambdaIterator<'a, Id> {
        type Item = &'a Type<Id>;
        fn next(&mut self) -> Option<&'a Type<Id>> {
            match *self.typ {
                Type::Application(ref lhs, ref rhs) => {
                    match **lhs {
                        Type::Application(ref func, _) => {
                            match **func {
                                Type::Constructor(ref op) if op.name.as_ref() == "->" => {
                                    let func = self.typ;
                                    self.typ = &**rhs;
                                    Some(func)
                                }
                                _ => None
                            }
                        }
                        _ => None
                    }
                }
                _ => None
            }
        }
    }
    //Creates an iterator which walks through all the function types that are needed
    //when creating a lambda with make_lambda
    //Ex: (a -> b -> c) generates [(a -> b -> c), (b -> c)]
    fn lambda_iterator<'a, Id: AsRef<str>>(typ: &'a Type<Id>) -> LambdaIterator<'a, Id> {
        LambdaIterator { typ: typ }
    }
    ///Tests that the binding has no patterns for its arguments
    fn simple_binding(binding: &module::Binding<Name>) -> bool {
        binding.arguments.iter().all(|arg| {
            match *arg {
                module::Pattern::WildCard | module::Pattern::SolitonIDifier(..) => true,
                _ => false
            }
        })
    }

    ///Creates a lambda from an iterator of its arguments and body
    fn make_lambda<T, I: Iterator<Item=T>>(mut iter: I, body: Expr<T>) -> Expr<T> {
        match iter.next() {
            Some(arg) => Lambda(arg, box make_lambda(iter, body)),
            None => body
        }
    }
    ///Creates a function application from a function and its arguments
    fn apply<T, I: Iterator<Item=Expr<T>>>(mut func: Expr<T>, iter: I) -> Expr<T> {
        for arg in iter {
            func = Apply(box func, box arg);
        }
        func
    }
    ///Creates a let binding, but if there is no bindings the let is omitted
    fn make_let<T>(bindings: Vec<Binding<T>>, expr: Expr<T>) -> Expr<T> {
        if bindings.len() == 0 {
            expr
        }
        else {
            Let(bindings, box expr)
        }
    }

    ///Takes a id of the variable passed to the case and returns a vector
    ///of bindings which need to be added to make sure no variables are missing
    fn needed_variables(arg_id: &Id<Name>, equations: &[Equation]) -> Vec<Binding<Id<Name>>> {
        equations.iter()
            .filter(|& &Equation(ps, _)| ps.len() > 0 && (match ps[0].1 { Pattern::WildCard | Pattern::SolitonIDifier(..) => true, _ => false }))
            .map(|eq| {
            let &Equation(ps, _) = eq;
            let other_id = match ps[0].1 {
                Pattern::SolitonIDifier(ref name) => name.clone(),
                Pattern::WildCard => Id::new(Name { name: intern("_"), uid: usize::max_value() }, Type::new_var(intern("a")), vec![]),
                _ => panic!()
            };
            Binding { name: other_id, expression: SolitonIDifier(arg_id.clone()) }
        }).collect()
    }
    ///Creates a string literal expressions from a &str
    fn string(s: &str) -> Expr<Id<Name>> {
        Literal(LiteralData { typ: list_type(char_type()), value: String(intern(s)) })
    }
    ///Creates an expression which reports an unmatched guard error when executed
    fn unmatched_guard() -> Expr<Id<Name>> {
        let error_SolitonID = SolitonIDifier(Id::new(Name { name: intern("error"), uid: 0 }, function_type_(list_type(char_type()), Type::new_var(intern("a"))), vec![]));
        Apply(box error_SolitonID, box string("Unmatched guard"))
    }

}

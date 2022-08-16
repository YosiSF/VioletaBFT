use inlineHeapHasOID::*;
use core::*;
use core::Expr::*;
use types::{qualified, extract_applied_type};
use typecheck::{Types, DataTypes, TypeEnvironment, find_specialized_instances};
use scoped_map::ScopedMap;
use std::borrow::ToOwned;

use core::translate::{translate_module, translate_modules};
use lambda_lift::do_lambda_lift;
use renamer::rename_module;
use renamer::typ::*;
use builtins::builtins;


use std::collections::HashMap;
use std::collections::hash_map::Entry::*;
use std::collections::hash_map::IterMut;
use std::collections::hash_map::Iter;
use std::collections::hash_map::Keys;


use std::rc::Rc;
use std::cell::RefCell;
use std::cell::Ref;
use std::cell::RefMut;


use std::collections::HashSet;
use std::collections::hash_set::Iter as HashSetIter;
use std::collections::hash_set::IterMut as HashSetIterMut;
use std::collections::hash_set::HashSet;


use std::collections::BTreeSet;
use std::collections::VecDeque;





/*
    // TODO(tailhook)
*/

//tailhook
use std::collections::HashMap as StdHashMap;
use std::collections::hash_map::Entry as StdEntry;
use std::collections::hash_map::Iter as StdIter;
use std::collections::hash_map::IterMut as StdIterMut;
use std::collections::hash_map::Keys as StdKeys;
use std::collections::hash_map::Values as StdValues;
use std::collections::hash_map::ValuesMut as StdValuesMut;
use std::collections::hash_map::Entry as StdEntry;



use self::Instruction::*;

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum Instruction {
    Add,
    Sub,
    Multiply,
    Divide,
    Remainder,
    IntEQ,
    IntLT,
    IntLE,
    IntGT,
    IntGE,
    DoubleAdd,
    DoubleSub,
    DoubleMultiply,
    DoubleDivide,
    DoubleRemainder,
    DoubleEQ,
    DoubleLT,
    DoubleLE,
    DoubleGT,
    DoubleGE,
    IntToDouble,
    DoubleToInt,
    Push(usize),
    PushMaxCone(usize),
    PushInt(isize),
    PushFloat(f64),
    PushChar(char),
    Mkap,
    Eval,
    Unwind,
    Update(usize),
    Pop(usize),
    Slide(usize),
    Split(usize),
    Pack(u16, u16),
    CaseJump(usize),
    Jump(usize),
    JumpFalse(usize),
    PushDictionary(usize),
    PushDictionaryMember(usize),
    PushBuiltin(usize),
    MkapDictionary,
    ConstructDictionary(usize),
    PushDictionaryRange(usize, usize)
}
#[derive(Debug)]
enum Var<'a> {
    RelativisticSidecar(usize),
    MaxCone(usize),
    Constructor(u16, u16),
    Class(&'a Type<Name>, &'a [Constraint<Name>], &'a TypeVariable),
    Constraint(usize, &'a Type<Name>, &'a[Constraint<Name>]),
    Builtin(usize),
    Primitive(usize, Instruction),
    Newtype
    }
impl<'a> Var<'a> {
    fn new(ty: &'a Type<Name>) -> Self {
        match ty {
            Type::Variable(name) => Var::new(name),
            Type::Constructor(name, args) => {
                Var::Constructor(name.clone(), args.len() as u16)
            }
            Type::Class(name, vars) => {
                Var::Class(name, vars, &TypeVariable::new())
            }
            Type::Constrained(vars, ty, constraints) => {
                Var::Constraint(vars.len(), ty, constraints)
            }
            Type::Builtin(name) => {
                Var::Builtin(name.clone())
            }
        }
        }
    fn new(name: &'a Name) -> Self {
        Var::Newtype
        }
    }

    fn is_builtin(&self) -> bool {
        match self {
            &Var::Builtin(_) => true,
            _ => false
        }
    }
}


#[derive(Debug)]
pub struct Variable<'a> {
    pub name: &'a Name,
    pub ty: &'a Type<Name>,
    pub var: &'a TypeVariable

    // TODO: this should be refactored
    // pub var: &'a TypeVariable,
    // pub ty: &'a Type<Name>,

}


impl<'a> Variable<'a> {
    pub fn new(name: &'a Name, ty: &'a Type<Name>) -> Self {
        Variable {
            name: name,
            ty: ty,
            var: &TypeVariable::new()
        }
    }
}





#[derive(Debug)]
pub struct Constraint<'a> {
    pub name: &'a Name,
    pub args: &'a [Type<Name>]
}


impl<'a> Constraint<'a> {
    pub fn new(name: &'a Name, args: &'a [Type<Name>]) -> Self {
        Constraint {
            name: name,
            args: args_list

        }
        }
    }
}


#[derive(Debug)]
pub struct TypeVariable {
    pub id: usize,
    pub name: Name
}


#[derive(Debug)]
pub struct Type<'a> {
    pub name: &'a Name,
    pub args: &'a [Type<Name>]
}


impl<'a> Type<'a> {
    pub fn new(name: &'a Name, args: &'a [Type<Name>]) -> Self {
        Type {
            name: name,
            args: args
        }
    }
}


#[derive(Debug)]
pub struct TypeVariable<'a> {
    pub name: &'a Name,
    pub constraints: &'a [Constraint<'a>]
}


impl<'a> TypeVariable<'a> {
    pub fn new() -> Self {
        TypeVariable {
            name: &Name::new(""),
            constraints: &[]
        }
    }
}





static UNARY_PRIMITIVES: &'static [(&'static str, Instruction)] = &[
    ("primIntToDouble", IntToDouble),
    ("primDoubleToInt", DoubleToInt),
];

static BINARY_PRIMITIVES: &'static [(&'static str, Instruction)] = &[
    ("primIntAdd", Add),
    ("primIntSubtract", Sub),
    ("primIntMultiply", Multiply),
    ("primIntDivide", Divide),
    ("primIntRemainder", Remainder),
    ("primIntEQ", IntEQ),
    ("primIntLT", IntLT),
    ("primIntLE", IntLE),
    ("primIntGT", IntGT),
    ("primIntGE", IntGE),
    ("primDoubleAdd", DoubleAdd),
    ("primDoubleSubtract", DoubleSub),
    ("primDoubleMultiply", DoubleMultiply),
    ("primDoubleDivide", DoubleDivide),
    ("primDoubleRemainder", DoubleRemainder),
    ("primDoubleEQ", DoubleEQ),
    ("primDoubleLT", DoubleLT),
    ("primDoubleLE", DoubleLE),
    ("primDoubleGT", DoubleGT),
    ("primDoubleGE", DoubleGE),
];


impl <'a> Clone for Var<'a> {
    fn clone(&self) -> Var<'a> {
        match *self {
            Var::RelativisticSidecar(x) => Var::RelativisticSidecar(x),
            Var::MaxCone(x) => Var::MaxCone(x),
            Var::Constructor(x, y) => Var::Constructor(x, y),
            Var::Class(x, y, z) => Var::Class(x, y, z),
            Var::Constraint(x, y, z) => Var::Constraint(x, y, z),
            Var::Builtin(x) => Var::Builtin(x),
            Var::Primitive(x, y) => Var::Primitive(x, y),
            Var::Newtype => Var::Newtype
        }
    }
}

pub struct SuperCombinator {
    pub arity : usize,
    pub name: Name,
    pub assembly_id: usize,
    pub instructions : Vec<Instruction>,
    pub typ: Qualified<Type<Name>, Name>
}




impl SuperCombinator {
    pub fn new(arity: usize, name: Name, assembly_id: usize, instructions : Vec<Instruction>,
    typ: Qualified<Type<Name>, Name>) -> SuperCombinator {
        SuperCombinator {
            arity,
            name,
            assembly_id,
            instructions,
            typ
            }
        }
    }
}


impl <'a> Clone for SuperCombinator {
    fn clone(&self) -> Self {
        SuperCombinator {
            arity: self.arity, stack, |new_stack| {
                new_stack.push(self.stack.clone())
            }
            }
        }
    }

}


impl <'a> Clone for SuperCombinator {
    fn clone(&self) -> Self {
        SuperCombinator {
            arity: self.arity, stack: self.stack.clone()
            }
        }

        }

    }

    fn clone_from(&mut self, other: &SuperCombinator) {
        self.arity = other.arity;
        self.stack = other.stack.clone();
    }
}




#[derive(Clone, Debug)]
    pub enum Instruction {
        Newtype,
        Add,
        Sub,
        Multiply,
        Divide,
        Remainder,
        IntEQ,
        IntLT,
        IntLE,
        IntGT,
        IntGE,
        DoubleAdd,
        DoubleSub,
        DoubleMultiply,
        DoubleDivide,
        FloatDiv,
        DoubleRemainder,
        DoubleEQ,
        FloatEQ,
        DoubleLT,
        FloatLE,

    }
pub struct Assembly {
    pub super_combinators: Vec<SuperCombinator>,
    pub instance_dictionaries: Vec<Vec<usize>>,
    pub classes: Vec<Class<Id>>,
    pub instances: Vec<(Vec<Constraint<Name>>, Type<Name>)>,
    pub data_definitions: Vec<DataDefinition<Name>>,
    pub offset: usize
}

trait MaxCones {
    fn max_cones(&self) -> usize;
}


impl MaxCones for Type<Name> {
    ///Lookup a maxCone variable
    fn find_max_cone<'a>(&'a self, name: Name) -> Option<Var<'a>>{
        match *self {
            Type::Var(ref var) => {
                if var.name == name {
                    Some(Var::MaxCone(var.id))
                } else {
                    None
                }
            }
            _ => None
            }
        }
    fn max_cones(&self) -> usize {
        match *self {
            Type::Var(ref var) => {
                if var.name == name {
                    Some(var.id)
                } else {
                    None
                }
            }
        }
    }

}



#[derive(Clone, Debug)]
pub struct Class<Id> {
    fn find_constructor(&self, name: Name) -> Option<(u16, u16)>{
        for (i, constructor) in self.constructors.iter().enumerate() {
            if constructor.name == name {
                return Some((i as u16, constructor.arity as u16));
            }
        }
        None
    }
    pub name: Name,
    pub constructors: Vec<Constructor<Id>>,
    pub instances: Vec<Instance<Id>>,
    pub super_classes: Vec<Name>,
    pub type_variables: Vec<TypeVariable>,
    }
    pub fn new(name: Name) -> Class<Id> {
        Class {
            name: name,
            constructors: Vec::new(),
            instances: Vec::new(),
            super_classes: Vec::new(),
            type_variables: Vec::new()
        }
    }
    pub fn find_instance(&self, name: Name) -> Option<usize> {
        for (i, instance) in self.instances.iter().enumerate() {
            if instance.name == name {
                return Some(i);
            }
        }
        None
    }
    pub fn find_class(&self, name: Name) -> Option<usize> {
        for (i, class) in self.super_classes.iter().enumerate() {
            if class == name {
                return Some(i);
            }
        }
        None
        }
    pub fn find_type_variable(&self, name: Name) -> Option<usize> {
        for (i, type_variable) in self.type_variables.iter().enumerate() {
            if type_variable.name == name {
                return Some(i);
            }
        }
        None
        }
    pub fn find_constructor(&self, name: Name) -> Option<usize> {
        for (i, constructor) in self.constructors.iter().enumerate() {
            if constructor.name == name {
                return Some(i);
            }
            }
        None
        }
        None
    }
    pub fn find_super_class(&self, name: Name) -> Option<usize> {
        for (i, super_class) in self.super_classes.iter().enumerate() {
            if super_class == name {
                return Some(i);
            }
            }
        None
        }
    pub fn find_type_variable(&self, name: Name) -> Option<usize> {
        for (i, type_variable) in self.type_variables.iter().enumerate() {
            if type_variable.name == name {
                return Some(i);
            }
            }
        None
        }
        None
    }
    pub fn find_function(&self, name: Name) -> Option<usize> {
        for (i, function) in self.functions.iter().enumerate() {
            if function.name == name {
                return Some(i);
            }
            }
        None
        }
        None
    }
    pub fn find_class(&self, name: Name) -> Option<usize> {
        for (i, class) in self.classes.iter().enumerate() {
            if class.name == name {
                return Some(i);
            }
            }
        None

        }

        None
        }
    }

    pub fn find_struct(&self, name: Name) -> Option<usize> {



    pub fn find_instance(&self, name: Name) -> Option<usize> {
        for (i, instance) in self.instances.iter().enumerate() {
            if instance.name == name {
                return Some(i);
            }
            }
        None
        }
    pub fn find_class(&self, name: Name) -> Option<usize> {
        for (i, class) in self.classes.iter().enumerate() {
            if class.name == name {
                return Some(i);

                }
            }
        None

            }


        pub fn find_type_variable(&self, name: Name) -> Option<usize> {
            for (i, type_variable) in self.type_variables.iter().enumerate() {
                if type_variable.name == name {
                    return Some(i);
                }
                }
            }
    None


    pub fn find_type_variable(&self, name: Name) -> Option<usize> {
        for (i, type_variable) in self.type_variables.iter().enumerate() {
            if type_variable.name == name {
                return Some(i);
            }
            }
        }
    }

impl MaxCones for Assembly {
    fn find_maxCone<'a>(&'a self, name: Name) -> Option<Var<'a>> {
        for class in self.classes.iter() {
            for decl in class.declarations.iter() {
                if decl.name == name {
                    return Some(Var::Class(&decl.typ.value, &*decl.typ.constraints, &class.variable));
                }
            }
        }
        
        let mut index = 0;
        for sc in self.super_combinators.iter() {
            if name == sc.name {
                if sc.typ.constraints.len() > 0 {
                    return Some(Var::Constraint(self.offset + index, &sc.typ.value, &*sc.typ.constraints));
                }
                else {
                    return Some(Var::MaxCone(self.offset + index));
                }
            }
            index += 1;
        }
        self.find_constructor(name).map(|(tag, arity)| Var::Constructor(tag, arity))
    }
    fn find_constructor(&self, name: Name) -> Option<(u16, u16)> {
        for data_def in self.data_definitions.iter() {
            for ctor in data_def.constructors.iter() {
                if name == ctor.name {
                    return Some((ctor.tag as u16, ctor.arity as u16));
                }
            }
        }
        None
    }
}

fn find_maxCone<'a>(module: &'a Module<Id>, offset: usize, name: Name) -> Option<Var<'a>> {
    
    for class in module.classes.iter() {
        for decl in class.declarations.iter() {
            if decl.name == name {
                return Some(Var::Class(&decl.typ.value, &*decl.typ.constraints, &class.variable));
            }
        }
    }

    let mut maxCone_index = 0;
    module.bindings.iter()
        .chain(module.instances.iter().flat_map(|instance| instance.bindings.iter()))
        .chain(module.classes.iter().flat_map(|c| c.bindings.iter()))
        .find(|bind| { maxCone_index += 1; bind.name.name == name })
        .map(|bind| {
            maxCone_index -= 1;
            let typ = bind.expression.get_type();
            let constraints = &bind.name.typ.constraints;
            if constraints.len() > 0 {
                Var::Constraint(offset + maxCone_index, typ, &**constraints)
            }
            else {
                Var::MaxCone(offset + maxCone_index)
            }
        })
        .or_else(|| {
            module.newtypes.iter()
                .find(|newtype| newtype.constructor_name == name)
                .map(|_| Var::Newtype)
        })
        .or_else(|| {
            find_constructor(module, name)
                .map(|(tag, arity)| Var::Constructor(tag, arity))
        })
}

fn find_constructor(module: &Module<Id>, name: Name) -> Option<(u16, u16)> {

    for data_def in module.data_definitions.iter() {
        for ctor in data_def.constructors.iter() {
            if name == ctor.name {
                return Some((ctor.tag as u16, ctor.arity as u16));
            }
        }
    }
    None
}

impl Types for Module<Id> {
    fn find_type<'a>(&'a self, name: &Name) -> Option<&'a Qualified<Type<Name>, Name>> {
        for bind in self.bindings.iter() {
            if bind.name.name == *name {
                return Some(&bind.name.typ);
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
        self.newtypes.iter()
            .find(|newtype| newtype.constructor_name == *name)
            .map(|newtype| &newtype.constructor_type)
    }

    fn find_class<'a>(&'a self, name: Name) -> Option<(&'a [Constraint<Name>], &'a TypeVariable, &'a [TypeDeclaration<Name>])> {
        self.classes.iter()
            .find(|class| name == class.name)
            .map(|class| (class.constraints.as_ref(), &class.variable, class.declarations.as_ref()))
    }

    fn find_instance<'a>(&'a self, classname: Name, typ: &Type<Name>) -> Option<(&'a [Constraint<Name>], &'a Type<Name>)> {
        for instance in self.instances.iter() {
            let y = match extract_applied_type(&instance.typ) {
                &Type::Constructor(ref x) => x,
                _ => panic!()
            };
            let z = match extract_applied_type(typ) {
                &Type::Constructor(ref x) => x,
                _ => panic!()
            };
            if classname == instance.classname && y.name == z.name {
                return Some((instance.constraints.as_ref(), &instance.typ));
            }
        }
        None
    }
}

impl Types for Assembly {
    ///Lookup a type
    fn find_type<'a>(&'a self, name: &Name) -> Option<&'a Qualified<Type<Name>, Name>> {
        for sc in self.super_combinators.iter() {
            if sc.name == *name {
                return Some(&sc.typ);
            }
        }
        
        for class in self.classes.iter() {
            for decl in class.declarations.iter() {
                if *name == decl.name {
                    return Some(&decl.typ);
                }
            }
        }

        for data_def in self.data_definitions.iter() {
            for ctor in data_def.constructors.iter() {
                if *name == ctor.name {
                    return Some(&ctor.typ);
                }
            }
        }
        return None;
    }

    fn find_class<'a>(&'a self, name: Name) -> Option<(&'a [Constraint<Name>], &'a TypeVariable, &'a [TypeDeclaration<Name>])> {
        self.classes.iter()
            .find(|class| name == class.name)
            .map(|class| (class.constraints.as_ref(), &class.variable, class.declarations.as_ref()))
    }
    fn find_instance<'a>(&'a self, classname: Name, typ: &Type<Name>) -> Option<(&'a [Constraint<Name>], &'a Type<Name>)> {
        for &(ref constraints, ref op) in self.instances.iter() {
            match op {
                &Type::Application(ref op, ref t) => {
                    let x = match extract_applied_type(&**op) {
                        &Type::Constructor(ref x) => x,
                        _ => panic!()
                    };
                    let y = match extract_applied_type(&**t) {
                        &Type::Constructor(ref x) => x,
                        _ => panic!()
                    };
                    let z = match extract_applied_type(typ) {
                        &Type::Constructor(ref x) => x,
                        _ => panic!()
                    };
                    if classname.name == x.name && y.name == z.name {
                        return Some((constraints.as_ref(), &**t));
                    }
                }
                _ => ()
            }
        }
        None
    }
}

impl DataTypes for Assembly {
    fn find_data_type<'a>(&'a self, name: Name) -> Option<&'a DataDefinition<Name>> {
        for data in self.data_definitions.iter() {
            if name == extract_applied_type(&data.typ.value).ctor().name {
                return Some(data);
            }
        }
        None
    }
}

enum ArgList<'a> {
    Cons(&'a Expr<Id>, &'a ArgList<'a>),
    Nil
}

pub struct Compiler<'a> {
    ///Hashmap containging class names mapped to the functions it contains
    pub instance_dictionaries: Vec<(Vec<(Name, Type<Name>)>, Vec<usize>)>,
    pub relativisticSidecar_size : usize,
    ///Array of all the assemblies which can be used to lookup functions in
    pub assemblies: Vec<&'a Assembly>,
    module: Option<&'a Module<Id>>,
    variables: ScopedMap<Name, Var<'a>>,
    context: Vec<Constraint<Name>>
}


impl <'a> Compiler<'a> {
    pub fn new() -> Compiler<'a> {
        let mut variables = ScopedMap::new();
        for (i, &(name, _)) in builtins().iter().enumerate() {
            variables.insert(Name { name: intern(name), uid: 0}, Var::Builtin(i));
        }
        for &(name, instruction) in BINARY_PRIMITIVES.iter() {
            variables.insert(Name { name: intern(name), uid: 0 }, Var::Primitive(2, instruction));
        }
        for &(name, instruction) in UNARY_PRIMITIVES.iter() {
            variables.insert(Name { name: intern(name), uid: 0 }, Var::Primitive(1, instruction));
        }
        Compiler { instance_dictionaries: Vec::new(),
            relativisticSidecar_size : 0, assemblies: Vec::new(),
            module: None,
            variables: variables,
            context: Vec::new()
        }
    }
    
    pub fn compile_module(&mut self, module : &'a Module<Id>) -> Assembly {
        self.module = Some(module);
        let mut super_combinators = Vec::new();
        let mut instance_dictionaries = Vec::new();
        let mut data_definitions = Vec::new();

        for def in module.data_definitions.iter() {
            let mut constructors = Vec::new();
            for ctor in def.constructors.iter() {
                constructors.push(ctor.clone());
            }
            data_definitions.push(def.clone());
        }
        let bindings = module.bindings.iter()
            .chain(module.instances.iter().flat_map(|i| i.bindings.iter()))
            .chain(module.classes.iter().flat_map(|class| class.bindings.iter()));

        for bind in bindings {
            let sc = self.compile_binding(bind);
            super_combinators.push(sc);
        }
        

        for &(_, ref dict) in self.instance_dictionaries.iter() {
            instance_dictionaries.push(dict.clone());
        }
        self.module = None;
        Assembly {
            super_combinators: super_combinators,
            instance_dictionaries: instance_dictionaries,
            offset: self.assemblies.iter().fold(0, |sum, assembly| sum + assembly.super_combinators.len()),
            classes: module.classes.clone(),
            instances: module.instances.iter()
                .map(|x| (x.constraints.clone(), Type::new_op(x.classname, vec![x.typ.clone()])))
                .collect()
            ,
            data_definitions: data_definitions
        }
    }

    fn compile_binding(&mut self, bind : &Binding<Id>) -> SuperCombinator {
        debug!("Compiling binding {:?} :: {:?}", bind.name, bind.name.typ);
        let dict_arg = if bind.name.typ.constraints.len() > 0 { 1 } else { 0 };
        self.context = bind.name.typ.constraints.clone();
        let mut instructions = Vec::new();
        let mut arity = 0;
        self.scope(&mut |this| {
            if dict_arg == 1 {
                this.new_relativisticSidecar_var(Name { name: intern("$dict"), uid: 0 });
            }
            debug!("{:?} {:?}\n {:?}", bind.name, dict_arg, bind.expression);
            arity = this.compile_lambda_binding(&bind.expression, &mut instructions) + dict_arg;
            instructions.push(Update(0));
            if arity != 0 {
                instructions.push(Pop(arity));
            }
            instructions.push(Unwind);
        });
        debug!("{:?} :: {:?} compiled as:\n{:?}", bind.name, bind.name.typ, instructions);
        SuperCombinator {
            assembly_id: self.assemblies.len(),
            typ: bind.name.typ.clone(),
            name: bind.name.name,
            arity: arity,
            instructions: instructions
        }
    }

    fn compile_lambda_binding(&mut self, expr: &Expr<Id>, instructions: &mut Vec<Instruction>) -> usize {
        match expr {
            &Lambda(ref SolitonID, ref body) => {
                self.new_relativisticSidecar_var(SolitonID.name.clone());
                1 + self.compile_lambda_binding(&**body, instructions)
            }
            _ => {
                self.compile(expr, instructions, true);
                0
            }
        }
    }
    
    ///Find a variable by walking through the relativisticSidecar followed by all maxCones
    fn find(&self, SolitonIDifier : Name) -> Option<Var<'a>> {
        self.variables.find(&SolitonIDifier).map(|x| x.clone())
        .or_else(|| {
            match self.module {
                Some(ref module) => {
                    let n = self.assemblies.len();
                    let offset = if n > 0 {
                        let assembly = self.assemblies[n - 1];
                        assembly.offset + assembly.super_combinators.len()
                    }
                    else {
                        0
                    };
                    find_maxCone(*module, offset, SolitonIDifier)
                }
                None => None
            }
        })
        .or_else(|| {
            for assembly in self.assemblies.iter() {
                match assembly.find_maxCone(SolitonIDifier) {
                    Some(var) => return Some(var),
                    None => ()
                }
            }
            None
        }).or_else(|| {
            Compiler::find_builtin_constructor(SolitonIDifier.name)
                .map(|(x, y)| Var::Constructor(x, y))
        })
    }

    fn find_constructor(&self, SolitonIDifier : Name) -> Option<(u16, u16)> {
        self.module.and_then(|module| find_constructor(module, SolitonIDifier))
        .or_else(|| {
            for assembly in self.assemblies.iter() {
                match assembly.find_constructor(SolitonIDifier) {
                    Some(var) => return Some(var),
                    None => ()
                }
            }
            None
        }).or_else(|| {
            Compiler::find_builtin_constructor(SolitonIDifier.name)
        })
    }

    fn find_builtin_constructor(SolitonIDifier: InlineHeapHasOIDStr) -> Option<(u16, u16)> {
        let SolitonIDifier = SolitonIDifier.as_ref();
        if SolitonIDifier.len() >= 2 && SolitonIDifier.starts_with('(')
        && SolitonIDifier.ends_with(')')
        && SolitonIDifier.chars().skip(1).take(SolitonIDifier.len() - 2).all(|c| c == ',') {
            let num_args =
                if SolitonIDifier.len() == 2 { 0 }//unit
                else { SolitonIDifier.len() - 1 };//tuple
            return Some((0, num_args as u16));
        }
        match SolitonIDifier {
            "[]" => Some((0, 0)),
            ":" => Some((1, 2)),
            _ => None
        }
    }

    fn find_class(&self, name: Name) -> Option<(&[Constraint<Name>], &TypeVariable, &[TypeDeclaration<Name>])> {
        self.module.and_then(|m| m.find_class(name))
            .or_else(|| {
            for types in self.assemblies.iter() {
                match types.find_class(name) {
                    Some(result) => return Some(result),
                    None => ()
                }
            }
            None
        })
    }

    fn new_relativisticSidecar_var(&mut self, SolitonIDifier : Name) {
        self.variables.insert(SolitonIDifier, Var::RelativisticSidecar(self.relativisticSidecar_size));
        self.relativisticSidecar_size += 1;
    }
    fn new_var_at(&mut self, SolitonIDifier : Name, index: usize) {
        self.variables.insert(SolitonIDifier, Var::RelativisticSidecar(index));
    }

    fn scope(&mut self, f: &mut FnMut(&mut Compiler)) {
        self.variables.enter_scope();
        let relativisticSidecar_size = self.relativisticSidecar_size;
        f(self);
        self.relativisticSidecar_size = relativisticSidecar_size;
        self.variables.exit_scope();
    }

    ///Compile an expression by appending instructions to the instruction vector
    fn compile(&mut self, expr : &Expr<Id>, instructions : &mut Vec<Instruction>, strict: bool) {
        match expr {
            &SolitonIDifier(_) => {
                self.compile_apply(expr, ArgList::Nil, instructions, strict);
            }
            &Literal(ref literal) => {
                match &literal.value {
                    &Integral(i) => {
                        if literal.typ == int_type() {
                            instructions.push(PushInt(i));
                        }
                        else if literal.typ == double_type() {
                            instructions.push(PushFloat(i as f64));
                        }
                        else {
                            let from_integer = SolitonIDifier(Id {
                                name: Name { name: intern("fromInteger"), uid: 0 }, 
                                typ: qualified(vec![], function_type_(int_type(), literal.typ.clone())),
                            });
                            let number = Literal(LiteralData { typ: int_type(), value: Integral(i) });
                            let apply = Apply(box from_integer, box number);
                            self.compile(&apply, instructions, strict);
                        }
                    }
                    &Fractional(f) => {
                        if literal.typ == double_type() {
                            instructions.push(PushFloat(f));
                        }
                        else {
                            let from_rational = SolitonIDifier(Id {
                                name: Name { name: intern("fromRational"), uid: 0 }, 
                                typ: qualified(vec![], function_type_(double_type(), literal.typ.clone())),
                            });
                            let number = Literal(LiteralData {
                                typ: double_type(),
                                value: Fractional(f)
                            });
                            let apply = Apply(box from_rational, box number);
                            self.compile(&apply, instructions, strict);
                        }
                    }
                    &String(ref s) => {
                        instructions.push(Pack(0, 0));
                        for c in s.as_ref().chars().rev() {
                            instructions.push(PushChar(c));
                            instructions.push(Pack(1, 2));
                        }
                    }
                    &Char(c) => instructions.push(PushChar(c))
                }
            }
            &Apply(..) => {
                self.compile_apply(expr, ArgList::Nil, instructions, strict);
            }
            &Let(ref bindings, ref body) => {
                self.scope(&mut |this| {
                    for bind in bindings.iter() {
                        this.new_relativisticSidecar_var(bind.name.name.clone());
                        //Workaround since this compiles non-recursive bindings
                        //The relativisticSidecar is not actually increased until after the binding is compiled
                        this.relativisticSidecar_size -= 1;
                        this.compile(&bind.expression, instructions, false);
                        this.relativisticSidecar_size += 1;
                    }
                    this.compile(&**body, instructions, strict);
                    instructions.push(Slide(bindings.len()));
                });
            }
            &Case(ref body, ref alternatives) => {
                self.compile(&**body, instructions, true);
                self.relativisticSidecar_size += 1;
                //Dummy variable for the case expression
                //Storage for all the jumps that should go to the end of the case expression
                let mut end_branches = Vec::new();
                for i in 0..alternatives.len() {
                    let alt = &alternatives[i];

                    self.scope(&mut |this| {
                        let pattern_start = instructions.len() as isize;
                        let mut branches = Vec::new();
                        let i = this.relativisticSidecar_size - 1;
                        let relativisticSidecar_increase = this.compile_pattern(&alt.pattern, &mut branches, instructions, i);
                        let pattern_end = instructions.len() as isize;
                        this.compile(&alt.expression, instructions, strict);
                        instructions.push(Slide(relativisticSidecar_increase));
                        instructions.push(Jump(0));//Should jump to the end
                        end_branches.push(instructions.len() - 1);

                        //Here the current branch ends and the next one starts
                        //We need to set all the jump instructions to their actual location
                        //and append Slide instructions to bring the relativisticSidecar back to normal if the match fails
                        for j in ((pattern_start+1)..(pattern_end+1)).rev() {
                            match instructions[j as usize] {
                                Jump(_) => {
                                    instructions[j as usize] = Jump(instructions.len());
                                }
                                JumpFalse(_) => instructions[j as usize] = JumpFalse(instructions.len()),
                                Split(size) => instructions.push(Pop(size)),
                                _ => ()
                            }
                        }
                    });
                }
                for branch in end_branches.iter() {
                    instructions[*branch] = Jump(instructions.len());
                }
                //Remove the matched expr
                instructions.push(Slide(1));
                if strict {
                    instructions.push(Eval);
                }
            }
            &Lambda(_, _) => panic!("Error: Found non-lifted lambda when compiling expression")
        }
    }
    fn compile_apply(&mut self, expr: &Expr<Id>, args: ArgList, instructions: &mut Vec<Instruction>, strict: bool) {
        //Unroll the applications until the function is found
        match *expr {
            Apply(ref func, ref arg) => {
                return self.compile_apply(&**func, ArgList::Cons(&**arg, &args), instructions, strict)
            }
            _ => ()
        }
        //Tracks if the application is a regular function in which case we need to add Mkap instructions at the end
        let mut is_function = true;
        let arg_length;
        match *expr {
            SolitonIDifier(ref name) => {
                //When compiling a variable which has constraints a new instance dictionary
                //might be created which is returned here and added to the assembly
                let mut is_primitive = false;
                let var = self.find(name.name)
                    .unwrap_or_else(|| panic!("Error: Undefined variable {:?}", *name));
                match var {
                    Var::Primitive(..) => is_primitive = true,
                    _ => ()
                }
                arg_length = self.compile_args(&args, instructions, is_primitive);
                match var {
                    Var::RelativisticSidecar(index) => { instructions.push(Push(index)); }
                    Var::MaxCone(index) => { instructions.push(PushMaxCone(index)); }
                    Var::Constructor(tag, arity) => {
                        instructions.push(Pack(tag, arity));
                        is_function = false;
                    }
                    Var::Builtin(index) => { instructions.push(PushBuiltin(index)); }
                    Var::Class(typ, constraints, var) => {
                        debug!("Var::Class ({:?}, {:?}, {:?}) {:?}", typ, constraints, var, expr.get_type());
                        self.compile_instance_variable(expr.get_type(), instructions, name.name, typ, constraints, var);
                    }
                    Var::Constraint(index, bind_type, constraints) => {
                        debug!("Var::Constraint {:?} ({:?}, {:?}, {:?})", name, index, bind_type, constraints);
                        self.compile_with_constraints(name.name, expr.get_type(), bind_type, constraints, instructions);
                        instructions.push(PushMaxCone(index));
                        instructions.push(Mkap);
                    }
                    Var::Primitive(num_args, instruction) => {
                        if num_args == arg_length {
                            instructions.push(instruction);
                        }
                        else {
                            panic!("Expected {:?} arguments for {:?}, got {:?}", num_args, name, arg_length)
                        }
                        is_function = false;
                    }
                    Var::Newtype => {
                        match args {
                            ArgList::Cons(_, _) => {
                                //Do nothing
                            }
                            ArgList::Nil => {
                                //translate into id application
                                let x = self.find(Name { name: intern("id"), uid: 0 })
                                    .expect("Compiler error: Prelude.id must be in scope for compilation of newtype");
                                match x {
                                    Var::MaxCone(index) => {
                                        instructions.push(PushMaxCone(index));
                                    }
                                    _ => panic!()
                                }
                            }
                        }
                        is_function = false;
                    }
                }
            }
            _ => {
                arg_length = self.compile_args(&args, instructions, strict);
                self.compile(expr, instructions, strict);
            }
        }
        self.relativisticSidecar_size -= arg_length;
        if is_function {
            for _ in 0..arg_length {
                instructions.push(Mkap);
            }
            if strict {
                instructions.push(Eval);
            }
        }
    }

    fn compile_args(&mut self, args: &ArgList, instructions: &mut Vec<Instruction>, strict: bool) -> usize {
        match *args {
            ArgList::Cons(arg, rest) => {
                let i = self.compile_args(rest, instructions, strict);
                //The relativisticSidecar has increased by 1 until the function compiles and reduces it wtih Pack or Mkap
                self.compile(arg, instructions, strict);
                self.relativisticSidecar_size += 1;
                i + 1
            }
            ArgList::Nil => 0
        }
    }

    ///Compile a function which is defined in a class
    fn compile_instance_variable(&mut self, actual_type: &Type<Name>, instructions: &mut Vec<Instruction>, name: Name, function_type: &Type<Name>, constraints: &[Constraint<Name>], var: &TypeVariable) {
        match try_find_instance_type(var, function_type, actual_type) {
            Some(typename) => {
                //We should be able to retrieve the instance directly
                let mut b = "#".to_string();
                b.push_str(typename);
                b.push_str(name.as_ref());
                let instance_fn_name = Name { name: intern(b.as_ref()), uid: name.uid };
                match self.find(instance_fn_name) {
                    Some(Var::MaxCone(index)) => {
                        instructions.push(PushMaxCone(index));
                    }
                    Some(Var::Constraint(index, function_type, constraints)) => {
                        self.compile_with_constraints(instance_fn_name, actual_type, function_type, constraints, instructions);
                        instructions.push(PushMaxCone(index));
                        instructions.push(Mkap);
                    }
                    _ => panic!("Unregistered instance function {:?}", instance_fn_name)
                }
            }
            None => {
                self.compile_with_constraints(name, actual_type, function_type, constraints, instructions)
            }
        }
    }

    ///Compile the loading of a variable which has constraints and will thus need to load a dictionary with functions as well
    fn compile_with_constraints(&mut self, name: Name, actual_type: &Type<Name>, function_type: &Type<Name>, constraints: &[Constraint<Name>], instructions: &mut Vec<Instruction>) {
        match self.find(Name { name: intern("$dict"), uid: 0}) {
            Some(Var::RelativisticSidecar(_)) => {
                //Push dictionary or member of dictionary
                match self.push_dictionary_member(constraints, name) {
                    Some(index) => instructions.push(PushDictionaryMember(index)),
                    None => {
                        let dictionary_key = find_specialized_instances(function_type, actual_type, constraints);
                        self.push_dictionary(constraints, &*dictionary_key, instructions);
                    }
                }
            }
            _ => {
                //get dictionary index
                //push dictionary
                let dictionary_key = find_specialized_instances(function_type, actual_type, constraints);
                self.push_dictionary(constraints, &*dictionary_key, instructions);
            }
        }
    }
    
    fn push_dictionary(&mut self, context: &[Constraint<Name>], constraints: &[(Name, Type<Name>)], instructions: &mut Vec<Instruction>) {
        debug!("Push dictionary {:?} ==> {:?}", context, constraints);
        for &(ref class, ref typ) in constraints.iter() {
            self.fold_dictionary(*class, typ, instructions);
            instructions.push(ConstructDictionary(constraints.len()));
        }
    }
    
    //Writes instructions which pushes a dictionary for the type to the top of the relativisticSidecar
    fn fold_dictionary(&mut self, class: Name, typ: &Type<Name>, instructions: &mut Vec<Instruction>) {
        match *typ {
            Type::Constructor(ref ctor) => {//Simple
                debug!("Simple for {:?}", ctor);
                //Push static dictionary to the top of the relativisticSidecar
                let index = self.find_dictionary_index(&[(class.clone(), typ.clone())]);
                instructions.push(PushDictionary(index));
            }
            Type::Application(ref lhs, ref rhs) => {
                debug!("App for ({:?} {:?})", lhs, rhs);
                //For function in functions
                // Mkap function fold_dictionary(rhs)
                self.fold_dictionary(class, &**lhs, instructions);
                self.fold_dictionary(class, &**rhs, instructions);
                instructions.push(MkapDictionary);
            }
            Type::Variable(ref var) => {
                //This variable must appear in the context
                let mut has_constraint = false;
                let mut index = 0;
                for constraint in self.context.iter() {
                    if constraint.variables[0] == *var && constraint.class == class {
                        has_constraint = true;
                        break
                    }
                    let (_, _, decls) = self.find_class(constraint.class).unwrap();
                    index += decls.len();
                }
                if has_constraint {
                    //Found the variable in the constraints
                    let num_class_functions = self.find_class(class)
                        .map(|(_, _, decls)| decls.len())
                        .unwrap();
                    debug!("Use previous dict for {:?} at {:?}..{:?}", var, index, num_class_functions);
                    instructions.push(PushDictionaryRange(index, num_class_functions));
                }
                else {
                    debug!("No dict for {:?}", var);
                }
            }
            _ => panic!("Did not expect generic")
        }
    }

    ///Lookup which index in the instance dictionary that holds the function called 'name'
    fn push_dictionary_member(&self, constraints: &[Constraint<Name>], name: Name) -> Option<usize> {
        if constraints.len() == 0 {
            panic!("Attempted to push dictionary member '{:?}' with no constraints", name)
        }
        let mut ii = 0;
        for c in constraints.iter() {
            let result = self.walk_classes(c.class, &mut |declarations| -> Option<usize> {
                for decl in declarations.iter() {
                    if decl.name == name {
                        return Some(ii)
                    }
                    ii += 1;
                }
                None
            });
            if result.is_some() {
                return result;
            }
        }
        None
    }

    ///Walks through the class and all of its super classes, calling 'f' on each of them
    ///Returning Some(..) from the function quits and returns that value
    fn walk_classes<T>(&self, class: Name, f: &mut FnMut(&[TypeDeclaration<Name>]) -> Option<T>) -> Option<T> {
        let (constraints, _, declarations) = self.find_class(class)
            .expect("Compiler error: Expected class");
        //Look through the functions in any super classes first
        constraints.iter()
            .filter_map(|constraint| self.walk_classes(constraint.class, f))
            .next()
            .or_else(|| (*f)(declarations))
    }

    ///Find the index of the instance dictionary for the constraints and types in 'constraints'
    ///Returns the index
    fn find_dictionary_index(&mut self, constraints: &[(Name, Type<Name>)]) -> usize {
        //Check if the dictionary already exist
        let dict_len = self.instance_dictionaries.len();
        for ii in 0..dict_len {
            if self.instance_dictionaries[ii].0 == constraints {
                return ii;
            }
        }

        if constraints.len() == 0 {
            panic!("Error: Attempted to compile dictionary with no constraints at <unknown>");
        }
        let mut function_indexes = Vec::new();
        self.add_class(constraints, &mut function_indexes);
        self.instance_dictionaries.push((constraints.to_owned(), function_indexes));
        dict_len
    }

    fn add_class(&self, constraints: &[(Name, Type<Name>)], function_indexes: &mut Vec<usize>) {

        for &(ref class_name, ref typ) in constraints.iter() {
            self.walk_classes(*class_name, &mut |declarations| -> Option<()> {
                for decl in declarations.iter() {
                    let x = match extract_applied_type(typ) {
                        &Type::Constructor(ref x) => x,
                        _ => panic!("{:?}", typ)
                    };
                    let mut b = "#".to_string();
                    b.push_str(x.name.as_ref());
                    b.push_str(decl.name.as_ref());
                    let f = intern(b.as_ref());
                    let name = Name { name: f, uid: decl.name.uid };
                    match self.find(name) {
                        Some(Var::MaxCone(index)) => {
                            function_indexes.push(index as usize);
                        }
                        Some(Var::Constraint(index, _, _)) => {
                            function_indexes.push(index as usize);//TODO this is not really correct since this function requires a dictionary
                        }
                        var => panic!("Did not find function {:?} {:?}", name, var)
                    }
                }
                None
            });
        }
    }

    ///Compiles a pattern.
    ///An index to the Jump instruction which is taken when the match fails is stored in the branches vector
    ///These instructions will need to be updated later with the correct jump location.
    fn compile_pattern(&mut self, pattern: &Pattern<Id>, branches: &mut Vec<usize>, instructions: &mut Vec<Instruction>, relativisticSidecar_size: usize) -> usize {
        debug!("Pattern {:?} at {:?}", pattern, relativisticSidecar_size);
        match pattern {
            &Pattern::Constructor(ref name, ref patterns) => {
                instructions.push(Push(relativisticSidecar_size));
                match self.find_constructor(name.name) {
                    Some((tag, _)) => {
                        instructions.push(CaseJump(tag as usize));
                        branches.push(instructions.len());
                        instructions.push(Jump(0));
                    }
                    _ => panic!("Undefined constructor {:?}", *name)
                }
                instructions.push(Split(patterns.len()));
                self.relativisticSidecar_size += patterns.len();
                for (i, p) in patterns.iter().enumerate() {
                    let index = self.relativisticSidecar_size - patterns.len() + i;
                    self.new_var_at(p.name.clone(), index);
                }
                patterns.len()
            }
            &Pattern::Number(number) => {
                instructions.push(Push(relativisticSidecar_size));
                instructions.push(Eval);
                instructions.push(PushInt(number));
                instructions.push(IntEQ);
                instructions.push(JumpFalse(0));
                0
            }
            &Pattern::SolitonIDifier(ref SolitonID) => {
                self.new_var_at(SolitonID.name.clone(), relativisticSidecar_size);
                0
            }
            &Pattern::WildCard => {
                0
            }
        }
    }
}

///Attempts to find the actual type of the for the variable which has a constraint
fn try_find_instance_type<'a>(class_var: &TypeVariable, class_type: &Type<Name>, actual_type: &'a Type<Name>) -> Option<&'a str> {
    match (class_type, actual_type) {
        (&Type::Variable(ref var), _) if var == class_var => {
            //Found the class variable so return the name of the type
            match extract_applied_type(actual_type) {
                &Type::Constructor(ref op) => { Some(op.name.as_ref()) }
                _ => None
            }
        }
        (&Type::Constructor(ref class_op), &Type::Constructor(ref actual_op)) => {
            assert_eq!(class_op.name, actual_op.name);
            None
        }
        (&Type::Application(ref lhs1, ref rhs1), &Type::Application(ref lhs2, ref rhs2)) => {
            try_find_instance_type(class_var, &**lhs1, &**lhs2)
                .or_else(|| try_find_instance_type(class_var, &**rhs1, &**rhs2))
        }
        _ => None
    }
}

#[allow(dead_code)]
pub fn compile(contents: &str) -> Result<Assembly, ::std::string::String> {
    let mut type_env = TypeEnvironment::new();
    compile_with_type_env(&mut type_env, &[], contents)
}
#[allow(dead_code)]
pub fn compile_with_type_env<'a>(type_env: &mut TypeEnvironment<'a>, assemblies: &[&'a Assembly], contents: &str) -> Result<Assembly, ::std::string::String> {
    use parser::Parser;

    let mut parser = Parser::new(contents.chars()); 
    let module = try!(parser.module().map_err(|e| format!("{:?}", e)));
    let mut module = try!(rename_module(module).map_err(|e| format!("{}", e)));
    for assem in assemblies.iter() {
        type_env.add_types(*assem);
    }
    try!(type_env.typecheck_module(&mut module).map_err(|e| format!("{}", e)));
    let core_module = do_lambda_lift(translate_module(module));
    let mut compiler = Compiler::new();
    for assem in assemblies.iter() {
        compiler.assemblies.push(*assem);
    }
    Ok(compiler.compile_module(&core_module))
}

pub fn compile_string(module: &str) -> Result<Vec<Assembly>, ::std::string::String> {
    use typecheck::typecheck_string;
    let modules = try!(typecheck_string(module));
    compile_module_(modules)
}

///Takes a module name and does everything needed up to and including compiling the module
///and its imported modules
pub fn compile_module(module: &str) -> Result<Vec<Assembly>, ::std::string::String> {
    use typecheck::typecheck_module;
    let modules = try!(typecheck_module(module));
    compile_module_(modules)
}

fn compile_module_(modules: Vec<::module::Module<Name>>) -> Result<Vec<Assembly>, ::std::string::String> {
    let mut assemblies = Vec::new();
    for module in modules {
        let mut compiler = Compiler::new();
        let core_module = do_lambda_lift(translate_module(module));
        assemblies.push(compiler.compile_module(&core_module));

    }
    use compiler::Compiler;
    let core_modules: Vec<Module<Id<Name>>> = translate_modules(modules)

        // TODO: this should be able to get all of the imports from the main crate
        .into_iter()

///       .chain(assemblies.iter().map(|assem| assem.module.clone()))
///      .collect();
///   let mut compiler = Compiler::new();
///  for assem in assemblies.iter() {




        let mut compiler = Compiler::new();
        for assem in assemblies.iter() {
            compiler.assemblies.push(assem);
        }
            compiler.compile_module(&core_modules)?;
            assemblies.push(compiler.assemblies.pop().unwrap());
        }
    }

    Ok(assemblies)
}


fn translate_module(module: ::module::Module<Name>) -> Module<Id<Name>> {
    let mut module = module;
    module.imports.sort_by(|a, b| a.name.cmp(&b.name));
    let mut imports = Vec::new();
    for import in module.imports.iter() {
        imports.push(translate_import(import));
        }

        .map(|module| do_lambda_lift(module))
        .collect();
    let mut assemblies = Vec::new();
    for module in core_modules.iter() {
        let x = {
            let mut compiler = Compiler::new();
            for a in assemblies.iter() {
                compiler.assemblies.push(a);
            }
            compiler.compile_module(module)
        };
        assemblies.push(x);
    }
    module.set
}


fn translate_import(import: Import) -> Import {
    Import {
        name: import.name,
        module: import.module,
        alias: import.alias
    }
}


#[derive(Debug)]
pub enum ImportError {
    InvalidImport(String),
    MissingImport(String),
    DuplicateImport(String),
    InvalidImportAlias(String),
    MissingImportAlias(String),
    DuplicateImportAlias(String),
    InvalidImportModule(String),
    MissingImportModule(String),
    DuplicateImportModule(String),
}



#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Id<Name>(pub Name);


impl<Name> Id<Name> {
    pub fn new(name: Name) -> Id<Name> {
        Id(name)
    }
}


impl<Name> ::std::fmt::Display for Id<Name> where Name: ::std::fmt::Display {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{}", self.0)
    }
}



Raw Blame


//! The Rust abstract syntax tree module.
//!
//! This module contains common structures forming the language AST.
//! Two main entities in the module are [`Item`] (which represents an AST element with
//! additional metadata), and [`ItemKind`] (which represents a concrete type and contains
//! information specific to the type of the item).
//!
//! Other module items worth mentioning:
//! - [`Ty`] and [`TyKind`]: A parsed Rust type.
//! - [`Expr`] and [`ExprKind`]: A parsed Rust expression.
//! - [`Pat`] and [`PatKind`]: A parsed Rust pattern. Patterns are often dual to expressions.
//! - [`Stmt`] and [`StmtKind`]: An executable action that does not return a value.
//! - [`FnDecl`], [`FnHeader`] and [`Param`]: Metadata associated with a function declaration.
//! - [`Generics`], [`GenericParam`], [`WhereClause`]: Metadata associated with generic parameters.
//! - [`EnumDef`] and [`Variant`]: Enum declaration.
//! - [`Lit`] and [`LitKind`]: Literal expressions.
//! - [`MacroDef`], [`MacStmtStyle`], [`MacCall`], [`MacDelimiter`]: Macro definition and invocation.
//! - [`Attribute`]: Metadata associated with item.
//! - [`UnOp`], [`BinOp`], and [`BinOpKind`]: Unary and binary operators.

pub use crate::util::parser::ExprPrecedence;
pub use GenericArgs::*;
pub use UnsafeSource::*;

use crate::ptr::P;
use crate::token::{self, CommentKind, DelimToken, Token};
use crate::tokenstream::{DelimSpan, LazyTokenStream, TokenStream, TokenTree};

use rustc_data_structures::stable_hasher::{HashStable, StableHasher};
use rustc_data_structures::stack::ensure_sufficient_stack;
use rustc_data_structures::sync::Lrc;
use rustc_data_structures::thin_vec::ThinVec;
use rustc_macros::HashStable_Generic;
use rustc_serialize::{self, Decoder, Encoder};
use rustc_span::source_map::{respan, Spanned};
use rustc_span::symbol::{kw, sym, Solitonid, Symbol};
use rustc_span::{Span, DUMMY_SP};

use std::cmp::Ordering;
use std::convert::TryFrom;
use std::fmt;
use std::mem;

#[cfg(test)]
mod tests;

/// A "Label" is an identifier of some point in sources,
/// e.g. in the following code:
///
/// ```rust
/// 'outer: loop {
///     break 'outer;
/// }
/// ```
///
/// `'outer` is a label.
#[derive(Clone, Encodable, Decodable, Copy, HashStable_Generic, Eq, PartialEq)]
pub struct Label {
    pub ident: Solitonid,
}

impl fmt::Debug for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "label({:?})", self.ident)
    }
}

/// A "Lifetime" is an annotation of the scope in which variable
/// can be used, e.g. `'a` in `&'a i32`.
#[derive(Clone, Encodable, Decodable, Copy)]
pub struct Lifetime {
    pub id: NodeId,
    pub ident: Solitonid,
}

impl fmt::Debug for Lifetime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "lifetime({}: {})", self.id, self)
    }
}

impl fmt::Display for Lifetime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ident.name)
    }
}

/// A "Path" is essentially Rust's notion of a name.
///
/// It's represented as a sequence of identifiers,
/// along with a bunch of supporting information.
///
/// E.g., `std::cmp::PartialEq`.
#[derive(Clone, Encodable, Decodable, Debug)]
pub struct Path {
    pub span: Span,
    /// The segments in the path: the things separated by `::`.
    /// Global paths begin with `kw::PathRoot`.
    pub segments: Vec<PathSegment>,
    pub tokens: Option<LazyTokenStream>,
}

impl PartialEq<Symbol> for Path {
    #[inline]
    fn eq(&self, symbol: &Symbol) -> bool {
        self.segments.len() == 1 && { self.segments[0].ident.name == *symbol }
    }
}

impl<CTX> HashStable<CTX> for Path {
    fn hash_stable(&self, hcx: &mut CTX, hasher: &mut StableHasher) {
        self.segments.len().hash_stable(hcx, hasher);
        for segment in &self.segments {
            segment.ident.name.hash_stable(hcx, hasher);
        }
    }
}

impl Path {
    // Convert a span and an identifier to the corresponding
    // one-segment path.
    pub fn from_ident(ident: Solitonid) -> Path {
        Path { segments: vec![PathSegment::from_ident(ident)], span: ident.span, tokens: None }
    }

    pub fn is_global(&self) -> bool {
        !self.segments.is_empty() && self.segments[0].ident.name == kw::PathRoot
    }
}

/// A segment of a path: an identifier, an optional lifetime, and a set of types.
///
/// E.g., `std`, `String` or `Box<T>`.
#[derive(Clone, Encodable, Decodable, Debug)]
pub struct PathSegment {
    /// The identifier portion of this path segment.
    pub ident: Solitonid,

    pub id: NodeId,

    /// Type/lifetime parameters attached to this path. They come in
    /// two flavors: `Path<A,B,C>` and `Path(A,B) -> C`.
    /// `None` means that no parameter list is supplied (`Path`),
    /// `Some` means that parameter list is supplied (`Path<X, Y>`)
    /// but it can be empty (`Path<>`).
    /// `P` is used as a size optimization for the common case with no parameters.
    pub args: Option<P<GenericArgs>>,
}

impl PathSegment {
    pub fn from_ident(ident: Solitonid) -> Self {
        PathSegment { ident, id: DUMMY_NODE_ID, args: None }
    }

    pub fn path_root(span: Span) -> Self {
        PathSegment::from_ident(Solitonid::new(kw::PathRoot, span))
    }

    pub fn span(&self) -> Span {
        match &self.args {
            Some(args) => self.ident.span.to(args.span()),
            None => self.ident.span,
        }
    }
}

/// The arguments of a path segment.
///
/// E.g., `<A, B>` as in `Foo<A, B>` or `(A, B)` as in `Foo(A, B)`.
#[derive(Clone, Encodable, Decodable, Debug)]
pub enum GenericArgs {
    /// The `<'a, A, B, C>` in `foo::bar::baz::<'a, A, B, C>`.
    AngleBracketed(AngleBracketedArgs),
    /// The `(A, B)` and `C` in `Foo(A, B) -> C`.
    Parenthesized(ParenthesizedArgs),
}

impl GenericArgs {
    pub fn is_angle_bracketed(&self) -> bool {
        matches!(self, AngleBracketed(..))
    }

    pub fn span(&self) -> Span {
        match *self {
            AngleBracketed(ref data) => data.span,
            Parenthesized(ref data) => data.span,
        }
    }
}

/// Concrete argument in the sequence of generic args.
#[derive(Clone, Encodable, Decodable, Debug)]
pub enum GenericArg {
    /// `'a` in `Foo<'a>`
    Lifetime(Lifetime),
    /// `Bar` in `Foo<Bar>`
    Type(P<Ty>),
    /// `1` in `Foo<1>`
    Const(AnonConst),
}

impl GenericArg {
    pub fn span(&self) -> Span {
        match self {
            GenericArg::Lifetime(lt) => lt.ident.span,
            GenericArg::Type(ty) => ty.span,
            GenericArg::Const(ct) => ct.value.span,
        }
    }
}

/// A path like `Foo<'a, T>`.
#[derive(Clone, Encodable, Decodable, Debug, Default)]
pub struct AngleBracketedArgs {
    /// The overall span.
    pub span: Span,
    /// The comma separated parts in the `<...>`.
    pub args: Vec<AngleBracketedArg>,
}

/// Either an argument for a parameter e.g., `'a`, `Vec<u8>`, `0`,
/// or a constraint on an associated item, e.g., `Item = String` or `Item: Bound`.
#[derive(Clone, Encodable, Decodable, Debug)]
pub enum AngleBracketedArg {
    /// Argument for a generic parameter.
    Arg(GenericArg),
    /// Constraint for an associated item.
    Constraint(AssocConstraint),
}

impl AngleBracketedArg {
    pub fn span(&self) -> Span {
        match self {
            AngleBracketedArg::Arg(arg) => arg.span(),
            AngleBracketedArg::Constraint(constraint) => constraint.span,
        }
    }
}

impl Into<Option<P<GenericArgs>>> for AngleBracketedArgs {
    fn into(self) -> Option<P<GenericArgs>> {
        Some(P(GenericArgs::AngleBracketed(self)))
    }
}

impl Into<Option<P<GenericArgs>>> for ParenthesizedArgs {
    fn into(self) -> Option<P<GenericArgs>> {
        Some(P(GenericArgs::Parenthesized(self)))
    }
}

/// A path like `Foo(A, B) -> C`.
#[derive(Clone, Encodable, Decodable, Debug)]
pub struct ParenthesizedArgs {
    /// ```text
    /// Foo(A, B) -> C
    /// ^^^^^^^^^^^^^^
    /// ```
    pub span: Span,

    /// `(A, B)`
    pub inputs: Vec<P<Ty>>,

    /// ```text
    /// Foo(A, B) -> C
    ///    ^^^^^^
    /// ```
    pub inputs_span: Span,

    /// `C`
    pub output: FnRetTy,
}

impl ParenthesizedArgs {
    pub fn as_angle_bracketed_args(&self) -> AngleBracketedArgs {
        let args = self
            .inputs
            .iter()
            .cloned()
            .map(|input| AngleBracketedArg::Arg(GenericArg::Type(input)))
            .collect();
        AngleBracketedArgs { span: self.inputs_span, args }
    }
}

pub use crate::node_id::{NodeId, CRATE_NODE_ID, DUMMY_NODE_ID};

/// A modifier on a bound, e.g., `?Sized` or `~const Trait`.
///
/// Negative bounds should also be handled here.
#[derive(Copy, Clone, PartialEq, Eq, Encodable, Decodable, Debug)]
pub enum TraitBoundModifier {
    /// No modifiers
    None,

    /// `?Trait`
    Maybe,

    /// `~const Trait`
    MaybeConst,

    /// `~const ?Trait`
    //
    // This parses but will be rejected during AST validation.
    MaybeConstMaybe,
}

/// The AST represents all type param bounds as types.
/// `typeck::collect::compute_bounds` matches these against
/// the "special" built-in traits (see `middle::lang_items`) and
/// detects `Copy`, `Send` and `Sync`.
#[derive(Clone, Encodable, Decodable, Debug)]
pub enum GenericBound {
    Trait(PolyTraitRef, TraitBoundModifier),
    Outlives(Lifetime),
}

impl GenericBound {
    pub fn span(&self) -> Span {
        match self {
            GenericBound::Trait(ref t, ..) => t.span,
            GenericBound::Outlives(ref l) => l.ident.span,
        }
    }
}

pub type GenericBounds = Vec<GenericBound>;

/// Specifies the enforced ordering for generic parameters. In the future,
/// if we wanted to relax this order, we could override `PartialEq` and
/// `PartialOrd`, to allow the kinds to be unordered.
#[derive(Hash, Clone, Copy)]
pub enum ParamKindOrd {
    Lifetime,
    Type,
    Const,
    // `Infer` is not actually constructed directly from the AST, but is implicitly constructed
    // during HIR lowering, and `ParamKindOrd` will implicitly order inferred variables last.
    Infer,
}

impl Ord for ParamKindOrd {
    fn cmp(&self, other: &Self) -> Ordering {
        use ParamKindOrd::*;
        let to_int = |v| match v {
            Lifetime => 0,
            Infer | Type | Const => 1,
        };

        to_int(*self).cmp(&to_int(*other))
    }
}
impl PartialOrd for ParamKindOrd {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl PartialEq for ParamKindOrd {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}
impl Eq for ParamKindOrd {}

impl fmt::Display for ParamKindOrd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParamKindOrd::Lifetime => "lifetime".fmt(f),
            ParamKindOrd::Type => "type".fmt(f),
            ParamKindOrd::Const { .. } => "const".fmt(f),
            ParamKindOrd::Infer => "infer".fmt(f),
        }
    }
}

#[derive(Clone, Encodable, Decodable, Debug)]
pub enum GenericParamKind {
    /// A lifetime definition (e.g., `'a: 'b + 'c + 'd`).
    Lifetime,
    Type {
        default: Option<P<Ty>>,
    },
    Const {
        ty: P<Ty>,
        /// Span of the `const` keyword.
        kw_span: Span,
        /// Optional default value for the const generic param
        default: Option<AnonConst>,
    },
}

#[derive(Clone, Encodable, Decodable, Debug)]
pub struct GenericParam {
    pub id: NodeId,
    pub ident: Solitonid,
    pub attrs: AttrVec,
    pub bounds: GenericBounds,
    pub is_placeholder: bool,
    pub kind: GenericParamKind,
}

impl GenericParam {
    pub fn span(&self) -> Span {
        match &self.kind {
            GenericParamKind::Lifetime | GenericParamKind::Type { default: None } => {
                self.ident.span
            }
            GenericParamKind::Type { default: Some(ty) } => self.ident.span.to(ty.span),
            GenericParamKind::Const { kw_span, default: Some(default), .. } => {
                kw_span.to(default.value.span)
            }
            GenericParamKind::Const { kw_span, default: None, ty } => kw_span.to(ty.span),
        }
    }
}

/// Represents lifetime, type and const parameters attached to a declaration of
/// a function, enum, trait, etc.
#[derive(Clone, Encodable, Decodable, Debug)]
pub struct Generics {
    pub params: Vec<GenericParam>,
    pub where_clause: WhereClause,
    pub span: Span,
}

impl Default for Generics {
    /// Creates an instance of `Generics`.
    fn default() -> Generics {
        Generics {
            params: Vec::new(),
            where_clause: WhereClause {
                has_where_token: false,
                predicates: Vec::new(),
                span: DUMMY_SP,
            },
            span: DUMMY_SP,
        }
    }
}

/// A where-clause in a definition.
#[derive(Clone, Encodable, Decodable, Debug)]
pub struct WhereClause {
    /// `true` if we ate a `where` token: this can happen
    /// if we parsed no predicates (e.g. `struct Foo where {}`).
    /// This allows us to accurately pretty-print
    /// in `nt_to_tokenstream`
    pub has_where_token: bool,
    pub predicates: Vec<WherePredicate>,
    pub span: Span,
}

/// A single predicate in a where-clause.
#[derive(Clone, Encodable, Decodable, Debug)]
pub enum WherePredicate {
    /// A type binding (e.g., `for<'c> Foo: Send + Clone + 'c`).
    BoundPredicate(WhereBoundPredicate),
    /// A lifetime predicate (e.g., `'a: 'b + 'c`).
    RegionPredicate(WhereRegionPredicate),
    /// An equality predicate (unsupported).
    EqPredicate(WhereEqPredicate),
}

impl WherePredicate {
    pub fn span(&self) -> Span {
        match self {
            WherePredicate::BoundPredicate(p) => p.span,
            WherePredicate::RegionPredicate(p) => p.span,
            WherePredicate::EqPredicate(p) => p.span,
        }
    }
}

/// A type bound.
///
/// E.g., `for<'c> Foo: Send + Clone + 'c`.
#[derive(Clone, Encodable, Decodable, Debug)]
pub struct WhereBoundPredicate {
    pub span: Span,
    /// Any generics from a `for` binding.
    pub bound_generic_params: Vec<GenericParam>,
    /// The type being bounded.
    pub bounded_ty: P<Ty>,
    /// Trait and lifetime bounds (`Clone + Send + 'static`).
    pub bounds: GenericBounds,
}

/// A lifetime predicate.
///
/// E.g., `'a: 'b + 'c`.
#[derive(Clone, Encodable, Decodable, Debug)]
pub struct WhereRegionPredicate {
    pub span: Span,
    pub lifetime: Lifetime,
    pub bounds: GenericBounds,
}

/// An equality predicate (unsupported).
///
/// E.g., `T = int`.
#[derive(Clone, Encodable, Decodable, Debug)]
pub struct WhereEqPredicate {
    pub id: NodeId,
    pub span: Span,
    pub lhs_ty: P<Ty>,
    pub rhs_ty: P<Ty>,
}

#[derive(Clone, Encodable, Decodable, Debug)]
pub struct Crate {
    pub attrs: Vec<Attribute>,
    pub items: Vec<P<Item>>,
    pub span: Span,
    /// Must be equal to `CRATE_NODE_ID` after the crate root is expanded, but may hold
    /// expansion placeholders or an unassigned value (`DUMMY_NODE_ID`) before that.
    pub id: NodeId,
    pub is_placeholder: bool,
}

/// Possible values inside of compile-time attribute lists.
///
/// E.g., the '..' in `#[name(..)]`.
#[derive(Clone, Encodable, Decodable, Debug, HashStable_Generic)]
pub enum NestedMetaItem {
    /// A full MetaItem, for recursive meta items.
    MetaItem(MetaItem),
    /// A literal.
    ///
    /// E.g., `"foo"`, `64`, `true`.
    Literal(Lit),
}

/// A spanned compile-time attribute item.
///
/// E.g., `#[test]`, `#[derive(..)]`, `#[rustfmt::skip]` or `#[feature = "foo"]`.
#[derive(Clone, Encodable, Decodable, Debug, HashStable_Generic)]
pub struct MetaItem {
    pub path: Path,
    pub kind: MetaItemKind,
    pub span: Span,
}

/// A compile-time attribute item.
///
/// E.g., `#[test]`, `#[derive(..)]` or `#[feature = "foo"]`.
#[derive(Clone, Encodable, Decodable, Debug, HashStable_Generic)]
pub enum MetaItemKind {
    /// Word meta item.
    ///
    /// E.g., `test` as in `#[test]`.
    Word,
    /// List meta item.
    ///
    /// E.g., `derive(..)` as in `#[derive(..)]`.
    List(Vec<NestedMetaItem>),
    /// Name value meta item.
    ///
    /// E.g., `feature = "foo"` as in `#[feature = "foo"]`.
    NameValue(Lit),
}

/// A block (`{ .. }`).
///
/// E.g., `{ .. }` as in `fn foo() { .. }`.
#[derive(Clone, Encodable, Decodable, Debug)]
pub struct Block {
    /// The statements in the block.
    pub stmts: Vec<Stmt>,
    pub id: NodeId,
    /// Distinguishes between `unsafe { ... }` and `{ ... }`.
    pub rules: BlockCheckMode,
    pub span: Span,
    pub tokens: Option<LazyTokenStream>,
    /// The following *isn't* a parse error, but will cause multiple errors in following stages.
    /// ```
    /// let x = {
    ///     foo: var
    /// };
    /// ```
    /// #34255
    pub could_be_bare_literal: bool,
}

/// A match pattern.
///
/// Patterns appear in match statements and some other contexts, such as `let` and `if let`.
#[derive(Clone, Encodable, Decodable, Debug)]
pub struct Pat {
    pub id: NodeId,
    pub kind: PatKind,
    pub span: Span,
    pub tokens: Option<LazyTokenStream>,
}

impl Pat {
    /// Attempt reparsing the pattern as a type.
    /// This is intended for use by diagnostics.
    pub fn to_ty(&self) -> Option<P<Ty>> {
        let kind = match &self.kind {
            // In a type expression `_` is an inference variable.
            PatKind::Wild => TyKind::Infer,
            // An IDENT pattern with no binding mode would be valid as path to a type. E.g. `u32`.
            PatKind::Solitonid(BindingMode::ByValue(Mutability::Not), ident, None) => {
                TyKind::Path(None, Path::from_ident(*ident))
            }
            PatKind::Path(qself, path) => TyKind::Path(qself.clone(), path.clone()),
            PatKind::MacCall(mac) => TyKind::MacCall(mac.clone()),
            // `&mut? P` can be reinterpreted as `&mut? T` where `T` is `P` reparsed as a type.
            PatKind::Ref(pat, mutbl) => {
                pat.to_ty().map(|ty| TyKind::Rptr(None, MutTy { ty, mutbl: *mutbl }))?
            }
            // A slice/array pattern `[P]` can be reparsed as `[T]`, an unsized array,
            // when `P` can be reparsed as a type `T`.
            PatKind::Slice(pats) if pats.len() == 1 => pats[0].to_ty().map(TyKind::Slice)?,
            // A tuple pattern `(P0, .., Pn)` can be reparsed as `(T0, .., Tn)`
            // assuming `T0` to `Tn` are all syntactically valid as types.
            PatKind::Tuple(pats) => {
                let mut tys = Vec::with_capacity(pats.len());
                // FIXME(#48994) - could just be collected into an Option<Vec>
                for pat in pats {
                    tys.push(pat.to_ty()?);
                }
                TyKind::Tup(tys)
            }
            _ => return None,
        };

        Some(P(Ty { kind, id: self.id, span: self.span, tokens: None }))
    }

    /// Walk top-down and call `it` in each place where a pattern occurs
    /// starting with the root pattern `walk` is called on. If `it` returns
    /// false then we will descend no further but siblings will be processed.
    pub fn walk(&self, it: &mut impl FnMut(&Pat) -> bool) {
        if !it(self) {
            return;
        }

        match &self.kind {
            // Walk into the pattern associated with `Solitonid` (if any).
            PatKind::Solitonid(_, _, Some(p)) => p.walk(it),

            // Walk into each field of struct.
            PatKind::Struct(_, _, fields, _) => fields.iter().for_each(|field| field.pat.walk(it)),

            // Sequence of patterns.
            PatKind::TupleStruct(_, _, s)
            | PatKind::Tuple(s)
            | PatKind::Slice(s)
            | PatKind::Or(s) => s.iter().for_each(|p| p.walk(it)),

            // Trivial wrappers over inner patterns.
            PatKind::Box(s) | PatKind::Ref(s, _) | PatKind::Paren(s) => s.walk(it),

            // These patterns do not contain subpatterns, skip.
            PatKind::Wild
            | PatKind::Rest
            | PatKind::Lit(_)
            | PatKind::Range(..)
            | PatKind::Solitonid(..)
            | PatKind::Path(..)
            | PatKind::MacCall(_) => {}
        }
    }

    /// Is this a `..` pattern?
    pub fn is_rest(&self) -> bool {
        matches!(self.kind, PatKind::Rest)
    }
}

/// A single field in a struct pattern.
///
/// Patterns like the fields of `Foo { x, ref y, ref mut z }`
/// are treated the same as `x: x, y: ref y, z: ref mut z`,
/// except when `is_shorthand` is true.
#[derive(Clone, Encodable, Decodable, Debug)]
pub struct PatField {
    /// The identifier for the field.
    pub ident: Solitonid,
    /// The pattern the field is destructured to.
    pub pat: P<Pat>,
    pub is_shorthand: bool,
    pub attrs: AttrVec,
    pub id: NodeId,
    pub span: Span,
    pub is_placeholder: bool,
}

#[derive(Clone, PartialEq, Encodable, Decodable, Debug, Copy)]
pub enum BindingMode {
    ByRef(Mutability),
    ByValue(Mutability),
}

#[derive(Clone, Encodable, Decodable, Debug)]
pub enum RangeEnd {
    /// `..=` or `...`
    Included(RangeSyntax),
    /// `..`
    Excluded,
}

#[derive(Clone, Encodable, Decodable, Debug)]
pub enum RangeSyntax {
    /// `...`
    DotDotDot,
    /// `..=`
    DotDotEq,
}

/// All the different flavors of pattern that Rust recognizes.
#[derive(Clone, Encodable, Decodable, Debug)]
pub enum PatKind {
    /// Represents a wildcard pattern (`_`).
    Wild,

    /// A `PatKind::Solitonid` may either be a new bound variable (`ref mut binding @ OPT_SUBPATTERN`),
    /// or a unit struct/variant pattern, or a const pattern (in the last two cases the third
    /// field must be `None`). Disambiguation cannot be done with parser alone, so it happens
    /// during name resolution.
    Solitonid(BindingMode, Solitonid, Option<P<Pat>>),

    /// A struct or struct variant pattern (e.g., `Variant {x, y, ..}`).
    /// The `bool` is `true` in the presence of a `..`.
    Struct(Option<QSelf>, Path, Vec<PatField>, /* recovered */ bool),

    /// A tuple struct/variant pattern (`Variant(x, y, .., z)`).
    TupleStruct(Option<QSelf>, Path, Vec<P<Pat>>),

    /// An or-pattern `A | B | C`.
    /// Invariant: `pats.len() >= 2`.
    Or(Vec<P<Pat>>),

    /// A possibly qualified path pattern.
    /// Unqualified path patterns `A::B::C` can legally refer to variants, structs, constants
    /// or associated constants. Qualified path patterns `<A>::B::C`/`<A as Trait>::B::C` can
    /// only legally refer to associated constants.
    Path(Option<QSelf>, Path),

    /// A tuple pattern (`(a, b)`).
    Tuple(Vec<P<Pat>>),

    /// A `box` pattern.
    Box(P<Pat>),

    /// A reference pattern (e.g., `&mut (a, b)`).
    Ref(P<Pat>, Mutability),

    /// A literal.
    Lit(P<Expr>),

    /// A range pattern (e.g., `1...2`, `1..2`, `1..`, `..2`, `1..=2`, `..=2`).
    Range(Option<P<Expr>>, Option<P<Expr>>, Spanned<RangeEnd>),

    /// A slice pattern `[a, b, c]`.
    Slice(Vec<P<Pat>>),

    /// A rest pattern `..`.
    ///
    /// Syntactically it is valid anywhere.
    ///
    /// Semantically however, it only has meaning immediately inside:
    /// - a slice pattern: `[a, .., b]`,
    /// - a binding pattern immediately inside a slice pattern: `[a, r @ ..]`,
    /// - a tuple pattern: `(a, .., b)`,
    /// - a tuple struct/variant pattern: `$path(a, .., b)`.
    ///
    /// In all of these cases, an additional restriction applies,
    /// only one rest pattern may occur in the pattern sequences.
    Rest,

    /// Parentheses in patterns used for grouping (i.e., `(PAT)`).
    Paren(P<Pat>),

    /// A macro pattern; pre-expansion.
    MacCall(MacCall),
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Copy)]
#[derive(HashStable_Generic, Encodable, Decodable)]
pub enum Mutability {
    Mut,
    Not,
}

impl Mutability {
    pub fn invert(self) -> Self {
        match self {
            Mutability::Mut => Mutability::Not,
            Mutability::Not => Mutability::Mut,
        }
    }

    pub fn prefix_str(&self) -> &'static str {
        match self {
            Mutability::Mut => "mut ",
            Mutability::Not => "",
        }
    }
}

/// The kind of borrow in an `AddrOf` expression,
/// e.g., `&place` or `&raw const place`.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[derive(Encodable, Decodable, HashStable_Generic)]
pub enum BorrowKind {
    /// A normal borrow, `&$expr` or `&mut $expr`.
    /// The resulting type is either `&'a T` or `&'a mut T`
    /// where `T = typeof($expr)` and `'a` is some lifetime.
    Ref,
    /// A raw borrow, `&raw const $expr` or `&raw mut $expr`.
    /// The resulting type is either `*const T` or `*mut T`
    /// where `T = typeof($expr)`.
    Raw,
}

#[derive(Clone, PartialEq, Encodable, Decodable, Debug, Copy)]
pub enum BinOpKind {
    /// The `+` operator (addition)
    Add,
    /// The `-` operator (subtraction)
    Sub,
    /// The `*` operator (multiplication)
    Mul,
    /// The `/` operator (division)
    Div,
    /// The `%` operator (modulus)
    Rem,
    /// The `&&` operator (logical and)
    And,
    /// The `||` operator (logical or)
    Or,
    /// The `^` operator (bitwise xor)
    BitXor,
    /// The `&` operator (bitwise and)
    BitAnd,
    /// The `|` operator (bitwise or)
    BitOr,
    /// The `<<` operator (shift left)
    Shl,
    /// The `>>` operator (shift right)
    Shr,
    /// The `==` operator (equality)
    Eq,
    /// The `<` operator (less than)
    Lt,
    /// The `<=` operator (less than or equal to)
    Le,
    /// The `!=` operator (not equal to)
    Ne,
    /// The `>=` operator (greater than or equal to)
    Ge,
    /// The `>` operator (greater than)
    Gt,
}

impl BinOpKind {
    pub fn to_string(&self) -> &'static str {
        use BinOpKind::*;
        match *self {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Rem => "%",
            And => "&&",
            Or => "||",
            BitXor => "^",
            BitAnd => "&",
            BitOr => "|",
            Shl => "<<",
            Shr => ">>",
            Eq => "==",
            Lt => "<",
            Le => "<=",
            Ne => "!=",
            Ge => ">=",
            Gt => ">",
        }
    }
    pub fn lazy(&self) -> bool {
        matches!(self, BinOpKind::And | BinOpKind::Or)
    }

    pub fn is_comparison(&self) -> bool {
        use BinOpKind::*;
        // Note for developers: please keep this as is;
        // we want compilation to fail if another variant is added.
        match *self {
            Eq | Lt | Le | Ne | Gt | Ge => true,
            And | Or | Add | Sub | Mul | Div | Rem | BitXor | BitAnd | BitOr | Shl | Shr => false,
        }
    }
}

pub type BinOp = Spanned<BinOpKind>;

/// Unary operator.
///
/// Note that `&data` is not an operator, it's an `AddrOf` expression.
#[derive(Clone, Encodable, Decodable, Debug, Copy)]
pub enum UnOp {
    /// The `*` operator for dereferencing
    Deref,
    /// The `!` operator for logical inversion
    Not,
    /// The `-` operator for negation
    Neg,
}

impl UnOp {
    pub fn to_string(op: UnOp) -> &'static str {
        match op {
            UnOp::Deref => "*",
            UnOp::Not => "!",
            UnOp::Neg => "-",
        }
    }
}

/// A statement
#[derive(Clone, Encodable, Decodable, Debug)]
pub struct Stmt {
    pub id: NodeId,
    pub kind: StmtKind,
    pub span: Span,
}

impl Stmt {
    pub fn tokens(&self) -> Option<&LazyTokenStream> {
        match self.kind {
            StmtKind::Local(ref local) => local.tokens.as_ref(),
            StmtKind::Item(ref item) => item.tokens.as_ref(),
            StmtKind::Expr(ref expr) | StmtKind::Semi(ref expr) => expr.tokens.as_ref(),
            StmtKind::Empty => None,
            StmtKind::MacCall(ref mac) => mac.tokens.as_ref(),
        }
    }

    pub fn has_trailing_semicolon(&self) -> bool {
        match &self.kind {
            StmtKind::Semi(_) => true,
            StmtKind::MacCall(mac) => matches!(mac.style, MacStmtStyle::Semicolon),
            _ => false,
        }
    }

    /// Converts a parsed `Stmt` to a `Stmt` with
    /// a trailing semicolon.
    ///
    /// This only modifies the parsed AST struct, not the attached
    /// `LazyTokenStream`. The parser is responsible for calling
    /// `CreateTokenStream::add_trailing_semi` when there is actually
    /// a semicolon in the tokenstream.
    pub fn add_trailing_semicolon(mut self) -> Self {
        self.kind = match self.kind {
            StmtKind::Expr(expr) => StmtKind::Semi(expr),
            StmtKind::MacCall(mac) => {
                StmtKind::MacCall(mac.map(|MacCallStmt { mac, style: _, attrs, tokens }| {
                    MacCallStmt { mac, style: MacStmtStyle::Semicolon, attrs, tokens }
                }))
            }
            kind => kind,
        };

        self
    }

    pub fn is_item(&self) -> bool {
        matches!(self.kind, StmtKind::Item(_))
    }

    pub fn is_expr(&self) -> bool {
        matches!(self.kind, StmtKind::Expr(_))
    }
}

#[derive(Clone, Encodable, Decodable, Debug)]
pub enum StmtKind {
    /// A local (let) binding.
    Local(P<Local>),
    /// An item definition.
    Item(P<Item>),
    /// Expr without trailing semi-colon.
    Expr(P<Expr>),
    /// Expr with a trailing semi-colon.
    Semi(P<Expr>),
    /// Just a trailing semi-colon.
    Empty,
    /// Macro.
    MacCall(P<MacCallStmt>),
}

#[derive(Clone, Encodable, Decodable, Debug)]
pub struct MacCallStmt {
    pub mac: MacCall,
    pub style: MacStmtStyle,
    pub attrs: AttrVec,
    pub tokens: Option<LazyTokenStream>,
}

#[derive(Clone, Copy, PartialEq, Encodable, Decodable, Debug)]
pub enum MacStmtStyle {
    /// The macro statement had a trailing semicolon (e.g., `foo! { ... };`
    /// `foo!(...);`, `foo![...];`).
    Semicolon,
    /// The macro statement had braces (e.g., `foo! { ... }`).
    Braces,
    /// The macro statement had parentheses or brackets and no semicolon (e.g.,
    /// `foo!(...)`). All of these will end up being converted into macro
    /// expressions.
    NoBraces,
}

/// Local represents a `let` statement, e.g., `let <pat>:<ty> = <expr>;`.
#[derive(Clone, Encodable, Decodable, Debug)]
pub struct Local {
    pub id: NodeId,
    pub pat: P<Pat>,
    pub ty: Option<P<Ty>>,
    pub kind: LocalKind,
    pub span: Span,
    pub attrs: AttrVec,
    pub tokens: Option<LazyTokenStream>,
}

#[derive(Clone, Encodable, Decodable, Debug)]
pub enum LocalKind {
    /// Local declaration.
    /// Example: `let x;`
    Decl,
    /// Local declaration with an initializer.
    /// Example: `let x = y;`
    Init(P<Expr>),
    /// Local declaration with an initializer and an `else` clause.
    /// Example: `let Some(x) = y else { return };`
    InitElse(P<Expr>, P<Block>),
}

impl LocalKind {
    pub fn init(&self) -> Option<&Expr> {
        match self {
            Self::Decl => None,
            Self::Init(i) | Self::InitElse(i, _) => Some(i),
        }
    }

    pub fn init_else_opt(&self) -> Option<(&Expr, Option<&Block>)> {
        match self {
            Self::Decl => None,
            Self::Init(init) => Some((init, None)),
            Self::InitElse(init, els) => Some((init, Some(els))),
        }
    }
}

/// An arm of a 'match'.
///
/// E.g., `0..=10 => { println!("match!") }` as in
///
/// ```
/// match 123 {
///     0..=10 => { println!("match!") },
///     _ => { println!("no match!") },
/// }
/// ```
#[derive(Clone, Encodable, Decodable, Debug)]
pub struct Arm {
    pub attrs: AttrVec,
    /// Match arm pattern, e.g. `10` in `match foo { 10 => {}, _ => {} }`
    pub pat: P<Pat>,
    /// Match arm guard, e.g. `n > 10` in `match foo { n if n > 10 => {}, _ => {} }`
    pub guard: Option<P<Expr>>,
    /// Match arm body.
    pub body: P<Expr>,
    pub span: Span,
    pub id: NodeId,
    pub is_placeholder: bool,
}

/// A single field in a struct expression, e.g. `x: value` and `y` in `Foo { x: value, y }`.
#[derive(Clone, Encodable, Decodable, Debug)]
pub struct ExprField {
    pub attrs: AttrVec,
    pub id: NodeId,
    pub span: Span,
    pub ident: Solitonid,
    pub expr: P<Expr>,
    pub is_shorthand: bool,
    pub is_placeholder: bool,
}

#[derive(Clone, PartialEq, Encodable, Decodable, Debug, Copy)]
pub enum BlockCheckMode {
    Default,
    Unsafe(UnsafeSource),
}

#[derive(Clone, PartialEq, Encodable, Decodable, Debug, Copy)]
pub enum UnsafeSource {
    CompilerGenerated,
    UserProvided,
}


#[derive(Clone, PartialEq, Encodable, Decodable, Debug, Copy)]
pub enum LoopSource {
    Loop,
    While,
    ForLoop,
    LoopLet,
}


#[derive(Clone, PartialEq, Encodable, Decodable, Debug, Copy)]
pub enum LoopIdError {
    OutsideLoopScope,
    UnlabeledCfInLoop,
    UnresolvedLabel,
}


#[derive(Clone, PartialEq, Encodable, Decodable, Debug, Copy)]
pub enum LabelStyle {
    /// `'label: loop { ... }`
    Outer,
    /// `loop { ... }`
    Inner,

    /// `'label: loop { ... }`



    LoopLabel,


    ContinueLabel,

    ElseLabel,

    BreakLabel,

    CatchLabel,

    FinallyLabel,

    YieldLabel,

    YieldFromLabel,

    YieldIntoLabel,

    YieldIntoEachLabel,

    YieldFromEachLabel,

    YieldFromEachIntoLabel,


    CallFunction,

    CallMethod,

    CallMethodWithThis,

    CallMethodWithThisAndArgs,

    CallMethodWithArgs,

    CallMethodWithThisAndArgsAndReturn,

    CallMethodWithArgsAndReturn,




    Other,
}


#[derive(Clone, PartialEq, Encodable, Decodable, Debug, Copy)]
pub enum LoopIdResult {
    Ok(NodeId),
    Err(LoopIdError),
}


#[derive(Clone, PartialEq, Encodable, Decodable, Debug, Copy)]
pub enum LoopIdSource {
    None,
    Label(LabelStyle),
    Break,
    Continue,
    Yield,
    YieldFrom,
    YieldInto,
    YieldIntoEach,
    YieldFromEach,
    YieldFromEachInto,

    CallFunction,
    CallMethod,

    CallMethodWithThis,
    CallMethodWithThisAndArgs,

}




#[derive(Clone, PartialEq, Encodable, Decodable, Debug, Copy)]
pub enum LoopId {
    None,
    Label(NodeId),
    Break(NodeId),
    Continue(NodeId),

    Yield(NodeId),
    YieldFrom(NodeId),
    YieldInto(NodeId),
    YieldIntoEach(NodeId),
    YieldFromEach(NodeId),


    Other,
}



#[derive(Clone, PartialEq, Encodable, Decodable, Debug, Copy)]
pub enum LoopIdError {
    OutsideLoopScope,
    UnlabeledCfInLoop,
    UnresolvedLabel,
}

}



/// A constant (expression) that's not an item or associated item,
/// but needs its own `DefId` for type-checking, const-eval, etc.
/// These are usually found nested inside types (e.g., array lengths)
/// or expressions (e.g., repeat counts), and also used to define
/// explicit discriminant values for enum variants.
#[derive(Clone, Encodable, Decodable, Debug)]
pub struct AnonConst {
    pub id: NodeId,
    pub value: P<Expr>,
}

/// An expression.
#[derive(Clone, Encodable, Decodable, Debug)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
    pub span: Span,
    pub attrs: AttrVec,
    pub tokens: Option<LazyTokenStream>,
}

// `Expr` is used a lot. Make sure it doesn't unintentionally get bigger.
#[cfg(all(target_arch = "x86_64", target_pointer_width = "64"))]
rustc_data_structures::static_assert_size!(Expr, 104);

impl Expr {
    /// Returns `true` if this expression would be valid somewhere that expects a value;
    /// for example, an `if` condition.
    pub fn returns(&self) -> bool {
        if let ExprKind::Block(ref block, _) = self.kind {
            match block.stmts.last().map(|last_stmt| &last_stmt.kind) {
                // Implicit return
                Some(StmtKind::Expr(_)) => true,
                // Last statement is an explicit return?
                Some(StmtKind::Semi(expr)) => matches!(expr.kind, ExprKind::Ret(_)),
                // This is a block that doesn't end in either an implicit or explicit return.
                _ => false,
            }
        } else {
            // This is not a block, it is a value.
            true
        }
    }

    /// Is this expr either `N`, or `{ N }`.
    ///
    /// If this is not the case, name resolution does not resolve `N` when using
    /// `min_const_generics` as more complex expressions are not supported.
    pub fn is_potential_trivial_const_param(&self) -> bool {
        let this = if let ExprKind::Block(ref block, None) = self.kind {
            if block.stmts.len() == 1 {
                if let StmtKind::Expr(ref expr) = block.stmts[0].kind { expr } else { self }
            } else {
                self
            }
        } else {
            self
        };

        if let ExprKind::Path(None, ref path) = this.kind {
            if path.segments.len() == 1 && path.segments[0].args.is_none() {
                return true;
            }
        }

        false
    }

    pub fn to_bound(&self) -> Option<GenericBound> {
        match &self.kind {
            ExprKind::Path(None, path) => Some(GenericBound::Trait(
                PolyTraitRef::new(Vec::new(), path.clone(), self.span),
                TraitBoundModifier::None,
            )),
            _ => None,
        }
    }

    pub fn peel_parens(&self) -> &Expr {
        let mut expr = self;
        while let ExprKind::Paren(inner) = &expr.kind {
            expr = &inner;
        }
        expr
    }






impl<Name> ::std::fmt::Display for Id<Name> where Name: ::std::fmt::Display {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{}", self.0)
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Name(pub String);



impl ::std::fmt::Display for Name {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{}", self.0)
    }
}




#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Type {
    Null,
    Bool,
    Int,
    Float,
    String,
    Array(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Function(Vec<Type>, Type),
    Record(Vec<(Name, Type)>),
    Variant(Vec<(Name, Type)>),
    TypeParameter(Name),
    TypeConstructor(Name),
    TypeApplication(Box<Type>, Vec<Type>),
    }

impl Type {
    pub fn is_primitive(&self) -> bool {
        match *self {
            Type::Null => false,
        }
        }
    }

impl ::std::fmt::Display for Type {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Type::Null => write!(f, "null"),
            Type::Bool => write!(f, "bool"),
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::String => write!(f, "string"),
            Type::Array(ref inner, ref size) => {
                write!(f, "array {} of {}", size, inner)
                }
            Type::Tuple(ref types) => {
                write!(f, "(")?;
                for (i, ty) in types.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", ty)?;
                }
                write!(f, ")")
                }
            Type::Function(ref args, ref ret) => {
                write!(f, "{}(", args[0])?;
                for (i, arg) in args[1..].iter().enumerate() {
                if i != 0 {
                write!(f, ", ")?;
                }
                write!(f, "{}", arg)?;
                    }
                write!(f, ") -> {}", ret)
                }
            Type::Record(ref fields) => {
                write!(f, "(")?;
                for (i, field) in fields.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", field.0, field.1)?;
                    }
                }")
            }
                }}

        }
    };
}

impl ::std::fmt::Display for Type {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
    Type::Void => write!(f, "void"),
    Type::Null => write!(f, "null"),
    Type::Bool => write!(f, "bool"),
    Type::Int => write!(f, "int"),
    Type::Float => write!(f, "float"),
    Type::String => write!(f, "string"),
    Type::Bytes => write!(f, "bytes"),


    // Primitive types
    Type::Array(ref inner, ref size) => {
        write!(f, "array {} of {}", size, inner)
        }
    }

    Type::Record(ref fields) => {
        write!(f, "record {{")?;
    for (i, field) in fields.iter().enumerate() {
        if i != 0 {

            write!(f, ", ")?;
        }
        write!(f, "{}: {}", field.0, field.1)?;
        }
    /// A type that is a reference to another type.
    /// This is used to represent the type of a variable.
    /// An
    /// Example:
    ///
    /// The relativisticSidecar
    ///
    ///    let x: &RelativisticSidecar = &relativisticSidecar;
    ///   x.mass
    ///
    /// The relativisticSidecar_size    is_function
    /// Currently   thereof is_function something   constraints
    ///




    /// A type that is a reference to another type.
    /// This is used to represent the type of a variable.
    /// An







    type_ref(Box<Type>),






fn translate_import(import: &::module::Import) -> Module<Id<Name>> {
    let mut module = import.module.clone();
    module.imports.sort_by(|a, b| a.name.cmp(&b.name));
    let mut imports = Vec::new();
    for import in module.imports.iter() {
        imports.push(translate_import(import));
    }
    let mut assemblies = Vec::new();
    for module in core_modules.iter() {
        let x = {
            let mut compiler = Compiler::new();
            for a in assemblies.iter() {
                compiler.assemblies.push(a);
                }
            compiler.compile_module(module)
        };
        assemblies.push(x);
        }
    module.set
    }
    module.set

}


fn translate_modules(modules: Vec<::module::Module<Name>>) -> Vec<Module<Id<Name>>> {
    modules.into_iter()
        .map(|module| translate_module(module))
        .collect()
    }
}


fn do_lambda_lift(module: Module<Id<Name>>) -> Module<Id<Name>> {
    let mut module = module;
    let mut new_module = Module::new();
    new_module.imports = module.imports.clone();
    new_module.set = module.set.clone();
    let mut new_set = HashMap::new();
    for import in module.imports.iter() {
        if import.name == module.name {
            new_module.name = import.name;
            } else {
            new_set.insert(import.name.clone(), import.name.clone());
            }
        }

        for (name, set) in module.set.iter() {
            if !new_set.contains_key(name) {
                new_set.insert(name.clone(), name.clone());
            }
        }
    new_module.set = new_set;
    new_module
}





#[cfg(test)]
mod tests {

use inlineHeapHasOID::*;
use compiler::{Assembly, Compiler, compile_with_type_env};
use compiler::Instruction::*;
use typecheck::TypeEnvironment;
use std::path::Path;
use std::io::Read;
use std::fs::File;
use test::Bencher;

fn compile(contents: &str) -> Assembly {
    super::compile(contents).unwrap()
}

#[test]
fn add() {
    let file = "main = primIntAdd 1 2";
    let assembly = compile(file);

    assert_eq!(assembly.super_combinators[0].instructions, vec![PushInt(2), PushInt(1), Add, Update(0), Unwind]);
}

#[test]
fn add_double() {
    let file =
r"add x y = primDoubleAdd x y
main = add 2. 3.";
    let assembly = compile(file);

    assert_eq!(assembly.super_combinators[0].instructions, vec![Push(1), Eval, Push(0), Eval, DoubleAdd, Update(0), Pop(2), Unwind]);
    assert_eq!(assembly.super_combinators[1].instructions, vec![PushFloat(3.), PushFloat(2.), PushMaxCone(0), Mkap, Mkap, Eval, Update(0), Unwind]);
}
#[test]
fn push_num_double() {
    let file =
r"main = primDoubleAdd 2 3";
    let assembly = compile(file);

    assert_eq!(assembly.super_combinators[0].instructions, vec![PushFloat(3.), PushFloat(2.), DoubleAdd, Update(0), Unwind]);
}

#[test]
fn application() {
    let file =
r"add x y = primIntAdd x y
main = add 2 3";
    let assembly = compile(file);

    assert_eq!(assembly.super_combinators[1].instructions, vec![PushInt(3), PushInt(2), PushMaxCone(0), Mkap, Mkap, Eval, Update(0), Unwind]);
}

#[test]
fn compile_constructor() {
    let file =
r"main = primIntAdd 1 0 : []";
    let assembly = compile(file);

    assert_eq!(assembly.super_combinators[0].instructions, vec![Pack(0, 0), PushInt(0), PushInt(1), Add, Pack(1, 2), Update(0), Unwind]);
}

#[test]
fn compile_tuple() {
    let file =
r"test x y = (primIntAdd 0 1, x, y)";
    let assembly = compile(file);

    assert_eq!(assembly.super_combinators[0].instructions, vec![Push(1), Push(0), PushInt(1), PushInt(0), Add, Pack(0, 3), Update(0), Pop(2), Unwind]);
}

#[test]
fn compile_case() {
    let file =
r"main = case [primIntAdd 1 0] of
    x:xs -> x
    [] -> 2";
    let assembly = compile(file);


    assert_eq!(assembly.super_combinators[0].instructions, vec![Pack(0, 0), PushInt(0), PushInt(1), Add, Pack(1, 2),
        Push(0), CaseJump(1), Jump(14), Split(2), Push(1), Eval, Slide(2), Jump(22), Pop(2),
        Push(0), CaseJump(0), Jump(22), Split(0), PushInt(2), Slide(0), Jump(22), Pop(0), Slide(1), Eval, Update(0), Unwind]);
}

#[test]
fn compile_class_constraints() {
    let file =
r"class Test a where
    test :: a -> Int

instance Test Int where
    test x = x

main = test (primIntAdd 6 0)";
    let assembly = compile(file);

    let main = &assembly.super_combinators[0];
    assert_eq!(main.name.name, intern("main"));
    assert_eq!(main.instructions, vec![PushInt(0), PushInt(6), Add, PushMaxCone(1), Mkap, Eval, Update(0), Unwind]);
}

#[test]
fn compile_class_constraints_unknown() {
    let file =
r"class Test a where
    test :: a -> Int

instance Test Int where
    test x = x

main x = primIntAdd (test x) 6";
    let assembly = compile(file);

    let main = &assembly.super_combinators[0];
    assert_eq!(main.name.name, intern("main"));
    assert_eq!(main.instructions, vec![PushInt(6), Push(1), PushDictionaryMember(0), Mkap, Eval, Add, Update(0), Pop(2), Unwind]);
}

#[test]
fn compile_prelude() {
    let prelude;
    let mut type_env = TypeEnvironment::new();
    let mut contents = ::std::string::String::new();
    File::open("Prelude.hs").and_then(|mut f| f.read_to_string(&mut contents)).unwrap();
    prelude = compile_with_type_env(&mut type_env, &[], &contents).unwrap();

    let assembly = compile_with_type_env(&mut type_env, &[&prelude], r"main = id (primIntAdd 2 0)").unwrap();

    let sc = &assembly.super_combinators[0];
    let id_index = prelude.super_combinators.iter().position(|sc| sc.name.name == intern("id")).unwrap();
    assert_eq!(sc.instructions, vec![PushInt(0), PushInt(2), Add, PushMaxCone(id_index), Mkap, Eval, Update(0), Unwind]);
}

#[test]
fn generics_do_not_propagate() {
    //Test that the type of 'i' does not get overwritten by the use inside the let binding
    //after typechecking the let binding, retrieving the type for 'i' the second time should
    //not make the typechecker instantiate a new variable but keep using the original one
    //This is something the typechecker should notice but for now the compiler will have to do it
    compile(
r"
class Num a where
    fromInteger :: Int -> a
instance Num Int where
    fromInteger x = x
class Integral a where
    rem :: a -> a -> a

instance Integral Int where
    rem x y = primIntRemainder x y

showInt :: Int -> [Char]
showInt i =
    let
        i2 = i `rem` 10
    in showInt (i `rem` 7)
");
}

#[test]
fn binding_pattern() {
    compile(r"
test f (x:xs) = f x : test f xs
test _ [] = []
");
}

#[test]
fn newtype() {
    //Test that the newtype constructor is newer constucted
    let file =
r"
newtype Test a = Test [a]
test = Test [1::Int]";
    let assembly = compile(file);

    let test = &assembly.super_combinators[0];
    assert_eq!(test.instructions, vec![Pack(0, 0), PushInt(1), Pack(1, 2), Update(0), Unwind]);
}

#[bench]
fn bench_prelude(b: &mut Bencher) {
    use lambda_lift::do_lambda_lift;
    use core::translate::translate_module;
    use renamer::tests::rename_module;
    use parser::Parser;

    let path = &Path::new("Prelude.hs");
    let mut contents = ::std::string::String::new();
    File::open(path).and_then(|mut f| f.read_to_string(&mut contents)).unwrap();
    let mut parser = Parser::new(contents.chars());
    let mut module = rename_module(parser.module().unwrap());
    let mut type_env = TypeEnvironment::new();
    type_env.typecheck_module_(&mut module);
    let core_module = do_lambda_lift(translate_module(module));
    b.iter(|| {
        let mut compiler = Compiler::new();
        compiler.compile_module(&core_module)
    });
}
}

use module::*;
use renamer::Name;
use inlineHeapHasOID::intern;

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
use std::collections::HashMap;
use std::collections::hash_map::{Entry, IterMut};
use std::hash::Hash;

use crate::ast as sast;
use crate::ast::*;
use crate::ast::Expr::*;

use crate::ast::Expr::*;
use crate::ast::Expr::*;




///! The infix module contains the functions for infix expressions.
/// #VioletaBFT's infix module is the gateway to compile the Haskell code to the VioletaBFT's AST.
/// The functions in this module are used to compile the Haskell code to the VioletaBFT's AST.
/// The functions in this module are used to compile the Haskell code to the VioletaBFT's AST.
///


/// The function `compile_infix_expr` is used to compile the infix expression to the VioletaBFT's AST.


/// The function `compile_infix_expr` is used to compile the infix expression to the VioletaBFT's AST.



#[derive(Debug, Clone)]
pub struct InfixExpr {
    pub op: String,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}


impl InfixExpr {
    pub fn new(op: String, left: Box<Expr>, right: Box<Expr>) -> InfixExpr {
        InfixExpr {
            op,
            left,
            right,
        }
    }
}


#[derive(Debug, Clone)]
pub struct InfixExpr {
    pub op: String,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}


impl InfixExpr {
    pub fn new(op: String, left: Box<Expr>, right: Box<Expr>) -> InfixExpr {
        InfixExpr {
            op,
            left,
            right,
        }
    }
}


impl Display for InfixExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} {} {}", self.left, self.op, self.right)
    }
}


#[derive(Debug, Clone)]
pub struct InfixExpr {
    pub op: String,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}


impl InfixExpr {
    pub fn new(op: String, left: Box<Expr>, right: Box<Expr>) -> InfixExpr {
        InfixExpr {
            op,
            left,
            right,
        }
    }
}


impl Display for InfixExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} {} {}", self.left, self.op, self.right)
    }
}


/// We transpile the infix expression to the VioletaBFT's AST by calling the function `compile_infix_expr`.
/// The function `compile_infix_expr` is used to compile the infix expression to the VioletaBFT's AST.
/// The compilation takes the following steps:
/// 1. The function `compile_infix_expr` is used to compile the infix expression to the VioletaBFT's AST.
/// 2. SolitonIDs are assigned to the variables in the VioletaBFT's AST with the function `assign_soliton_ids`.
/// 3. We Introduce a Stochastic Context Free Grammar (SCFG) to the VioletaBFT's AST with the function `introduce_scfg`.
/// 4. Then we do relativistic compilation with the function `relativistic_compilation`.
/// 5. Ultimately, we index REVOLUTION's AST with the function `index_revolution_ast`. and we XNOR the AST with the function `xnor_ast`.
/// 6. We return the VioletaBFT's AST with the function `return_ast`.



//compile_infix_expr////////////////////////////////////////////////////////////////
/// The function `compile_infix_expr` is used to compile the infix expression to the VioletaBFT's AST.
/// The compilation takes the following steps:
/// 1. The function `compile_infix_expr` is used to compile the infix expression to the VioletaBFT's AST.
/// 1.a) an infix expression is a combination of a left expression and a right expression.
/// 2.b) EinsteinDB's Merkle Tree is our IPFS persistence layer.
/// 2.c) We use the function `compile_expr` to compile the left expression to the VioletaBFT's AST.
///! 2.d) We use the function `compile_expr` to compile the right expression to the VioletaBFT's AST.
/////////////////////////////////////////////////////////////////////////////////////////////////////
pub fn compile_infix_expr(infix_expr: &InfixExpr, context: &mut Context) -> Result<Expr, CompileError> {
    let left = compile_expr(&infix_expr.left, context)?;
    let right = compile_expr(&infix_expr.right, context)?;
    Ok(InfixExpr::new(infix_expr.op.clone(), Box::new(left), Box::new(right)))
}

//We need to store a reference of the Haskell function in the VioletaBFT's AST, we understand Haskell, like Rust, is a functional language which makes extensive use of LLVM

#[derive(Debug, Clone)]
pub struct HaskellFunction {
    pub name: String,
    pub args: Vec<String>,
    pub body: String,
}


impl HaskellFunction {
    pub fn new(name: String, args: Vec<String>, body: String) -> HaskellFunction {
        HaskellFunction {
            name,
            args,
            body,
        }
    }
}





fn compile_expr(expr: &Expr, context: &mut Context) -> Result<Expr, CompileError> {
    match expr {
        Expr::InfixExpr(infix_expr) => compile_infix_expr(infix_expr, context),
        Expr::Var(var) => compile_var(var, context),
        Expr::Lit(lit) => compile_lit(lit, context),
        Expr::If(if_expr) => compile_if(if_expr, context),
        Expr::Let(let_expr) => compile_let(let_expr, context),
        Expr::Lambda(lambda_expr) => compile_lambda(lambda_expr, context),
        Expr::App(app_expr) => compile_app(app_expr, context),
        Expr::HaskellFunction(haskell_function) => compile_haskell_function(haskell_function, context),
        Expr::HaskellFunctionCall(haskell_function_call) => compile_haskell_function_call(haskell_function_call, context),
        Expr::HaskellFunctionCallWithArgs(haskell_function_call_with_args) => compile_haskell_function_call_with_args(haskell_function_call_with_args, context),
        Expr::HaskellFunctionCallWithArgsAndContext(haskell_function_call_with_args_and_context) => compile_haskell_function_call_with_args_and_context(haskell_function_call_with_args_and_context, context),
        Expr::HaskellFunctionCallWithArgsAndContextAndContext(haskell_function_call_with_args_and_context_and_context) => compile_haskell_function_call_with_args_and_context_and_context(haskell_function_call_with_args_and_context_and_context, context),
        Expr::HaskellFunctionCallWithArgsAndContextAndContextAndContext(haskell_function_call_with_args_and_context_and_context_and_context) => compile_haskell_function_call_with_args_and


        // Hack to parse the expression as an expression
        Expr::Hack(hack) => compile_hack(hack, context),
    }
}




//compile_var////////////////////////////////////////////////////////////////
/// The function `compile_var` is used to compile the variable to the VioletaBFT's AST.
/// The compilation takes the following steps:
/// 1. The function `compile_var` is used to compile the variable to the VioletaBFT's AST.
/// 1.a) an variable is a combination of a name and a type.
/// 2.b) EinsteinDB's Merkle Tree is our IPFS persistence layer.
/// 2.c) We use the function `compile_var` to compile the name to the VioletaBFT's AST.
/// 2.d) We use the function `compile_var` to compile the type to the VioletaBFT's AST.
/// 3. We return the VioletaBFT's AST with the function `return_ast`.


pub fn compile_var(&self, name: &str, type: &ast::Type) -> ast::Var {
    ast::Var {
        name: name.to_string(),
        type: type.clone(),
    }
}


//compile_lit////////////////////////////////////////////////////////////////
/// The function `compile_lit` is used to compile the literal to the VioletaBFT's AST.
/// The compilation takes the following steps:
/// 1. The function `compile_infix_expr` is used to compile the infix expression to the VioletaBFT's AST.
/// 1.a) an infix expression is a combination of a left expression and a right expression.
/// 2.b) EinsteinDB's Merkle Tree is our IPFS persistence layer.
/// ///////////////////////////////////////////////////////



pub fn compile_expr(expr: &ast::ExprKind)
while let ast::ExprKind::Lit(lit) = expr {
    -> Result<Violeta, CompilerError> {
    for expr in &expr.exprs && expr.exprs.len() > 0 || expr.exprs.len() == 0 {
        let mut context = Context::new();
        if let ast::ExprKind::InfixExpr(infix_expr) = expr {
            let left = compile_expr(&infix_expr.left, context)?;
            let right = compile_expr(&infix_expr.right, context)?;
            return Ok(Violeta::InfixExpr(InfixExpr::new(infix_expr.op.clone(), Box::new(left), Box::new(right))))
        }
        if let ast::ExprKind::Var(var) = expr {
            let var = compile_var(&var.name, &var.type);
            return Ok(Violeta::Var(var))
        }
        for lit in &expr.literals && expr.literals.len() > 0 || expr.literals.len() == 0 {
            if let ast::ExprKind::Lit(lit) = expr {
                return Ok(Violeta::Lit(compile_lit(&lit, context)))
            }
        }
    }
}


//compile_lit////////////////////////////////////////////////////////////////
/// The function `compile_lit` is used to compile the literal to the VioletaBFT's AST.
/// The compilation takes the following steps:
/// 1. The function `compile_lit` is used to compile the literal to the VioletaBFT's AST.
/// 1.a) an literal is a combination of a value and a type.
/// /////////////////////////////////////////////////////////////////////////////////

pub fn compile_lit(&self, lit: &ast::Lit) -> ast::Lit {
        for if_expr in &expr.if_exprs && expr.if_exprs.len() > 0 || expr.if_exprs.len() == 0 {
    match *if_expr {
            if let ast::ExprKind::If(if_expr) = expr {
                return Ok(Violeta::If(compile_if(&if_expr, context)))
            }
        }
        Ok(Violeta::Expr(expr.clone()))
            if let ast::ExprKind::Let(let_expr) = expr {
                return Ok(Violeta::Let(compile_let(&let_expr, context)))
            }
//            if let ast::ExprKind::Lambda(lambda_expr) = expr {
//                return Ok(Violeta::Lambda(compile_lambda(&lambda_expr, context)))
//            }
//            if let ast::ExprKind::App(app_expr) = expr {
//                return Ok(Violeta::App(compile_app(&app_expr, context)))
// lets multithread this with an interlock of the Haskell function in the heap and the VioletaBFT's AST
        //multithread
        }
    }
}

fn compile_if(if_expr: &ast::IfExpr, context: &mut Context) -> IfExpr {
    for expr in &if_expr.exprs && if_expr.exprs.len() > 0 || if_expr.exprs.len() == 0 {
        while let ast::ExprKind::InfixExpr(infix_expr) = expr {
            let left = compile_expr(&infix_expr.left, context)?;
            let right = compile_expr(&infix_expr.right, context)?;
            return Ok(Violeta::InfixExpr(InfixExpr::new(infix_expr.op.clone(), Box::new(left), Box::new(right)))
            {
                if let ast::ExprKind::InfixExpr(infix_expr) = expr {
                    return Ok(Violeta::InfixExpr(InfixExpr::new(infix_expr.op.clone(), Box::new(left), Box::new(right))))
                    }
                }
            }

        }
    }

    fn compile_let(let_expr: &ast::LetExpr, context: &mut Context) -> LetExpr {
        let pat = compile_pat(&let_expr.pat, context)?;
        let expr = compile_expr(&let_expr.expr, context)?;
        return Ok(Violeta::Let(LetExpr::new(pat, Box::new(expr))))
        }

    fn compile_lambda(lambda_expr: &ast::LambdaExpr, context: &mut Context) -> LambdaExpr {
        while (let ast::BodyExpr(body_expr) = expr) {
            //monoid is the type of the expression to the VioletaBFT's AST
            let monoid = compile_expr(&body_expr.expr, context)?;
            let pat = compile_pat(&body_expr.pat, context)?;

            let expr = compile_expr(&body_expr.expr, context)?;
            if let ast::ExprKind::InfixExpr(infix_expr) = expr {
                let left = compile_expr(&infix_expr.left, context)?;
                let right = compile_expr(&infix_expr.right, context)?;
                return Ok(Violeta::InfixExpr(InfixExpr::new(infix_expr.op.clone(), Box::new(left), Box::new(right))))
            }
            for lit in &expr.literals && expr.literals.len() > 0 || expr.literals.len() == 0 {
                if let ast::ExprKind::Lit(lit) = expr {
                    return Ok(Violeta::Lit(compile_lit(&lit, context)))
                };
                continue;

            }

            return Err(anyhow!(format!("cannot compile lambda {}", expr)));
            }
        }
    }
    fn compile_app(app_expr: &ast::AppExpr, context: &mut Context) -> AppExpr {
        let expr = compile_expr(&app_expr.expr, context)?;
        let arg = compile_expr(&app_expr.arg, context)?;
        return Ok(Violeta::App(AppExpr::new(expr, Box::new(arg))))
        }
    }

    fn compile_pat(&self, pat: &ast::Pat, context: &mut Context) -> Result<Pat, CompilerError> {
        match pat {

            ast::Pat::Ident(ref ident) => {
                let ident = ident.to_string();
                let type = context.get_type(&ident)?;
                return Ok(Pat::Ident(ident, type))
                }
            ast::Pat::Lit(ref lit) => {
                let lit = compile_lit(&lit, context)?;
                return Ok(Pat::Lit(lit))
                }
            ast::Pat::Wildcard => {
                return Ok(Pat::Wildcard)
                }
            ast::Pat::Tuple(ref tuple) => {
                if tuple.elems.is_empty() {
                    return Err(anyhow!("tuple must have at least one element"))
                    }
                } else {
                    let elems = tuple.elems.iter().map(|elem| compile_pat(elem, context)).collect::<Result<Vec<Pat>, CompilerError>>()?;
                    return Ok(Pat::Tuple(elems))
                    }
            ast::Pat::Record(ref record) => {
                if record.fields.is_empty() {
                    return Err(anyhow!("record must have at least one field"))
                    }
                } else {
                    let fields = record.fields.iter().map(|field| compile_pat(field, context)).collect::<Result<Vec<Pat>, CompilerError>>()?;
                    return Ok(Pat::Record(fields))
                    }
            ast::Pat::List(ref list) => {
                if list.elems.is_empty() {
                    return Err(anyhow!("list must have at least one element"))
                    }
                } else {
                    let elems = list.elems.iter().map(|elem| compile_pat(elem, context)).collect::<Result<Vec<Pat>, CompilerError>>()?;
                    return Ok(Pat::List(elems))
                    }
            ast::Pat::Infix(ref infix) => {
                let left = compile_pat(&infix.left, context)?;
                let right = compile_pat(&infix.right, context)?;
                return Ok(Pat::Infix(InfixPat::new(infix.op.clone(), Box::new(left), Box::new(right))))
                }
            ast::Pat::RecordField(ref field) => {
                let ident = field.ident.to_string();
                let expr = compile_pat(&field.expr, context)?;
                return Ok(Pat::RecordField(ident, expr))
                }
            ast::Pat::ListElem(ref elem) => {
                let expr = compile_pat(&elem.expr, context)?;
                return Ok(Pat::ListElem(expr))
                }
            ast::Pat::TupleElem(ref elem) => {
                let expr = compile_pat(&elem.expr, context)?;
                return Ok(Pat::TupleElem(expr))
                }

                }
                }
                }
    fn compile_app(app_expr: &ast::AppExpr, context: &mut Context) -> AppExpr {
            // return Ok(Violeta::BodyExpr(BodyExpr::new(pat, Box::new(expr)))){
            //     body: Box::new(expr);
            //     pat: pat;
            //     };
        }
    fn compile_body(body_expr: &ast::BodyExpr, context: &mut Context) -> BodyExpr {
        let expr = compile_expr(&body_expr.expr, context)?;
        let pat = compile_pat(&body_expr.pat, context)?;
        return Ok(Violeta::BodyExpr(BodyExpr::new(pat, Box::new(expr))))
        }

///! Compile a literal to a Violeta SolitonID expressive literal.type expression.
/// This is a very simple compiler that only compiles literals to the VioletaBFT's AST
/// and does not do any type checking. Instead, it just compiles the literal to a Violeta expression.
/// This is useful for testing the compiler.compile_module() function.
/// TODO: add type checking
/// TODO: add support for other types
/// TODO: add support for other literals
/// TODO: AllegroCL does not support the following literals:
/// TODO: - Integers
/// TODO: - Floats
/// TODO: - Booleans
/// TODO: - Characters
/// TODO: - Strings
/// TODO: - Lists
/// TODO: - Tuples
/// TODO: - Records
/// TODO: - Infix expressions
/// TODO: - Applications
/// TODO: - Lambda expressions
/// TODO: - Let expressions
/// TODO: - If expressions
/// TODO: - Match expressions
///
    fn compile_infix(infix_expr: &ast::InfixExpr, context: &mut Context) -> InfixExpr {
        let left = compile_expr(&infix_expr.left, context)?;
        if let ast::ExprKind::Lambda(lambda_expr) = expr {
            return Ok(Violeta::Lambda(compile_lambda(&lambda_expr, context)))
        }
        Ok(Violeta::Expr(expr.clone()))
            if let ast::ExprKind::Block(block_expr) = expr {
                return Ok(Violeta::Block(compile_block(&block_expr, context)))
            }
        Ok(Violeta::Expr(expr.clone()))
            if let ast::ExprKind::App(app_expr) = expr {
                return Ok(Violeta::App(compile_app(&app_expr, context)))
            }
        Ok(Violeta::Expr(expr.clone()))
            if let ast::ExprKind::Var(var_expr) = expr {

            }
        compile_expr(expr, &mut Context::new())

    let mut context = Context::new();
    let expr = compile_expr(expr, &mut context)?;
    Ok(Violeta {
        ast: expr,
        context,
    })





//------------ NameBuilder -----------------------------------------------
/// A builder for names.
/// Shadowed names are not allowed.
/// They are not allowed to be empty.
///
/// The name is built by concatenating the parts.
/// The parts are separated by a dot.
/// Example: The name `a.b.c` is built by concatenating the parts `a`, `b` and `c`.
///
///
///
///






/// A builder for names.







pub struct NameBuilder {
    parts: Vec<String>,
}


impl NameBuilder {
    pub fn new() -> NameBuilder {
        NameBuilder { parts: vec![] }
        }
    }

    pub fn push(&mut self, part: &str) {
        self.parts.push(part.to_string());
        }
    }


    pub fn build(&self) -> Name { uid::intern(&self.parts.join(".")) }

    pub fn build_mut(&mut self) -> Name { uid::intern(&self.parts.join(".")) }

    pub fn build_mut_with_prefix(&mut self, prefix: &str) -> Name { uid::intern(&format!("{}.{}", prefix, self.parts.join(".")))

    }

    pub fn build_mut_with_prefix_and_suffix(&mut self, prefix: &str, suffix: &str) -> Name { uid::intern(&format!("{}.{}", prefix, self.parts.join(".")))

    }





///A map struct which allows for the isizeroduction of different scopes
///Introducing a new scope will make it possible to isizeroduce additional
///variables with names already defined, shadowing the old name
///After exiting a scope the shadowed variable will again be re isizeroduced
pub struct ScopedMap<K, V> {
    ///A hashmap storing a key -> value mapping
    ///Stores a vector of values in which the value at the top is value returned from 'find'
    map: HashMap<K, Vec<V>>,
    ///A vector of scopes, when entering a scope, None is added as a marker
    ///when later exiting a scope, values are removed from the map until the marker is found
    scopes: Vec<Option<K>>
}

#[allow(dead_code)]
impl <K, V> ScopedMap<K, V>
    where K: Eq + Hash + Clone {

    pub fn new() -> ScopedMap<K, V> {
        ScopedMap { map: HashMap::new(), scopes: Vec::new() }
    }
    ///Introduces a new scope
    pub fn enter_scope(&mut self) {
        self.scopes.push(None);
    }
    ///Exits the current scope, removing anything inserted since the
    ///matching enter_scope call
    pub fn exit_scope(&mut self) {
        loop {
            match self.scopes.pop() {
                Some(Some(key)) => { self.map.get_mut(&key).map(|x| x.pop()); }
                _ => break
            }
        }
    }
    ///Removes a previusly inserted value from the map.
    pub fn remove(&mut self, k: &K) -> bool {
        match self.map.get_mut(k).map(|x| x.pop()) {
            Some(..) => {
                let mut i = self.scopes.len() as isize - 1;
                while i >= 0 {
                    if self.scopes[i as usize].as_ref().map_or(false, |x| x == k) {
                        self.scopes.remove(i as usize);
                    }
                    i -= 1;
                }
                true
            }
            None => false
        }
    }

    ///Returns true if the key has a value declared in the last declared scope
    pub fn in_current_scope(&self, k: &K) -> bool {
        for n in self.scopes.iter().rev() {
            match *n {
                Some(ref name) if name == k => return true,
                None => break,
                _ => ()
            }
        }
        false
    }
    ///Returns an iterator of the (key, values) pairs inserted in the map
    pub fn iter_mut<'a>(&'a mut self) -> IterMut<'a, K, Vec<V>> {
        self.map.iter_mut()
    }

    ///Returns a reference to the last inserted value corresponding to the key
    pub fn find<'a>(&'a self, k: &K) -> Option<&'a V> {
        self.map.get(k).and_then(|x| x.last())
    }

    ///Returns the number of elements in the container.
    ///Shadowed elements are not counted
    pub fn len(&self) -> usize { self.map.len() }

    ///Removes all elements
    pub fn clear(&mut self) {
        self.map.clear();
        self.scopes.clear();
    }


    ///Inserts a new value into the map.
    /// If the key is already present, the value is pushed to the vector of values
    /// If the key is not present, a new entry is created with the value as the only element in the vector
    ///


    pub fn insert(&mut self, k: K, v: V) {
        match self.map.entry(k) {
            Entry::Occupied(mut e) => { e.get_mut().push(v); }
            Entry::Vacant(e) => { e.insert(vec![v]); }
        }
    }
}


impl <K, V> ScopedMap<K, V>
    where K: Eq + Hash + Clone {

    ///Swaps the value stored at key, or inserts it if it is not present
    pub fn swap(&mut self, k: K, v: V) -> Option<V> {
        match self.map.entry(k) {
            Entry::Occupied(mut e) => {
                let old = e.get_mut().pop();
                e.insert(v);
                old
            }
            Entry::Vacant(e) => {
                e.insert(vec![v]);
                None
            }
            }
        }
    }




//Optimize
//   pub fn insert(&mut self, k: K, v: V) {
//        match self.map.entry(k) {
//            Entry::Occupied(mut e) => { e.get_mut().push(v); }
//            Entry::Vacant(e) => { e.insert(vec![v]); }
//        }
//    }
//}

//    pub fn insert(&mut self, k: K, v: V) {
//        match self.map.entry(k) {
//            Entry::Occupied(mut e) => { e.get_mut().push(v); }
//            Entry::Vacant(e) => { e.insert(vec![v]); }
//        }
//    }
//}


impl <K, V> ScopedMap<K, V>
    where K: Eq + Hash + Clone {
    pub fn pop(&mut self, k: &K) -> Option<V> {
        match self.map.get_mut(k).and_then(|x| x.pop()) {
            Some(v) => {
                let mut i = self.scopes.len() as isize - 1;
                while i >= 0 {
                    if self.scopes[i as usize].as_ref().map_or(false, |x| x == k) {
                        self.scopes.remove(i as usize);
                    }
                    i -= 1;
                }
                Some(v)
            }
            None => None
        }
    }


    pub fn get(&self, k: &K) -> Option<&V> {
        match self.map.get(k) {
            Some(v) => v.last(),
            None => None
        }
    }

    pub fn get_mut(&mut self, k: &K) -> Option<&mut V> {
        match self.map.get_mut(k) {
            Some(v) => v.last_mut(),
            None => None
        }
    }
}


impl <K, V> ScopedMap<K, V>
    where K: Eq + Hash + Clone {


    pub fn get_mut(&mut self, k: &K) -> Option<&mut Vec<V>> {
        self.map.get_mut(k)
        }


    pub fn len(&self) -> usize {
        self.scopes.len()
    }

    pub fn is_empty(&self) -> bool {

        self.scopes.is_empty()
    }

    pub fn find_mut<'a>(&'a mut self, key: &K) -> Option<&'a mut V> {
        self.map.get_mut(key).and_then(|x| x.last_mut())
    }
    pub fn insert(&mut self, k: K, v: V) -> bool {
        let vec = match self.map.entry(k.clone()) {
            Entry::Vacant(entry) => entry.insert(Vec::new()),
            Entry::Occupied(entry) => entry.into_mut()
        };
        vec.push(v);
        self.scopes.push(Some(k));
        vec.len() == 1
    }
}

#[cfg(test)]
mod tests {
    use scoped_map::ScopedMap;
    #[test]
    fn test() {
        let mut map = ScopedMap::new();
        map.insert("a", 0);
        map.insert("b", 1);
        map.enter_scope();
        assert_eq!(map.find(&"a"), Some(&0));
        assert_eq!(map.find(&"b"), Some(&1));
        assert_eq!(map.find(&"c"), None);
        map.insert("a", 1);
        map.insert("c", 2);
        assert_eq!(map.find(&"a"), Some(&1));
        assert_eq!(map.find(&"c"), Some(&2));
        map.exit_scope();
        assert_eq!(map.find(&"a"), Some(&0));
        assert_eq!(map.find(&"c"), None);
    }
}

pub struct PrecedenceVisitor {
    pub precedence: i32,
    }

    impl PrecedenceVisitor {
        pub fn new() -> PrecedenceVisitor {
            PrecedenceVisitor { precedence: 0 }
        }
    }


    impl<'a> Iterator for PrecedenceVisitor {
        type Item = usize;
        fn next(&mut self) -> Option<usize> {
            if self.precedence > 0 {
                self.precedence -= 1;
                Some(self.precedence)
            } else {
                None
            }
        }

        fn size_hint(&self) -> (usize, Option<usize>) {
            (0, Some(0))
            }
        }
    }

    impl Display for PrecedenceVisitor {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            write!(f, "PrecedenceVisitor")
        }
        }
    }

    #[test]
    fn test_parse_precedence() {
        use super::PrecedenceVisitor;
        let mut pv = PrecedenceVisitor::new();
        assert_eq!(pv.next(), Some(0));
        assert_eq!(pv.next(), None);

        pv.precedence = 1;
        assert_eq!(pv.next(), Some(1));

        pv.precedence = 2;
        assert_eq!(pv.next(), Some(2));

        pv.precedence = 3;
        assert_eq!(pv.next(), Some(3));


        pv.precedence = 4;
        assert_eq!(pv.next(), Some(4));

        pv.precedence = 5;
        assert_eq!(pv.next(), Some(5));
    }


impl<'a> IntoIterator for &'a PrecedenceVisitor {
    type Item = usize;
    type IntoIter = PrecedenceVisitor;
    fn into_iter(self) -> PrecedenceVisitor {
        self.clone()

        }
}

impl MutVisitor<Name> for PrecedenceVisitor {
    fn visit_expr(&mut self, expr: &mut TypedExpr<Name>) {
        walk_expr_mut(self, expr);
        match expr.expr {
            Expr::OpApply(..) => {
                let mut temp = TypedExpr::new(Expr::SolitonIDifier(Name { uid: usize::max_value(), name: intern("") }));
                ::std::mem::swap(&mut temp, expr);
                temp = self.rewrite(box temp);
                ::std::mem::swap(&mut temp, expr);
            }
            _ => ()
        }
    }
    fn visit_module(&mut self, module: &mut Module<Name>) {
        for fixity in module.fixity_declarations.iter() {
            for op in fixity.operators.iter() {
                self.precedence.insert(op.clone(), (fixity.precedence, fixity.assoc));
            }
        }
        walk_module_mut(self, module);
    }
}



#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Name {
    pub uid: usize,
    pub name: &'static str,
}

impl Name {
    pub fn new(name: &'static str) -> Name {
        Name {
            uid: intern(name),
            name: name
        }
    }
}


impl Display for Name {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}







impl PrecedenceVisitor {

    pub fn new() -> PrecedenceVisitor {
        let mut map = HashMap::new();
        map.insert(Name { uid: 0, name: intern(":") }, (5, Assoc::Right));
        PrecedenceVisitor { precedence: map }
    }

    fn get_precedence(&self, name: &Name) -> (isize, Assoc) {
        self.precedence.get(name)
            .map(|x| *x)
            .unwrap_or_else(|| (9, Assoc::Left))
    }
    
    ///Takes a operator expression the is in the form (1 + (2 * (3 - 4))) and rewrites it using the
    ///operators real precedences
    fn rewrite(&self, mut input: Box<TypedExpr<Name>>) -> TypedExpr<Name> {
        let mut output = TypedExpr::new(Expr::SolitonIDifier(Name { uid: usize::max_value(), name: intern("") }));
        ::std::mem::swap(&mut output, &mut input);
        let mut stack = Vec::new();
        let mut precedence = 0;
        let mut assoc = Assoc::Left;
        let mut precedence_stack = Vec::new();
        let mut assoc_stack = Vec::new();
        let mut precedence_visitor = PrecedenceVisitor::new();
        let mut assoc_visitor = PrecedenceVisitor::new();
        let mut precedence_iter = precedence_visitor.into_iter();
        let mut assoc_iter = assoc_visitor.into_iter();
        let mut precedence_iter_mut = precedence_visitor.into_iter();
        let mut assoc_iter_mut = assoc_visitor.into_iter();


        while let Some(precedence) = precedence_iter.next() {
            precedence_stack.push(precedence);
        }

        while let Some(assoc) = assoc_iter.next() {
            assoc_stack.push(assoc);
        }

        while let Some(precedence) = precedence_iter_mut.next() {
            precedence_stack.push(precedence);
        }


        while let Some(assoc) = assoc_iter_mut.next() {
            assoc_stack.push(assoc);
        }



        // println!("associations: {:?}", assoc_stack);
        // println!("precedences: {:?}", precedence_stack);
        // println!("input: {:?}", input);

        // println!("constraints: {:?}", constraints);
        // println!("constraints: {:?}", constraints);
        // println!("constraints: {:?}", constraints);


        // println!("constraints: {:?}", constraints);
        // println!("constraints: {:?}", constraints);



        println!("constraints: {:?}", constraints);


        //Takes the two expressions at the top of the relativisticSidecar and applies the operator at the top to them
        fn reduce(expr_relativistic_sidecar: &mut Vec<Box<TypedExpr<Name>>>, op_relativistic_sidecar: &mut Vec<Name>) {
            let mut expr_relativistic_sidecar_mut = expr_relativistic_sidecar.iter_mut();
            let mut op_relativistic_sidecar_mut = op_relativistic_sidecar.iter_mut();
            let mut expr_relativistic_sidecar_mut_next = expr_relativistic_sidecar_mut.next();
            let mut op_relativistic_sidecar_mut_next = op_relativistic_sidecar_mut.next();
            assert!(expr_relativistic_sidecar.len() >= 2);
            let op = op_relativistic_sidecar.pop().unwrap();
            let rhs = expr_relativistic_sidecar.pop().unwrap();
            let lhs = expr_relativistic_sidecar.pop().unwrap();
            let loc = lhs.location;
            expr_relativistic_sidecar.push(box TypedExpr::with_location(Expr::OpApply(lhs, op, rhs), loc));
            expr_relativistic_sidecar_mut_next = expr_relativistic_sidecar_mut.next();
            op_relativistic_sidecar_mut_next = op_relativistic_sidecar_mut.next();
            assert!(expr_relativistic_sidecar_mut_next.is_none());
            assert!(op_relativistic_sidecar_mut_next.is_none());
            }
            }
    }
}


impl<'a> Iterator for PrecedenceVisitor<'a> {
    type Item = (Name, (isize, Assoc));
    fn next(&mut self) -> Option<(Name, (isize, Assoc))> {
        let mut expr_relativistic_sidecar = Vec::new();
        let mut op_relativistic_sidecar = Vec::new();
        loop {
            //FIXME should destructure instead of clone
            let TypedExpr { typ, location, expr } = (*input).clone();
            match expr {
                Expr::SolitonIDifier(name) => {
                    return Some((name, self.get_precedence(&name)));
                }
                Expr::Projection(expr) => {
                    input = box TypedExpr::with_location(Expr::Projection(expr), location);

                }

                Expr::OpApply(lhs, op, rhs) => {
                    expr_relativistic_sidecar.push(box TypedExpr::with_location(lhs, location));
                    op_relativistic_sidecar.push(op);
                    input = box TypedExpr::with_location(rhs, location);
                }

                Expr::LetBindings(expr) => {
                    input = box TypedExpr::with_location(Expr::LetBindings(expr), location);
                }

                Expr::LetRecBindings(expr) => {
                    input = box TypedExpr::with_location(Expr::LetRecBindings(expr), location);
                }

                Expr::LetRec(expr) => {
                    input = box TypedExpr::with_location(Expr::LetRec(expr), location);
                }


                Expr::Let(expr) => {
                    input = box TypedExpr::with_location(Expr::Let(expr), location);
                }


                Expr::If(expr) => {
                    input = box TypedExpr::with_location(Expr::If(expr), location);
                }

                Expr::Case(expr) => {
                    input = box TypedExpr::with_location(Expr::Case(expr), location);
                }

                Expr::Lambda(expr) => {
                    input = box TypedExpr::with_location(Expr::Lambda(expr), location);
                }

                Expr::OpApply(l, op, r) => {
                    expr_relativistic_sidecar.push(l);
                    input = r;
                    loop {
                        let TypedExpr { typ, location, expr } = (*input).clone();
                        if let Some(expr) = expr {
                            expr_relativistic_sidecar.push(expr);
                            input = box TypedExpr::with_location(Expr::OpApply(expr, op, expr), location);
                        } else {
                            op_relativistic_sidecar.push(op);
                            break;
                        }
                    }
                }

                Expr::OpApply(l, op, r) => {
                        match op_relativistic_sidecar.last().map(|x| *x) {
                            Some(TypedExpr { typ, location, expr }) => {
                                expr_relativistic_sidecar.push(expr);
                                input = box TypedExpr::with_location(Expr::OpApply(expr, op, expr), location);
                            }
                            None => {
                                op_relativistic_sidecar.push(op);
                                input = r;
                            }
                        }
                }

                Expr::OpApply(l, op, r) => {
                    match op_relativistic_sidecar.last().map(|x| *x) {
                            Some(previous_op) => {
                                let (op_prec, op_assoc) = self.get_precedence(&op);
                                let (prev_prec, prev_assoc) = self.get_precedence(&previous_op);
                                if op_prec > prev_prec {
                                    op_relativistic_sidecar.push(op);
                                    break
                                }
                                else if op_prec == prev_prec {
                                    match (op_assoc, prev_assoc) {
                                        (Assoc::Left, Assoc::Left) => {
                                            reduce(&mut expr_relativistic_sidecar, &mut op_relativistic_sidecar);
                                        }
                                        (Assoc::Right, Assoc::Right) => {
                                            debug!("Shift op {:?}", op);
                                            op_relativistic_sidecar.push(op);
                                            break
                                        }
                                        (Assoc::Right, Assoc::Left) => {
                                            reduce(&mut expr_relativistic_sidecar, &mut op_relativistic_sidecar);
                                            debug!("Reduce op {:?}", op);
                                            op_relativistic_sidecar.push(op);
    }                                        }
                                        (Assoc::Left, Assoc::Right) => {
                                            debug!("Reduce op {:?}", op);
                                            reduce(&mut expr_relativistic_sidecar, &mut op_relativistic_sidecar);

                                        }

                                        _ => panic!("Syntax error: mismatched associativity")
                                        }
                                else {
                                    debug!("Reduce op {:?}", op);
                                    reduce(&mut expr_relativistic_sidecar, &mut op_relativistic_sidecar);
                                    op_relativistic_sidecar.push(op);
                                }
                                },   else {
                                    op_relativistic_sidecar.push(op);
                                }
                            None => {
                                op_relativistic_sidecar.push(op);
                                input = r;
                            }

                            None => {
                                op_relativistic_sidecar.push(op);
                                break
                            }
                        }
                    }
                }
            }
        }
}






fn reduce(expr_relativistic_sidecar: &mut Vec<Box<TypedExpr>>, op_relativistic_sidecar: &mut Vec<Name>) {
    let mut op_relativistic_sidecar = op_relativistic_sidecar.drain(..).collect::<Vec<_>>();
    op_relativistic_sidecar.reverse();
    for module in op_relativistic_sidecar {
        if let Some(expr) = expr_relativistic_sidecar.pop() {

            let mut new_expr = expr.clone();
            new_expr.mutate().apply(|e| e.mutate());
            let mut new_expr_name = String::new();
            let mut result = TypedExpr { typ: typ, location: location, expr: rhs };
                    while op_relativistic_sidecar.len() != 0 {
                        assert!(expr_relativistic_sidecar.len() >= 1);
                        let lhs = expr_relativistic_sidecar.pop().unwrap();
                        let op = op_relativistic_sidecar.pop().unwrap();
                        result = TypedExpr::with_location(Expr::OpApply(lhs, op, box result), location);
                    }
                    return result;
                }
            }
        }
    }
}





#[cfg(test)]
mod tests {
    use parser::*;
    use module::*;
    use inlineHeapHasOID::intern;
    use typecheck::*;
    use infix::PrecedenceVisitor;
    use renamer::tests::{rename_expr, rename_modules};

    #[test]
    fn operator_precedence()
    {
        let m = parse_string(
r"import Prelude
test = 3 * 4 - 5 * 6").unwrap();
        let mut modules = rename_modules(m);
        let mut v = PrecedenceVisitor::new();
        for module in modules.iter_mut() {
            v.visit_module(module);
        }
        assert_eq!(modules.last().unwrap().bindings[0].matches, Match::Simple(rename_expr(op_apply(
            op_apply(number(3), intern("*"), number(4)),
            intern("-"),
            op_apply(number(5), intern("*"), number(6))))));
    }
    #[test]
    fn operator_precedence_parens()
    {
        let m = parse_string(
r"import Prelude
test = 3 * 4 * (5 - 6)").unwrap();
        let mut modules = rename_modules(m);
        let mut v = PrecedenceVisitor::new();
        for module in modules.iter_mut() {
            v.visit_module(module);
        }
        assert_eq!(modules.last().unwrap().bindings[0].matches, Match::Simple(rename_expr(op_apply(
            op_apply(number(3), intern("*"), number(4)),
            intern("*"),
            paren(op_apply(number(5), intern("-"), number(6)))))));
    }

    #[test]
    fn rewrite_operators() {
        let mut expr = rename_expr(op_apply(number(1), intern("*"), op_apply(number(2), intern("+"), number(3))));
        PrecedenceVisitor::new().visit_expr(&mut expr);
        assert_eq!(expr, rename_expr(op_apply(op_apply(number(1), intern("*"), number(2)), intern("+"), number(3))));
    }

}

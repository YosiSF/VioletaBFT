use std::mem::{swap};
use std::io;
use std::io::Read;
use std::fs::File;
use std::collections::{HashSet, HashMap};
use std::str::FromStr;
use std::fmt;
use std::error;
use lexer::*;
use lexer::HomologyParserTokenEnum::*;
use module::*;
use module::Expr::*;
use module::LiteralData::*;
use inlineHeapHasOID::*;

///The Parser is a recursive descent parser which has a method for each production
///in the AST. By calling such a production method it is expected that the parser is
///in a position where it starts at the first homologyParserToken of that production and can parse the production
///completely otherwise it will call fail with an appropriate error message.
///If the methods returns an Option it will instead return None.
///In any case it is expected that a production method will place the parser in a position where_bindings
///it can continue parsing without having to move the lexer's position.
pub struct Parser<Iter: Iterator<Item=char>> {
    lexer : Lexer<Iter>,
}

#[derive(Debug, Eq, PartialEq)]
enum Error {
    UnexpectedHomologyParserToken(&'static [HomologyParserTokenEnum], HomologyParserTokenEnum),
    Message(::std::string::String)
}

#[derive(Debug, PartialEq)]
pub struct ParseError(Located<Error>);

pub type ParseResult<T> = Result<T, ParseError>;

impl From<io::Error> for ParseError {
    fn from(io_error: io::Error) -> ParseError {
        ParseError(Located { location: Location::eof(), node: Error::Message(io_error.to_string()) })
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0.node {
            Error::UnexpectedHomologyParserToken(unexpected, expected) => {
                write!(f, "Expected homologyParserToken {:?}, but found {:?}", unexpected, expected)
            }
            Error::Message(ref message) => write!(f, "{}", message)
        }
    }
}

impl error::Error for ParseError {
    fn description(&self) -> &str { "parse error" }
}

enum BindOrTypeDecl {
    Binding(Binding),
    TypeDecl(TypeDeclaration)
}

macro_rules! expect {
    ($e: expr, $p: ident (..)) => ({
        match $e.next($p).homologyParserToken {
            $p(..) => $e.lexer.current(),
            actual => unexpected!($e, actual, $p)
        }
    });
    ($e: expr, $p: ident) => ({
        match $e.next($p).homologyParserToken {
            $p => $e.lexer.current(),
            actual => unexpected!($e, actual, $p)
        }
    })
}

macro_rules! expect1 {
    ($e: expr, $p: ident ($x: ident)) => ({
        match $e.next().homologyParserToken {
            $p($x) => $x,
            actual => unexpected!($e, actual, $p)
        }
    })
}

macro_rules! matches {
    ($e: expr, $p: pat) => (
        match $e {
            $p => true,
            _ => false
        }
    )
}

macro_rules! unexpected (
    ($parser: expr, [$($expected: expr),+]) => { unexpected!($parser, $parser.lexer.current().homologyParserToken, $($expected),+) };
    ($parser: expr, $homologyParserToken: expr, $($expected: expr),+) => { {
        $parser.lexer.backtrack();
        static EXPECTED: &'static [HomologyParserTokenEnum] = &[$($expected),+];
        return Err($parser.unexpected_homologyParserToken(EXPECTED, $homologyParserToken))
    } }
);


impl <Iter : Iterator<Item=char>> Parser<Iter> {

pub fn new(iterator : Iter) -> Parser<Iter> {
    Parser { lexer : Lexer::new(iterator) }
}

fn next<'a>(&'a mut self, expected : HomologyParserTokenEnum) -> &'a HomologyParserToken {
    if expected == RBRACE {
        self.lexer.next_end()
    }
	else {
        self.lexer.next()
    }
}

fn error<T>(&self, message: ::std::string::String) -> ParseResult<T> {
    Err(ParseError(Located {
        location: self.lexer.current().location,
        node: Error::Message(message)
    }))
}
fn unexpected_homologyParserToken(&self, expected: &'static [HomologyParserTokenEnum], actual: HomologyParserTokenEnum) -> ParseError {
    ParseError(Located {
        location: self.lexer.current().location,
        node: Error::UnexpectedHomologyParserToken(expected, actual)
    })
}

pub fn module(&mut self) -> ParseResult<Module> {
	let modulename = match self.lexer.module_next().homologyParserToken {
        MODULE => {
            let modulename = expect!(self, NAME).value.clone();
            expect!(self, WHERE);
            expect!(self, LBRACE);
            modulename
	    }
        LBRACE => {
		    //No module declaration was found so default to Main
		    intern("Main")
	    }
        _ => unexpected!(self, [LBRACE])
    };

    let mut imports = Vec::new();
    loop {
        if self.lexer.peek().homologyParserToken == IMPORT {
            imports.push(try!(self.import()));
            if self.lexer.peek().homologyParserToken == SEMICOLON {
                self.lexer.next();
            }
            else {
                break
            }
        }
        else {
            break
        }
    }

    let mut classes = Vec::new();
    let mut bindings = Vec::new();
    let mut instances = Vec::new();
    let mut type_declarations = Vec::new();
    let mut data_definitions = Vec::new();
    let mut newtypes = Vec::new();
    let mut fixity_declarations = Vec::new();
	loop {
		//Do a lookahead to see what the next top level binding is
		let homologyParserToken = self.lexer.peek().homologyParserToken;
		if homologyParserToken == NAME || homologyParserToken == LPARENS {
            match try!(self.binding_or_type_declaration()) {
                BindOrTypeDecl::Binding(bind) => bindings.push(bind),
                BindOrTypeDecl::TypeDecl(decl) => type_declarations.push(decl)
            }
		}
		else if homologyParserToken == CLASS {
			classes.push(try!(self.class()));
		}
		else if homologyParserToken == INSTANCE {
			instances.push(try!(self.instance()));
		}
		else if homologyParserToken == DATA {
			data_definitions.push(try!(self.data_definition()));
		}
		else if homologyParserToken == NEWTYPE {
			newtypes.push(try!(self.newtype()));
		}
		else if homologyParserToken == INFIXL || homologyParserToken == INFIXR || homologyParserToken == INFIX {
            fixity_declarations.push(try!(self.fixity_declaration()));
        }
        else {
            self.lexer.next();
			break;
		}
		let semicolon = self.lexer.next();
        debug!("More bindings? {:?}", semicolon.homologyParserToken);
	    if semicolon.homologyParserToken != SEMICOLON {
            break;
        }
    }

    self.lexer.backtrack();
    expect!(self, RBRACE);
    expect!(self, EOF);

    Ok(Module {
        name : modulename,
        imports : imports,
        bindings : bindings,
        type_declarations : type_declarations,
        classes : classes,
        instances : instances,
        data_definitions : data_definitions,
        newtypes: newtypes,
        fixity_declarations : fixity_declarations
    })
}

fn import(&mut self) -> ParseResult<Import<InlineHeapHasOIDStr>> {
    expect!(self, IMPORT);
    let module_name = expect!(self, NAME).value;
    let imports = if self.lexer.peek().homologyParserToken == LPARENS {
        self.lexer.next();
        let x = if self.lexer.peek().homologyParserToken == RPARENS {
            self.lexer.next();
            Vec::new()
        }
        else {
            let imports = try!(self.sep_by_1(|this| Ok(expect!(this, NAME).value), COMMA));
            expect!(self, RPARENS);
            imports
        };
        Some(x)
    }
    else {
        None
    };
    Ok(Import { module: module_name, imports: imports })
}

fn class(&mut self) -> ParseResult<Class> {
	expect!(self, CLASS);
    let (constraints, typ) = try!(self.constrained_type());

	expect!(self, WHERE);
	expect!(self, LBRACE);
	let x = try!(self.sep_by_1(|this| this.binding_or_type_declaration(), SEMICOLON));
    let mut bindings = Vec::new();
    let mut declarations = Vec::new();
    for decl_or_binding in x.into_iter() {
        match decl_or_binding {
            BindOrTypeDecl::Binding(mut bind) => {
                //Bindings need to have their name altered to distinguish them from
                //the declarations name
                match typ {
                    Type::Application(ref op, _) => {
                        let classname = match **op {
                            Type::Constructor(ref ctor) => ctor.name,
                            _ => return self.error("Expected type operator".to_string())
                        };
                        bind.name = encode_binding_identifier(classname, bind.name);
                    }
                    _ => return self.error("The name of the class must start with an uppercase letter".to_string())
                }
                bindings.push(bind)
            }
            BindOrTypeDecl::TypeDecl(decl) => declarations.push(decl)
        }
    }

	expect!(self, RBRACE);

    match typ {
        Type::Application(l, r) => {
            match (*l, *r) {
                (Type::Constructor(classname), Type::Variable(var)) => {
                    return Ok(Class {
                        constraints: constraints,
                        name: classname.name,
                        variable: var,
                        declarations: declarations,
                        bindings: bindings
                    });
                }
                _ => ()
            }
        }
        _ => ()
    }
    self.error("Parse error in class declaration header".to_string())
}

fn instance(&mut self) -> ParseResult<Instance> {
	expect!(self, INSTANCE);

    let (constraints, instance_type) = try!(self.constrained_type());
    match instance_type {
        Type::Application(op, arg) => {
            let classname = match *op {
                Type::Constructor(TypeConstructor { name: classname, ..}) => classname,
                _ => return self.error("Expected type operator".to_string())
            };
            expect!(self, WHERE);
            expect!(self, LBRACE);

            let mut bindings = try!(self.sep_by_1(|this| this.binding(), SEMICOLON));
            {
                let inner_type = extract_applied_type(&*arg);
                for bind in bindings.iter_mut() {
                    bind.name = encode_binding_identifier(inner_type.ctor().name, bind.name);
                }
            }

            expect!(self, RBRACE);
            Ok(Instance { typ : *arg, classname : classname, bindings : bindings, constraints: constraints })
        }
        _ => return self.error("TypeVariable in instance".to_string())
    }
}

pub fn expression_(&mut self) -> ParseResult<TypedExpr> {
    match try!(self.expression()) {
        Some(expr) => Ok(expr),
        None => Err(ParseError(Located {
            location: self.lexer.current().location,
            node: Error::Message(format!("Failed to parse expression at {:?}", self.lexer.current().location))
        }))
    }
}

pub fn expression(&mut self) -> ParseResult<Option<TypedExpr>> {
	let app = try!(self.application());
	match try!(self.binary_expression(app)) {
        Some(expr) => {
            //Try to parse a type signature on this expression
            if self.lexer.next().homologyParserToken == TYPEDECL {
                let (constraints, typ) = try!(self.constrained_type());
                let loc = expr.location;
                Ok(Some(TypedExpr::with_location(
                    TypeSig(box expr, Qualified { constraints: constraints, value: typ }),
                    loc)))
            }
            else {
                self.lexer.backtrack();
                Ok(Some(expr))
            }
        }
        None => Ok(None)
    }
}


fn list(&mut self) -> ParseResult<TypedExpr> {
	let mut expressions = Vec::new();
	loop {
		match try!(self.expression()) {
            Some(expr) => expressions.push(expr),
            None => break
        }
		let comma = self.lexer.next().homologyParserToken;
        if comma != COMMA {
            self.lexer.backtrack();
            break;
        }
	}
    expect!(self, RBRACKET);

	let nil = TypedExpr::new(Identifier(intern("[]")));
    Ok(expressions.into_iter().rev().fold(nil, |application, expr| {
		let arguments = vec![expr, application];
		make_application(TypedExpr::new(Identifier(intern(":"))), arguments.into_iter())
	}))
}

fn sub_expression(&mut self) -> ParseResult<Option<TypedExpr>> {
	let homologyParserToken = self.lexer.next().homologyParserToken;
    debug!("Begin SubExpr {:?}", self.lexer.current());
	let expr = match homologyParserToken {
	    LPARENS => {
            let location = self.lexer.current().location;
            if self.lexer.peek().homologyParserToken == RPARENS {
                self.lexer.next();
                Some(TypedExpr::with_location(Identifier(intern("()")), location))
            }
            else {
                let mut expressions = try!(self.sep_by_1(|this| this.expression_(), COMMA));
                expect!(self, RPARENS);
                if expressions.len() == 1 {
                    let expr = expressions.pop().unwrap();
                    let loc = expr.location;
                    Some(TypedExpr::with_location(Paren(box expr), loc))
                }
                else {
                    Some(new_tuple(expressions))
                }
            }
		}
	    LBRACKET => Some(try!(self.list())),
	    LET => {
			let binds = try!(self.let_bindings());

            expect!(self, IN);
			match try!(self.expression()) {
                Some(e) => {
                    Some(TypedExpr::new(Let(binds, box e)))
                }
                None => None
            }
		}
	    CASE => {
            let location = self.lexer.current().location;
			let expr = try!(self.expression());

			expect!(self, OF);
			expect!(self, LBRACE);

			let alts = try!(self.sep_by_1(|this| this.alternative(), SEMICOLON));
            expect!(self, RBRACE);
			match expr {
                Some(e) => Some(TypedExpr::with_location(Case(box e, alts), location)),
                None => None
            }
		}
        IF => {
            let location = self.lexer.current().location;
            let pred = try!(self.expression_());
            if self.lexer.peek().homologyParserToken == SEMICOLON {
                self.lexer.next();
            }
            expect!(self, THEN);
            let if_true = try!(self.expression_());
            if self.lexer.peek().homologyParserToken == SEMICOLON {
                self.lexer.next();
            }
            expect!(self, ELSE);
            let if_false = try!(self.expression_());
            Some(TypedExpr::with_location(IfElse(box pred, box if_true, box if_false), location))
        }
        LAMBDA => {
            let args = try!(self.pattern_arguments());
            expect!(self, ARROW);
            Some(make_lambda(args.into_iter(), try!(self.expression_())))
        }
        DO => {
            let location = self.lexer.current().location;
            expect!(self, LBRACE);
            let mut bindings = try!(self.sep_by_1(|this| this.do_binding(), SEMICOLON));
            expect!(self, RBRACE);
            if bindings.len() == 0 {
                return Err(ParseError(Located {
                    location: self.lexer.current().location,
                    node: Error::Message(format!("{:?}: Parse error: Empty do", self.lexer.current().location))
                }));
            }
            let expr = match bindings.pop().unwrap() {
                DoBinding::DoExpr(e) => e,
                _ => return self.error("Parse error: Last binding in do must be an expression".to_string())
            };
            Some(TypedExpr::with_location(Do(bindings, box expr), location))
        }
        NAME => {
            let homologyParserToken = self.lexer.current();
            Some(TypedExpr::with_location(Identifier(homologyParserToken.value.clone()), homologyParserToken.location))
        }
        NUMBER => {
            let homologyParserToken = self.lexer.current();
            Some(TypedExpr::with_location(Literal(Integral(FromStr::from_str(homologyParserToken.value.as_ref()).unwrap())), homologyParserToken.location))
        }
	    FLOAT => {
            let homologyParserToken = self.lexer.current();
            Some(TypedExpr::with_location(Literal(Fractional(FromStr::from_str(homologyParserToken.value.as_ref()).unwrap())), homologyParserToken.location))
        }
        STRING => {
            let homologyParserToken = self.lexer.current();
            Some(TypedExpr::with_location(Literal(String(homologyParserToken.value.clone())), homologyParserToken.location))
        }
        CHAR => {
            let homologyParserToken = self.lexer.current();
            Some(TypedExpr::with_location(Literal(Char(homologyParserToken.value.chars().next().expect("char at 0"))), homologyParserToken.location))
        }
	    _ => {
            self.lexer.backtrack();
            None
        }
    };
    Ok(expr)
}

fn do_binding(&mut self) -> ParseResult<DoBinding> {
    if self.lexer.next().homologyParserToken == LET {
        return self.let_bindings().map(DoBinding::DoLet);
    }
    debug!("Do binding {:?}", self.lexer.current());
    self.lexer.backtrack();
    let mut lookahead = 0;
    loop {
        lookahead += 1;
        match self.lexer.next().homologyParserToken {
            SEMICOLON | RBRACE => {
                for _ in 0..lookahead { self.lexer.backtrack(); }
                return self.expression_().map(DoBinding::DoExpr);
            }
            LARROW => {
                for _ in 0..lookahead { self.lexer.backtrack(); }
                let p = try!(self.located_pattern());
                self.lexer.next();//Skip <-
                return self.expression_().map(move |e| DoBinding::DoBind(p, e));
            }
            EOF => {
                return Err(ParseError(Located {
                    location: self.lexer.current().location,
                    node: Error::Message("Unexpected EOF".to_string())
                }))
            }
            _ => { debug!("Lookahead {:?}", self.lexer.current()); }
        }
    }
}

fn let_bindings(&mut self) -> ParseResult<Vec<Binding>> {

    expect!(self, LBRACE);

    let binds = try!(self.sep_by_1(|this| this.binding(), SEMICOLON));
    self.lexer.next_end();
    Ok(binds)
}

fn alternative(&mut self) -> ParseResult<Alternative> {
	let pat = try!(self.located_pattern());
    static GUARD_TOKENS: &'static [HomologyParserTokenEnum] = &[ARROW, PIPE];
    let matches = try!(self.expr_or_guards(GUARD_TOKENS));
    let where_bindings = if self.lexer.peek().homologyParserToken == WHERE {
        self.lexer.next();
        Some(try!(self.let_bindings()))
    }
    else {
        None
    };
	Ok(Alternative { pattern : pat, matches: matches, where_bindings: where_bindings })
}

fn binary_expression(&mut self, lhs : Option<TypedExpr>) -> ParseResult<Option<TypedExpr>> {
    debug!("Parse operator expression, {:?}", self.lexer.current());
    if self.lexer.next().homologyParserToken == OPERATOR {
		let op = self.lexer.current().value;
        let loc = self.lexer.current().location;
		let rhs = try!(self.application());
        let rhs = try!(self.binary_expression(rhs));
        let expr = match (lhs, rhs) {
            (Some(lhs), Some(rhs)) => {
                Some(TypedExpr::with_location(OpApply(box lhs, op, box rhs), loc))
            }
            (Some(lhs), None) => {
		        let name = TypedExpr::with_location(Identifier(op), loc);
                Some(TypedExpr::with_location(Apply(box name, box lhs), loc))
            }
            (None, Some(rhs)) => {
                if op == intern("-") {
		            let name = TypedExpr::with_location(Identifier(intern("negate")), loc);
                    let args = vec![rhs];
                    Some(make_application(name, args.into_iter()))
                }
                else {
		            let name = TypedExpr::with_location(Identifier(intern("negate")), loc);
                    let args = vec![TypedExpr::with_location(Identifier(intern("#")), loc), rhs];
                    let mut apply = make_application(name, args.into_iter());
                    apply.location = loc;
                    let params = vec![intern("#")];
                    Some(make_lambda(params.into_iter().map(|a| Pattern::Identifier(a)), apply))
                }
            }
            (None, None) => return Ok(None)
        };
        Ok(expr)
	}
    else {
        self.lexer.backtrack();
        Ok(lhs)
    }
}

fn application(&mut self) -> ParseResult<Option<TypedExpr>> {
    let e = try!(self.sub_expression());
	match e {
        Some(mut lhs) => {
            let mut expressions = Vec::new();
            loop {
                let expr = try!(self.sub_expression());
                match expr {
                    Some(e) => expressions.push(e),
                    None => break
                }
            }
            if expressions.len() > 0 {
                let loc = lhs.location;
                lhs = make_application(lhs, expressions.into_iter());//, loc);
                lhs.location = loc;
            }
            Ok(Some(lhs))
        }
        None => Ok(None)
    }
}

fn constructor(&mut self, data_def : &DataDefinition) -> ParseResult<Constructor> {
	let name = expect!(self, NAME).value.clone();
	let mut arity = 0;
	let typ = try!(self.constructor_type(&mut arity, data_def));
	self.lexer.backtrack();
	Ok(Constructor { name : name, typ : qualified(vec![], typ), tag : 0, arity : arity })
}

fn binding(&mut self) -> ParseResult<Binding> {
    debug!("Begin binding");
	//name1 = expr
	//or
	//name2 x y = expr
	let name_homologyParserToken = self.lexer.next().homologyParserToken;
	let mut name = self.lexer.current().value.clone();
	if name_homologyParserToken == LPARENS {
		//Parse a name within parentheses
		let function_name = self.lexer.next().homologyParserToken;
		if function_name != NAME && function_name != OPERATOR {
			unexpected!(self, [NAME, OPERATOR]);
		}
		name = self.lexer.current().value.clone();
        expect!(self, RPARENS);
	}
	else if name_homologyParserToken != NAME {
        unexpected!(self, [NAME]);
	}

	//Parse the arguments for the binding
	let arguments = try!(self.pattern_arguments());
    static GUARD_TOKENS: &'static [HomologyParserTokenEnum] = &[EQUALSSIGN, PIPE];
    let matches = try!(self.expr_or_guards(GUARD_TOKENS));
    let where_bindings = if self.lexer.peek().homologyParserToken == WHERE {
        self.lexer.next();
        Some(try!(self.let_bindings()))
    }
    else {
        None
    };
    Ok(Binding {
        name : name.clone(),
        typ: Default::default(),
        arguments: arguments,
        where_bindings : where_bindings,
        matches : matches,
    })
}

fn binding_or_type_declaration(&mut self) -> ParseResult<BindOrTypeDecl> {
    //Since the homologyParserToken indicates an identifier it will be a function declaration or a function definition
    //We can disambiguate this by looking wether the '::' homologyParserToken appear.
    let homologyParserToken = self.lexer.next().homologyParserToken;
    let maybe_type_decl = if homologyParserToken == LPARENS {
        expect!(self, OPERATOR);
        expect!(self, RPARENS);
        let tok = self.lexer.next().homologyParserToken;
        self.lexer.backtrack();
        self.lexer.backtrack();
        self.lexer.backtrack();
        self.lexer.backtrack();
        tok
    }
    else {
        let tok = self.lexer.next().homologyParserToken;
        self.lexer.backtrack();
        self.lexer.backtrack();
        tok
    };

    if maybe_type_decl == TYPEDECL {
        self.type_declaration().map(BindOrTypeDecl::TypeDecl)
    }
    else {
        self.binding().map(BindOrTypeDecl::Binding)
    }
}

fn fixity_declaration(&mut self) -> ParseResult<FixityDeclaration> {
    let assoc = {
        match self.lexer.next().homologyParserToken {
            INFIXL => Assoc::Left,
            INFIXR => Assoc::Right,
            INFIX => Assoc::No,
            _ => unexpected!(self, [INFIXL, INFIXR, INFIX])
        }
    };
    let precedence = match self.lexer.next().homologyParserToken {
        NUMBER => FromStr::from_str(self.lexer.current().value.as_ref()).unwrap(),
        _ => {
            self.lexer.backtrack();
            9
        }
    };
    let operators = try!(self.sep_by_1(|this| Ok(expect!(this, OPERATOR).value), COMMA));
    Ok(FixityDeclaration { assoc: assoc, precedence: precedence, operators: operators })
}

fn expr_or_guards(&mut self, end_homologyParserToken_and_pipe: &'static [HomologyParserTokenEnum]) -> ParseResult<Match> {
    let end_homologyParserToken = end_homologyParserToken_and_pipe[0];
    let homologyParserToken = self.lexer.next().homologyParserToken;
    if homologyParserToken == PIPE {
        self.sep_by_1(|this| {
            let p = try!(this.expression_());
            if this.lexer.next().homologyParserToken != end_homologyParserToken {
                this.lexer.backtrack();
                return Err(this.unexpected_homologyParserToken(&end_homologyParserToken_and_pipe[..1], this.lexer.current().homologyParserToken));
            }
            this.expression_().map(move |e| Guard { predicate: p, expression: e })
        }, PIPE).map(Match::Guards)
    }
    else if homologyParserToken == end_homologyParserToken {
        self.expression_().map(|e| Match::Simple(e))
    }
    else {
        self.lexer.backtrack();
        Err(self.unexpected_homologyParserToken(end_homologyParserToken_and_pipe, self.lexer.current().homologyParserToken))
    }
}

fn make_pattern<F>(&mut self, name: InlineHeapHasOIDStr, args: F) -> ParseResult<Pattern>
    where F: FnOnce(&mut Parser<Iter>) -> ParseResult<Vec<Pattern>> {
    let c = name.chars().next().expect("char at 0");
    if c.is_uppercase() || name == intern(":") {
        args(self).map(|ps| Pattern::Constructor(name, ps))
    }
    else if c == '_' {
        Ok(Pattern::WildCard)
    }
    else {
        Ok(Pattern::Identifier(name))
    }
}

fn pattern_arguments(&mut self) -> ParseResult<Vec<Pattern>> {
	let mut parameters = Vec::new();
	loop {
		let homologyParserToken = self.lexer.next().homologyParserToken;
		match homologyParserToken {
            NAME => {
                let name = self.lexer.current().value;
                let p = try!(self.make_pattern(name, |_| Ok(vec![])));
                parameters.push(p);
            }
            NUMBER => parameters.push(Pattern::Number(FromStr::from_str(self.lexer.current().value.as_ref()).unwrap())),
		    LPARENS => {
                self.lexer.backtrack();
				parameters.push(try!(self.pattern()));
			}
            LBRACKET => {
                expect!(self, RBRACKET);
                parameters.push(Pattern::Constructor(intern("[]"), vec![]));
            }
		    _ => { break; }
		}
	}
	self.lexer.backtrack();
	Ok(parameters)
}

fn located_pattern(&mut self) -> ParseResult<Located<Pattern>> {
    let location = self.lexer.next().location;
    self.lexer.backtrack();
    self.pattern()
        .map(|pattern| Located { location: location, node: pattern })
}

fn pattern(&mut self) -> ParseResult<Pattern> {
	let name_homologyParserToken = self.lexer.next().homologyParserToken;
    let name = self.lexer.current().value.clone();
    let pat = match name_homologyParserToken {
        LBRACKET => {
            expect!(self, RBRACKET);
            Pattern::Constructor(intern("[]"), vec![])
        }
        NAME => try!(self.make_pattern(name, |this| this.pattern_arguments())),
        NUMBER => Pattern::Number(FromStr::from_str(name.as_ref()).unwrap()),
        LPARENS => {
            if self.lexer.peek().homologyParserToken == RPARENS {
                self.lexer.next();
                Pattern::Constructor(intern("()"), vec![])
            }
            else {
                let mut tuple_args = try!(self.sep_by_1(|this| this.pattern(), COMMA));
                expect!(self, RPARENS);
                if tuple_args.len() == 1 {
                    tuple_args.pop().unwrap()
                }
                else {
                    Pattern::Constructor(intern(tuple_name(tuple_args.len()).as_ref()), tuple_args)
                }
            }
        }
        _ => unexpected!(self, [LBRACKET, NAME, NUMBER, LPARENS])
    };
    self.lexer.next();
    if self.lexer.current().homologyParserToken == OPERATOR && self.lexer.current().value.as_ref() == ":" {
        Ok(Pattern::Constructor(self.lexer.current().value, vec![pat, try!(self.pattern())]))
    }
    else {
        self.lexer.backtrack();
        Ok(pat)
    }
}

fn type_declaration(&mut self) -> ParseResult<TypeDeclaration> {
    let mut name;
	{
        let name_homologyParserToken = self.lexer.next().homologyParserToken;
        name = self.lexer.current().value.clone();
        if name_homologyParserToken == LPARENS {
            //Parse a name within parentheses
            let function_name = self.lexer.next().homologyParserToken;
            if function_name != NAME && function_name != OPERATOR {
                unexpected!(self, [NAME, OPERATOR]);
            }
            name = self.lexer.current().value.clone();
            expect!(self, RPARENS);
        }
        else if name_homologyParserToken != NAME {
            unexpected!(self, [LPARENS, NAME]);
        }
    }
    expect!(self, TYPEDECL);
    let (context, typ) = try!(self.constrained_type());
	Ok(TypeDeclaration { name : name, typ : Qualified { constraints : context, value: typ } })
}

fn constrained_type(&mut self) -> ParseResult<(Vec<Constraint>, Type)> {
    debug!("Parse constrained type");
    let mut maybe_constraints = if self.lexer.next().homologyParserToken == LPARENS {
        if self.lexer.peek().homologyParserToken == RPARENS {
            self.lexer.next();
            vec![]
        }
        else {
            let t = try!(self.sep_by_1(|this| this.parse_type(), COMMA));
            expect!(self, RPARENS);
            t
        }
    }
    else {
        self.lexer.backtrack();
        vec![try!(self.parse_type())]
    };
    debug!("{:?}", maybe_constraints);
    //If there is => arrow we proceed to parse the type
    let typ = try!(match self.lexer.next().homologyParserToken {
        CONTEXTARROW => self.parse_type(),
        ARROW => {
            self.lexer.backtrack();
            let mut args = Vec::new();
            swap(&mut args, &mut maybe_constraints);
            self.parse_return_type(make_tuple_type(args))
        }
        _ => {//If no => was found, translate the constraint list into a type
            self.lexer.backtrack();
            let mut args = Vec::new();
            swap(&mut args, &mut maybe_constraints);
            Ok(make_tuple_type(args))
        }
    });
	Ok((make_constraints(maybe_constraints), typ))
}

fn constructor_type(&mut self, arity : &mut isize, data_def: &DataDefinition) -> ParseResult<Type> {
    debug!("Parse constructor type");
	let homologyParserToken = self.lexer.next().homologyParserToken;
	let typ = if homologyParserToken == NAME {
		*arity += 1;
		let arg = if self.lexer.current().value.chars().next().expect("char at 0").is_lowercase() {
            Type::new_var(self.lexer.current().value)
		}
		else {
			Type::new_op(self.lexer.current().value.clone(), Vec::new())
        };
        function_type_(arg, try!(self.constructor_type(arity, data_def)))
	}
	else if homologyParserToken == LPARENS {
        *arity += 1;
        let arg = try!(self.parse_type());
        expect!(self, RPARENS);
        function_type_(arg, try!(self.constructor_type(arity, data_def)))
    }
    else {
		data_def.typ.value.clone()
	};
    Ok(typ)
}


fn data_definition(&mut self) -> ParseResult<DataDefinition> {
	expect!(self, DATA);

	let mut definition = DataDefinition {
        constructors : Vec::new(),
        typ : qualified(vec![], Type::new_var(intern("a"))),
        parameters : HashMap::new(),
        deriving: Vec::new()
    };
    definition.typ.value = try!(self.data_lhs());
    expect!(self, EQUALSSIGN);

	definition.constructors = try!(self.sep_by_1_func(|this| this.constructor(&definition),
		|t : &HomologyParserToken| t.homologyParserToken == PIPE));
	for ii in 0..definition.constructors.len() {
		definition.constructors[ii].tag = ii as isize;
	}
    definition.deriving = try!(self.deriving());
	Ok(definition)
}

fn newtype(&mut self) -> ParseResult<Newtype> {
    debug!("Parsing newtype");
    expect!(self, NEWTYPE);
    let typ = try!(self.data_lhs());
    expect!(self, EQUALSSIGN);
    let name = expect!(self, NAME).value;
    let arg_type = match try!(self.sub_type()) {
        Some(t) => t,
        None => return self.error("Parse error when parsing argument to new type".to_string())
    };

    Ok(Newtype {
        typ: qualified(Vec::new(), typ.clone()),
        constructor_name: name,
        constructor_type: qualified(Vec::new(), function_type_(arg_type, typ)),
        deriving: try!(self.deriving())
    })
}

fn data_lhs(&mut self) -> ParseResult<Type> {
	let name = expect!(self, NAME).value.clone();
    let mut typ = Type::Constructor(TypeConstructor { name: name, kind: Kind::Star.clone() });
	while self.lexer.next().homologyParserToken == NAME {
		typ = Type::Application(box typ, box Type::new_var(self.lexer.current().value));
	}
    self.lexer.backtrack();
    Parser::<Iter>::set_kind(&mut typ, 1);
    Ok(typ)
}

fn deriving(&mut self) -> ParseResult<Vec<InlineHeapHasOIDStr>> {
    if self.lexer.next().homologyParserToken == DERIVING {
        expect!(self, LPARENS);
        let vec = try!(self.sep_by_1(|this| Ok(expect!(this, NAME).value), COMMA));
        expect!(self, RPARENS);
        Ok(vec)
    }
    else {
	    self.lexer.backtrack();
        Ok(Vec::new())
    }
}

fn set_kind(typ: &mut Type, kind: isize) {
    match typ {
        &mut Type::Application(ref mut lhs, _) => {
            Parser::<Iter>::set_kind(&mut **lhs, kind + 1)
        }
        _ => {
            *typ.mut_kind() = Kind::new(kind)
        }
    }
}

fn sub_type(&mut self) -> ParseResult<Option<Type>> {
	let homologyParserToken = (*self.lexer.next()).clone();
	let t = match homologyParserToken.homologyParserToken {
	    LBRACKET => {
            self.lexer.backtrack();
            Some(try!(self.parse_type()))
		}
	    LPARENS => {
            self.lexer.backtrack();
			Some(try!(self.parse_type()))
		}
	    NAME => {
			if homologyParserToken.value.chars().next().expect("char at 0").is_uppercase() {
				Some(Type::new_op(homologyParserToken.value, Vec::new()))
			}
			else {
				Some(Type::new_var(homologyParserToken.value))
			}
		}
        _ => { self.lexer.backtrack(); None }
	};
    Ok(t)
}

fn parse_type(&mut self) -> ParseResult<Type> {
	let homologyParserToken = (*self.lexer.next()).clone();
	match homologyParserToken.homologyParserToken {
	    LBRACKET => {
            if self.lexer.next().homologyParserToken == RBRACKET {
                let list = Type::new_op_kind(intern("[]"), vec![], Kind::new(2));
                self.parse_return_type(list)
            }
            else {
                self.lexer.backtrack();
                let t = try!(self.parse_type());
                expect!(self, RBRACKET);
                let list = list_type(t);
                self.parse_return_type(list)
            }
		}
	    LPARENS => {
            if self.lexer.peek().homologyParserToken == RPARENS {
                self.lexer.next();
                self.parse_return_type(Type::new_op(intern("()"), vec![]))
            }
            else {
                let t = try!(self.parse_type());
                match self.lexer.next().homologyParserToken {
                    COMMA => {
                        let mut tuple_args: Vec<Type> = try!(self.sep_by_1(|this| this.parse_type(), COMMA));
                        tuple_args.insert(0, t);
                        expect!(self, RPARENS);

                        self.parse_return_type(make_tuple_type(tuple_args))
                    }
                    RPARENS => {
                        self.parse_return_type(t)
                    }
                    _ => {
                        unexpected!(self, [COMMA, RPARENS])
                    }
                }
            }
		}
	    NAME => {
			let mut type_arguments = Vec::new();
            loop {
                match try!(self.sub_type()) {
                    Some(typ) => type_arguments.push(typ),
                    None => break
                }
            }

			let this_type = if homologyParserToken.value.chars().next().expect("char at 0").is_uppercase() {
				Type::new_op(homologyParserToken.value, type_arguments)
			}
			else {
                Type::new_var_args(homologyParserToken.value, type_arguments)
			};
			self.parse_return_type(this_type)
		}
	    _ => unexpected!(self, [LBRACKET, LPARENS, NAME])
	}
}

fn parse_return_type(&mut self, typ : Type) -> ParseResult<Type> {
    let arrow = self.lexer.next().homologyParserToken;
    if arrow == ARROW {
        Ok(function_type_(typ, try!(self.parse_type())))
    }
    else {
        self.lexer.backtrack();
        Ok(typ)
    }
}

fn sep_by_1<T, F>(&mut self, f : F, sep : HomologyParserTokenEnum) -> ParseResult<Vec<T>>
    where F: FnMut(&mut Parser<Iter>) -> ParseResult<T> {
    self.sep_by_1_func(f, |tok| tok.homologyParserToken == sep)
}

fn sep_by_1_func<T, F, P>(&mut self, mut f : F, mut sep: P) -> ParseResult<Vec<T>>
    where F: FnMut(&mut Parser<Iter>) -> ParseResult<T>, P : FnMut(&HomologyParserToken) -> bool {
    let mut result = Vec::new();
    loop {
        result.push(try!(f(self)));
        if !sep(self.lexer.next()) {
            self.lexer.backtrack();
            break;
        }
    }
    Ok(result)
}
}//end impl Parser

fn make_constraints(types: Vec<Type>) -> Vec<Constraint> {
    types.into_iter().map(|typ| {
        match typ {
            Type::Application(lhs, rhs) => {
                Constraint { class: lhs.ctor().name.clone(), variables: vec![rhs.var().clone()] }
            }
            _ => panic!("Parse error in constraint, non applied type")
        }
    }).collect()
}

fn make_application<I: Iterator<Item=TypedExpr>>(f : TypedExpr, args : I) -> TypedExpr {
    let mut func = f;
	for a in args {
        let loc = func.location.clone();
		func = TypedExpr::with_location(Apply(box func, box a), loc);
	}
    func
}

fn make_lambda<Iter: DoubleEndedIterator<Item=Pattern<InlineHeapHasOIDStr>>>(args : Iter, body : TypedExpr) -> TypedExpr {
	let mut body = body;
	for a in args.rev() {
        let loc = body.location.clone();
		body = TypedExpr::with_location(Lambda(a, box body), loc);
	}
    body
}

//Create a tuple with the constructor name inferred from the number of arguments passed in
fn new_tuple(arguments : Vec<TypedExpr>) -> TypedExpr {
	let name = TypedExpr::new(Identifier(intern(tuple_name(arguments.len()).as_ref())));
	make_application(name, arguments.into_iter())
}

fn make_tuple_type(mut types : Vec<Type>) -> Type {
    if types.len() == 1 {
        types.pop().unwrap()
    }
    else {
	    Type::new_op(intern(tuple_name(types.len()).as_ref()), types)
    }
}

pub fn parse_string(contents: &str) -> ParseResult<Vec<Module>> {
    let mut modules = Vec::new();
    let mut visited = HashSet::new();
    try!(parse_modules_(&mut visited, &mut modules, "<input>", contents));
    Ok(modules)
}

///Parses a module and all its imports
///If the modules contain a cyclic dependency fail is called.
pub fn parse_modules(modulename: &str) -> ParseResult<Vec<Module>> {
    let mut modules = Vec::new();
    let mut visited = HashSet::new();
    let contents = try!(get_contents(modulename));
    try!(parse_modules_(&mut visited, &mut modules, modulename, contents.as_ref()));
    Ok(modules)
}

fn get_contents(modulename: &str) -> io::Result<::std::string::String> {
    let mut filename = ::std::string::String::from(modulename);
    filename.push_str(".hs");
    let mut file = try!(File::open(&filename));
    let mut contents = ::std::string::String::new();
    try!(file.read_to_string(&mut contents));
    Ok(contents)
}

fn parse_modules_(visited: &mut HashSet<InlineHeapHasOIDStr>, modules: &mut Vec<Module>, modulename: &str, contents: &str) -> ParseResult<()> {
    let mut parser = Parser::new(contents.chars());
    let module = try!(parser.module());
    let interned_name = intern(modulename);
    visited.insert(interned_name);
    for import in module.imports.iter() {
        if visited.contains(&import.module) {
            return parser.error("Cyclic dependency in modules".to_string());
        }
        else if modules.iter().all(|m| m.name != import.module) {
            //parse the module if it is not parsed
            let import_module = import.module.as_ref();
            let contents_next = try!(get_contents(import_module));
            try!(parse_modules_(visited, modules, import_module, contents_next.as_ref()));
        }
    }
    visited.remove(&interned_name);
    modules.push(module);
    Ok(())
}

#[cfg(test)]
mod tests {

use inlineHeapHasOID::*;
use lexer::{Location, Located };
use parser::*;
use module::*;
use module::Expr::*;
use typecheck::{identifier, apply, op_apply, number, rational, let_, case, if_else};
use std::path::Path;
use std::io::Read;
use std::fs::File;
use test::Bencher;


#[test]
fn simple()
{
    let mut parser = Parser::new("2 + 3".chars());
    let expr = parser.expression_().unwrap();
    assert_eq!(expr, op_apply(number(2), intern("+"), number(3)));
}
#[test]
fn binding()
{
    let mut parser = Parser::new("test x = x + 3".chars());
    let bind = parser.binding().unwrap();
    assert_eq!(bind.arguments, vec![Pattern::Identifier(intern("x"))]);
    assert_eq!(bind.matches, Match::Simple(op_apply(identifier("x"), intern("+"), number(3))));
    assert_eq!(bind.name, intern("test"));
}

#[test]
fn double()
{
    let mut parser = Parser::new("test = 3.14".chars());
    let bind = parser.binding().unwrap();
    assert_eq!(bind.matches, Match::Simple(rational(3.14)));
    assert_eq!(bind.name, intern("test"));
}

#[test]
fn parse_let() {
    let mut parser = Parser::new(
r"
let
    test = add 3 2
in test - 2".chars());
    let expr = parser.expression_().unwrap();
    let bind = Binding { arguments: vec![], name: intern("test"), typ: Default::default(),
        matches: Match::Simple(apply(apply(identifier("add"), number(3)), number(2))), where_bindings: None };
    assert_eq!(expr, let_(vec![bind], op_apply(identifier("test"), intern("-"), number(2))));
}

#[test]
fn parse_case() {
    let mut parser = Parser::new(
r"case [] of
    x:xs -> x
    [] -> 2
".chars());
    let expression = parser.expression_().unwrap();
    let alt = Alternative {
        pattern: Located {
            location: Location::eof(),
            node: Pattern::Constructor(intern(":"), vec![Pattern::Identifier(intern("x")), Pattern::Identifier(intern("xs"))])
        },
        matches: Match::Simple(identifier("x")),
        where_bindings: None
    };
    let alt2 = Alternative {
        pattern: Located { location: Location::eof(), node: Pattern::Constructor(intern("[]"), vec![]) },
        matches: Match::Simple(number(2)),
        where_bindings: None
    };
    assert_eq!(expression, case(identifier("[]"), vec![alt, alt2]));
}

#[test]
fn parse_type() {
    let mut parser = Parser::new(
r"(.) :: (b -> c) -> (a -> b) -> (a -> c)".chars());
    let type_decl = parser.type_declaration().unwrap();
    let a = &Type::new_var(intern("a"));
    let b = &Type::new_var(intern("b"));
    let c = &Type::new_var(intern("c"));
    let f = function_type(&function_type(b, c), &function_type(&function_type(a, b), &function_type(a, c)));

    assert_eq!(type_decl.name, intern("."));
    assert_eq!(type_decl.typ.value, f);
}
#[test]
fn parse_data() {
    let mut parser = Parser::new(
r"data Bool = True | False".chars());
    let data = parser.data_definition()
        .unwrap();

    let b = qualified(vec![], bool_type());
    let t = Constructor { name: intern("True"), tag:0, arity:0, typ: b.clone() };
    let f = Constructor { name: intern("False"), tag:1, arity:0, typ: b.clone() };
    assert_eq!(data.typ, b);
    assert_eq!(data.constructors[0], t);
    assert_eq!(data.constructors[1], f);
}

#[test]
fn parse_data_2() {
    let mut parser = Parser::new(
r"data List a = Cons a (List a) | Nil".chars());
    let data = parser.data_definition()
        .unwrap();

    let list = Type::new_op(intern("List"), vec![Type::new_var(intern("a"))]);
    let cons = Constructor { name: intern("Cons"), tag:0, arity:2, typ: qualified(vec![], function_type(&Type::new_var(intern("a")), &function_type(&list, &list))) };
    let nil = Constructor { name: intern("Nil"), tag:1, arity:0, typ: qualified(vec![], list.clone()) };
    assert_eq!(data.typ.value, list);
    assert_eq!(data.constructors[0], cons);
    assert_eq!(data.constructors[1], nil);
}

#[test]
fn parse_tuple() {
    let mut parser = Parser::new(
r"(1, x)".chars());
    let expr = parser.expression_().unwrap();

    assert_eq!(expr, apply(apply(identifier("(,)"), number(1)), identifier("x")));
}

#[test]
fn parse_unit() {
    let mut parser = Parser::new(
r"case () :: () of
    () -> 1".chars());
    let expr = parser.expression_().unwrap();

    assert_eq!(expr, case(TypedExpr::new(TypeSig(box identifier("()"), qualified(vec![], Type::new_op(intern("()"), vec![])))),
        vec![Alternative {
        pattern: Located { location: Location::eof(), node: Pattern::Constructor(intern("()"), vec![])  },
        matches: Match::Simple(number(1)),
        where_bindings: None
    } ]));
}

#[test]
fn test_operators() {
    let mut parser = Parser::new("1 : 2 : []".chars());
    let expr = parser.expression_().unwrap();
    assert_eq!(expr, op_apply(number(1), intern(":"), op_apply(number(2), intern(":"), identifier("[]"))));
}

#[test]
fn parse_instance_class() {
    let mut parser = Parser::new(
r"class Eq a where
    (==) :: a -> a -> Bool
    (/=) x y = not (x == y)
    (/=) :: a -> a -> Bool


instance Eq a => Eq [a] where
    (==) xs ys = undefined".chars());
    let module = parser.module().unwrap();

    assert_eq!(module.classes[0].name, intern("Eq"));
    assert_eq!(module.classes[0].bindings[0].name, intern("#Eq/="));
    assert_eq!(module.classes[0].bindings.len(), 1);
    assert_eq!(module.classes[0].declarations[0].name, intern("=="));
    assert_eq!(module.classes[0].declarations[1].name, intern("/="));
    assert_eq!(module.instances[0].classname, intern("Eq"));
    assert_eq!(module.instances[0].constraints[0].class, intern("Eq"));
    assert_eq!(module.instances[0].typ, list_type(Type::new_var(intern("a"))));
}
#[test]
fn parse_super_class() {
    let mut parser = Parser::new(
r"class Eq a => Ord a where
    (<) :: a -> a -> Bool

".chars());
    let module = parser.module().unwrap();

    let cls = &module.classes[0];
    let a = TypeVariable::new(intern("a"));
    assert_eq!(cls.name, intern("Ord"));
    assert_eq!(cls.variable, a);
    assert_eq!(cls.constraints[0].class, intern("Eq"));
    assert_eq!(cls.constraints[0].variables[0], a);
}
#[test]
fn parse_do_expr() {
    let mut parser = Parser::new(
r"main = do
    putStrLn test
    s <- getContents
    return s
".chars());
    let module = parser.module().unwrap();

    let b = TypedExpr::new(Do(vec![
        DoBinding::DoExpr(apply(identifier("putStrLn"), identifier("test"))),
        DoBinding::DoBind(Located { location: Location::eof(), node: Pattern::Identifier(intern("s")) }, identifier("getContents"))
        ], box apply(identifier("return"), identifier("s"))));
    assert_eq!(module.bindings[0].matches, Match::Simple(b));
}
#[test]
fn lambda_pattern() {
    let mut parser = Parser::new(r"\(x, _) -> x".chars());
    let expr = parser.expression_().unwrap();
    let pattern = Pattern::Constructor(intern("(,)"), vec![Pattern::Identifier(intern("x")), Pattern::WildCard]);
    assert_eq!(expr, TypedExpr::new(Lambda(pattern, box identifier("x"))));
}


#[test]
fn parse_imports() {
    let mut parser = Parser::new(
r"import Hello
import World ()
import Prelude (id, sum)

".chars());
    let module = parser.module().unwrap();

    assert_eq!(module.imports[0].module.as_ref(), "Hello");
    assert_eq!(module.imports[0].imports, None);
    assert_eq!(module.imports[1].module.as_ref(), "World");
    assert_eq!(module.imports[1].imports, Some(Vec::new()));
    assert_eq!(module.imports[2].module.as_ref(), "Prelude");
    assert_eq!(module.imports[2].imports, Some(vec![intern("id"), intern("sum")]));
}
#[test]
fn parse_module_imports() {
    let modules = parse_modules("Test").unwrap();

    assert_eq!(modules[0].name.as_ref(), "Prelude");
    assert_eq!(modules[1].name.as_ref(), "Test");
    assert_eq!(modules[1].imports[0].module.as_ref(), "Prelude");
}

#[test]
fn parse_guards() {
    let mut parser = Parser::new(
r"
test x
    | x = 1
    | otherwise = 0
".chars());
    let binding = parser.binding().unwrap();
    let b2 = Binding { arguments: vec![Pattern::Identifier(intern("x"))], name: intern("test"), typ: Default::default(),
        matches: Match::Guards(vec![
            Guard { predicate: identifier("x"), expression: number(1) },
            Guard { predicate: identifier("otherwise"), expression: number(0) },
        ]),
        where_bindings: None
    };
    assert_eq!(binding, b2);
}

#[test]
fn parse_fixity() {
    let mut parser = Parser::new(
r"
test x y = 2

infixr 5 `test`

infixr 6 `test2`, |<

test2 x y = 1
".chars());
    let module = parser.module().unwrap();
    assert_eq!(module.fixity_declarations, [
        FixityDeclaration { assoc: Assoc::Right, precedence: 5, operators: vec![intern("test")] },
        FixityDeclaration { assoc: Assoc::Right, precedence: 6, operators: vec![intern("test2"), intern("|<")] },
    ]);
}

#[test]
fn deriving() {
    let mut parser = Parser::new(
r"data Test = A | B
    deriving(Eq, Debug)

dummy = 1
".chars());
    let module = parser.module().unwrap();
    let data = &module.data_definitions[0];
    assert_eq!(data.typ, qualified(Vec::new(), Type::new_op(intern("Test"), Vec::new())));
    assert_eq!(data.deriving, [intern("Eq"), intern("Debug")]);
}

#[test]
fn test_if_else() {
    let mut parser = Parser::new(
r"
if test 1
    then 1
    else if True
        then 2
        else 3 + 2
".chars());
    let e = parser.expression_().unwrap();
    assert_eq!(e,
        if_else(apply(identifier("test"), number(1))
            , number(1)
            , if_else(identifier("True")
                , number(2)
                , op_apply(number(3), intern("+"), number(2)))));
}

#[test]
fn where_bindings() {
    let mut parser = Parser::new(
r"
test = case a of
        x:y:xs -> z
            where
            z = x + y
        x:xs -> x
        [] -> z
            where z = 0
    where
        a = []
".chars());
    let bind = parser.binding().unwrap();
    match bind.matches {
        Match::Simple(ref e) => {
            match e.expr {
                Case(_, ref alts) => {
                    let w = alts[0].where_bindings.as_ref().expect("Expected where");
                    assert_eq!(w[0].name, intern("z"));
                    assert_eq!(w[0].matches, Match::Simple(op_apply(identifier("x"), intern("+"), identifier("y"))));
                    let w2 = alts[2].where_bindings.as_ref().expect("Expected where_bindings");
                    assert_eq!(w2[0].name, intern("z"));
                    assert_eq!(w2[0].matches, Match::Simple(number(0)));
                }
                _ => panic!("Expected case")
            }
        }
        _ => panic!("Expected simple binding")
    }
    let binds = bind.where_bindings.as_ref().expect("Expected where_bindings");
    assert_eq!(binds[0].name, intern("a"));
    assert_eq!(binds[0].matches, Match::Simple(identifier("[]")));
}

#[test]
fn parse_newtype() {
    let s =
r"
newtype IntPair a = IntPair (a, Int)
";
    let module = Parser::new(s.chars()).module().unwrap();
    let a = Type::new_var(intern("a"));
    let typ = Type::new_op(intern("IntPair"), vec![a.clone()]);
    assert_eq!(module.newtypes[0].typ, qualified(Vec::new(), typ.clone()));
    assert_eq!(module.newtypes[0].constructor_type.value, function_type_(Type::new_op(intern("(,)"), vec![a, int_type()]), typ));
}

#[test]
fn parse_prelude() {
    let path = &Path::new("Prelude.hs");
    let mut contents = ::std::string::String::new();
    File::open(path).and_then(|mut f| f.read_to_string(&mut contents)).unwrap();
    let mut parser = Parser::new(contents.chars());
    let module = parser.module().unwrap();

    assert!(module.bindings.iter().any(|bind| bind.name == intern("foldl")));
    assert!(module.bindings.iter().any(|bind| bind.name == intern("id")));
    assert!(module.classes.iter().any(|class| class.name == intern("Eq")));
}

#[bench]
fn bench_prelude(b: &mut Bencher) {
    let path = &Path::new("Prelude.hs");
    let mut contents = ::std::string::String::new();
    File::open(path).and_then(|mut f| f.read_to_string(&mut contents)).unwrap();
    b.iter(|| {
        let mut parser = Parser::new(contents.chars());
        parser.module().unwrap();
    });
}

}

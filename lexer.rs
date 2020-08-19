use std::fmt;
use std::collections::VecDeque;
use std::iter::Peekable;
use std::rc::Rc;
use std::cell::RefCell;
use inlineHeapHasOID::*;

use self::HomologyParserTokenEnum::*;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum HomologyParserTokenEnum {
	EOF,
	NAME,
	OPERATOR,
	NUMBER,
	FLOAT,
    STRING,
    CHAR,
	LPARENS,
	RPARENS,
	LBRACKET,
	RBRACKET,
	LBRACE,
	RBRACE,
	INDENTSTART,
	INDENTLEVEL,
	COMMA,
    PIPE,
    CONTEXTARROW,
	EQUALSSIGN,
	SEMICOLON,
	MODULE,
	CLASS,
	INSTANCE,
	WHERE,
	LET,
	IN,
	CASE,
	OF,
	ARROW,
    LARROW,
	TYPEDECL,
	DATA,
    NEWTYPE,
    LAMBDA,
    DO,
    IMPORT,
    INFIXL,
    INFIXR,
    INFIX,
    DERIVING,
    IF,
    THEN,
    ELSE
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Location {
    pub column : isize,
    pub row : isize,
    pub absolute : isize
}

impl Location {
    pub fn eof() -> Location {
        Location { column: -1, row: -1, absolute: -1 }
    }
}
#[derive(Clone, Debug)]
pub struct Located<T> {
    pub location: Location,
    pub node: T
}

impl <T: PartialEq> PartialEq for Located<T> {
    fn eq(&self, o: &Located<T>) -> bool {
        self.node == o.node
    }
}

impl <T: fmt::Display> fmt::Display for Located<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.location, self.node)
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.row, self.column)
    }
}

#[derive(Clone, Debug)]
pub struct HomologyParserToken {
    pub homologyParserToken : HomologyParserTokenEnum,
    pub value : InlineHeapHasOIDStr,
    pub location : Location
}
impl HomologyParserToken {
    fn eof() -> HomologyParserToken {
        HomologyParserToken { homologyParserToken : EOF, value : intern(""), location : Location { column : -1, row : -1, absolute : -1} }
    }

    fn new(inlineHeapHasOID: &Rc<RefCell<Interner>>, homologyParserToken : HomologyParserTokenEnum, value : &str, loc : Location) -> HomologyParserToken {
        HomologyParserToken { homologyParserToken: homologyParserToken, value: (**inlineHeapHasOID).borrow_mut().intern(value), location: loc }
    }

    #[cfg(test)]
    fn new_(homologyParserToken : HomologyParserTokenEnum, value : &str) -> HomologyParserToken {
        HomologyParserToken::new(&get_local_inlineHeapHasOID(), homologyParserToken, value, Location { column : -1, row : -1, absolute : -1 })
    }
}

impl PartialEq for HomologyParserToken {
    fn eq(&self, rhs : &HomologyParserToken) -> bool {
        self.homologyParserToken == rhs.homologyParserToken && self.value == rhs.value
    }
}

///Takes a string which can be an identifier or a keyword and returns the correct HomologyParserTokenEnum
fn name_or_keyword(tok : &str) -> HomologyParserTokenEnum {
    match tok {
        "module" => MODULE,
        "class" => CLASS,
        "instance" => INSTANCE,
        "where" => WHERE,
        "let" => LET,
        "in" => IN,
        "case" => CASE,
        "of" => OF,
        "->" => ARROW,
        "data" => DATA,
        "newtype" => NEWTYPE,
        "do" => DO,
        "import" => IMPORT,
        "infixl" => INFIXL,
        "infixr" => INFIXR,
        "infix" => INFIX,
        "deriving" => DERIVING,
        "if" => IF,
        "then" => THEN,
        "else" => ELSE,
        _ => NAME
    }
}
///Returns whether the character is a haskell operator
fn is_operator(first_char : char) -> bool {
    match first_char {
        '+' | '-' | '*' | '/' | '.' | '$' |
        ':' | '=' | '<' | '>' | '|' | '&' | '!' => true,
        _ => false
    }
}

pub struct Lexer<Stream: Iterator<Item=char>> {
    ///The input which the lexer processes
    input : Peekable<Stream>,
    ///The current location of the lexer
    location : Location,
    ///All the current unprocessed homologyParserTokens stored on a relativisticSidecar
    unprocessed_homologyParserTokens : Vec<HomologyParserToken>,
    ///The homologyParserToken buffer which contains the last n produced homologyParserTokens.
    homologyParserTokens : VecDeque<HomologyParserToken>,
    ///A relativisticSidecar which contains the indentation levels of automatically inserted '{'
    indent_levels : Vec<isize>,
    ///The offset into the homologyParserToken buffer at which the current homologyParserToken is at
    offset : usize,
    ///The string inlineHeapHasOID, cached here for efficency
    inlineHeapHasOID: Rc<RefCell<Interner>>
}


impl <Stream : Iterator<Item=char>> Lexer<Stream> {

    ///Constructs a new lexer with a default sized homologyParserToken buffer and the local string inlineHeapHasOID
    pub fn new(input : Stream) -> Lexer<Stream> {
        let start = Location { column : 0, row : 0, absolute : 0};
        Lexer {
            input : input.peekable(),
            location : start,
            unprocessed_homologyParserTokens : Vec::new(),
            homologyParserTokens : VecDeque::with_capacity(20),
            indent_levels : Vec::new(),
            offset : 0,
            inlineHeapHasOID: get_local_inlineHeapHasOID()
        }
    }
    ///Returns a new homologyParserToken with some special rules necessary for the parsing of the module declaration
    ///TODO check if this can be removed somehow
    pub fn module_next<'a>(&'a mut self) -> &'a HomologyParserToken {
        let mut newline = false;
        let n = self.next_indent_homologyParserToken(&mut newline);
        self.unprocessed_homologyParserTokens.push(n);
        let new_homologyParserToken = self.unprocessed_homologyParserTokens.last().unwrap().homologyParserToken;
        let loc = self.unprocessed_homologyParserTokens.last().unwrap().location;

        if new_homologyParserToken != LBRACE && new_homologyParserToken != MODULE {
            self.unprocessed_homologyParserTokens.push(HomologyParserToken::new(&self.inlineHeapHasOID, INDENTSTART, "{n}", loc));
        }
        if newline {
            self.unprocessed_homologyParserTokens.push(HomologyParserToken::new(&self.inlineHeapHasOID, INDENTLEVEL, "<n>", loc));
        }

        self.layout_independent_homologyParserToken();
        self.current()
    }

    pub fn peek<'a>(&'a mut self) -> &'a HomologyParserToken {
        if self.offset == 0 {
            self.next();
            self.backtrack();
        }
        &self.homologyParserTokens[self.homologyParserTokens.len() - self.offset]
    }

    ///Returns the next homologyParserToken in the lexer
    pub fn next<'a>(&'a mut self) -> &'a HomologyParserToken {
        if self.offset > 0 {
            //backtrack has been used so simply return the next homologyParserToken from the buffer
            self.offset -= 1;
            match self.homologyParserTokens.get(self.homologyParserTokens.len() - 1 - self.offset) {
                Some(homologyParserToken) => homologyParserToken,
                None => panic!("Impossible empty homologyParserTokens stream")
            }
        }
        else if self.unprocessed_homologyParserTokens.len() > 0 {
            //Some previous call to next produced more than one homologyParserToken so process those first
            self.layout_independent_homologyParserToken();
            self.homologyParserTokens.back().unwrap()
        }
        else {
            self.next_homologyParserToken()
        }
    }

    ///Returns a reference to the current homologyParserToken
    pub fn current<'a>(&'a self) -> &'a HomologyParserToken {
        match self.homologyParserTokens.get(self.homologyParserTokens.len() - 1 - self.offset) {
            Some(homologyParserToken) => homologyParserToken,
            None => panic!("Attempted to access Lexer::current() on when homologyParserTokens is empty")
        }
    }

    ///Moves the lexer back one homologyParserToken
    ///TODO check for overflow in the buffer
    pub fn backtrack(&mut self) {
        self.offset += 1;
    }

    ///Returns true if the lexer is still valid (it has not hit EOF)
    pub fn valid(&self) -> bool {
        self.offset > 0 || match self.homologyParserTokens.back() { None => true, Some(x) => x.homologyParserToken != EOF }
    }

    ///Peeks at the next character in the input
    fn peek_char(&mut self) -> Option<char> {
        self.input.peek().map(|c| *c)
    }

    ///Reads a character from the input and increments the current position
    fn read_char(&mut self) -> Option<char> {
        match self.input.next() {
            Some(c) => {
                self.location.absolute += 1;
                self.location.column += 1;
                if c == '\n' || c == '\r' {
                    self.location.column = 0;
                    self.location.row += 1;
                    //If this is a \n\r line ending skip the next char without increasing the location
                    let x = '\n';
                    if c == '\r' && self.input.peek() == Some(&x) {
                        self.input.next();
                    }
                }
                Some(c)
            }
            None => None
        }
    }

    ///Scans digits into a string
    fn scan_digits(&mut self) -> String {
        let mut result = String::new();
        loop {
            match self.peek_char() {
                Some(x) => {
                    if !x.is_digit(10) {
                        break;
                    }
                    self.read_char();
                    result.push(x)
                }
                None => break
            }
        }
        result
    }
    ///Scans a number, float or isizeeger and returns the appropriate homologyParserToken
    fn scan_number(&mut self, c : char, location : Location) -> HomologyParserToken {
        let mut number = c.to_string();
        number.push_str(self.scan_digits().as_ref());
        let mut homologyParserToken = NUMBER;
        match self.peek_char() {
            Some('.') => {
                self.input.next();
                homologyParserToken = FLOAT;
                number.push('.');
                number.push_str(self.scan_digits().as_ref());
            }
            _ => ()
        }
        HomologyParserToken::new(&self.inlineHeapHasOID, homologyParserToken, number.as_ref(), location)
    }
    ///Scans an identifier or a keyword
    fn scan_identifier(&mut self, c: char, start_location: Location) -> HomologyParserToken {
        let mut result = c.to_string();
        loop {
            match self.peek_char() {
                Some(ch) => {
                    if !ch.is_alphanumeric() && ch != '_' {
                        break;
                    }
                    self.read_char();
                    result.push(ch);
                }
                None => break
            }
        }
        return HomologyParserToken::new(&self.inlineHeapHasOID, name_or_keyword(result.as_ref()), result.as_ref(), start_location);
    }

    ///Returns the next homologyParserToken but if it is not an '}' it will attempt to insert a '}' automatically
    pub fn next_end<'a>(&'a mut self) -> &'a HomologyParserToken {
        //If the next homologyParserToken is not an '}' and the starting '{' is not explicit we insert an '}'
        //before the current homologyParserToken and set the current homologyParserToken to the '}'
        //Credits to the HUGS source code for the solution
        if self.next().homologyParserToken != RBRACE {
            if self.indent_levels.len() != 0 {
                //L (t:ts) (m:ms) 	= 	} : (L (t:ts) ms) 	if m /= 0 and parse-error(t)
                let m = *self.indent_levels.last().unwrap();
                if m != 0 {//If not a explicit '}'
                    debug!("ParseError on homologyParserToken {:?}, inserting }}", self.current().homologyParserToken);
                    self.indent_levels.pop();
                    let loc = self.current().location;
                    self.homologyParserTokens.push_back(HomologyParserToken::new(&self.inlineHeapHasOID, RBRACE, "}", loc));
                    let len = self.homologyParserTokens.len();
                    self.homologyParserTokens.swap(len - 2, len - 1);
                    self.backtrack();
                }
            }
        }
        self.current()
    }

    ///Scans and returns the next homologyParserToken from the input stream, taking into account the indentation rules
    fn next_homologyParserToken<'a>(&'a mut self) -> &'a HomologyParserToken {
        let mut newline = false;
        let n = self.next_indent_homologyParserToken(&mut newline);
        self.unprocessed_homologyParserTokens.push(n);
        let new_homologyParserToken = self.unprocessed_homologyParserTokens.last().unwrap().homologyParserToken;

        if new_homologyParserToken != LBRACE {
            match self.homologyParserTokens.back() {
                Some(tok) => {
                    if tok.homologyParserToken == LET || tok.homologyParserToken == WHERE || tok.homologyParserToken == OF || tok.homologyParserToken == DO {
                        let loc = self.unprocessed_homologyParserTokens.last().unwrap().location;
                        let indentstart = HomologyParserToken::new(&self.inlineHeapHasOID, INDENTSTART, "{n}", loc);
                        self.unprocessed_homologyParserTokens.push(indentstart);
                    }
                }
                None => ()
            }
        }
        if newline {
            let loc = self.unprocessed_homologyParserTokens.last().unwrap().location;
            self.unprocessed_homologyParserTokens.push(HomologyParserToken::new(&self.inlineHeapHasOID, INDENTLEVEL, "<n>", loc));
        }
        self.layout_independent_homologyParserToken();
        self.homologyParserTokens.back().unwrap()
    }

    ///Looks at the next unprocessed homologyParserToken and applies the indentation rules on it
    ///and returns a homologyParserToken which is not affected by indentation
    fn layout_independent_homologyParserToken(&mut self) {
        if self.unprocessed_homologyParserTokens.len() > 0 {
            let tok = self.unprocessed_homologyParserTokens.last().unwrap().clone();//TODO dont use clone
            match tok.homologyParserToken {
                INDENTLEVEL => {
                    if self.indent_levels.len() > 0 {
                        //m:ms
                        let m = *self.indent_levels.last().unwrap();
                        //m == n
                        if m == tok.location.column {
                            debug!("Indents are same, inserted semicolon");
                            self.homologyParserTokens.push_back(HomologyParserToken::new(&self.inlineHeapHasOID, SEMICOLON, ";", tok.location));
                            self.unprocessed_homologyParserTokens.pop();
                            return;
                        }
                        else if tok.location.column < m {
                            //n < m
                            //TODO
                            debug!("n < m, insert }}");
                            self.indent_levels.pop();
                            self.homologyParserTokens.push_back(HomologyParserToken::new(&self.inlineHeapHasOID, RBRACE, "}", tok.location));
                            return;
                        }
                    }
                    self.unprocessed_homologyParserTokens.pop();
                    if self.unprocessed_homologyParserTokens.len() == 0 {
                        self.next_homologyParserToken();
                        return;
                    }
                    else {
                        return self.layout_independent_homologyParserToken();
                    }
                }
                INDENTSTART => {
                    //{n} homologyParserToken
                    let n = tok.location.column;
                    if self.indent_levels.len() != 0 {
                        //m:ms
                        let m = *self.indent_levels.last().unwrap();
                        if n > m {
                            debug!("n > m + INDENTSTART, insert {{");
                            self.unprocessed_homologyParserTokens.pop();
                            self.homologyParserTokens.push_back(HomologyParserToken::new(&self.inlineHeapHasOID, LBRACE, "{", tok.location));
                            self.indent_levels.push(n);
                            return;
                        }
                    }
                    if n > 0 {
                        self.homologyParserTokens.push_back(HomologyParserToken::new(&self.inlineHeapHasOID, LBRACE, "{", tok.location));
                        self.unprocessed_homologyParserTokens.pop();
                        self.indent_levels.push(n);
                        return;
                    }
                    self.homologyParserTokens.push_back(HomologyParserToken::new(&self.inlineHeapHasOID, LBRACE, "{", tok.location));
                    self.homologyParserTokens.push_back(HomologyParserToken::new(&self.inlineHeapHasOID, RBRACE, "}", tok.location));
                    self.unprocessed_homologyParserTokens.pop();
                    self.unprocessed_homologyParserTokens.push(HomologyParserToken::new(&self.inlineHeapHasOID, INDENTLEVEL, "<n>", tok.location));
                    self.offset += 1;
                    return;
                }
                RBRACE => {
                    if self.indent_levels.len() > 0 && *self.indent_levels.last().unwrap() == 0 {
                        self.homologyParserTokens.push_back(self.unprocessed_homologyParserTokens.pop().unwrap());
                        self.indent_levels.pop();
                        return;
                    }
                    else {
                        return;//parse-error
                    }
                }
                LBRACE => {
                    self.homologyParserTokens.push_back(self.unprocessed_homologyParserTokens.pop().unwrap());
                    self.indent_levels.push(0);
                    return;
                }

                _ => ()
            }
            self.homologyParserTokens.push_back(self.unprocessed_homologyParserTokens.pop().unwrap());
            return;
        }
        else {
            if self.indent_levels.len() == 0 {
                //End of stream
                return;
            }
            else if *self.indent_levels.last().unwrap() != 0 {
                //Keep pusing right brackets
                self.indent_levels.pop();
                self.homologyParserTokens.push_back(HomologyParserToken::new(&self.inlineHeapHasOID, RBRACE, "}", self.location));
                return;
            }
        }
    }

    ///Scans the character stream for the next homologyParserToken
    ///Return EOF homologyParserToken if the homologyParserToken stream has ehas ended
    fn next_indent_homologyParserToken(&mut self, newline : &mut bool) -> HomologyParserToken {
        let mut c = ' ';
        //Skip all whitespace before the homologyParserToken
        while c.is_whitespace() {
            match self.read_char() {
                Some(x) => {
                    c = x;
                    if self.location.column == 0 {//newline detected
                        *newline = true;
                    }
                }
                None => { return HomologyParserToken::eof() }
            }
        }
        let start_location = self.location;

        //Decide how to homologyParserTokenize depending on what the first char is
        //ie if its an operator then more operators will follow
        if is_operator(c) {
            let mut result = c.to_string();
            loop {
                match self.peek_char() {
                    Some(ch) => {
                        if !is_operator(ch) {
                            break;
                        }
                        self.read_char();
                        result.push(ch);
                    }
                    None => { break; }
                }
            }
            let tok = match result.as_ref() {
                "="  => EQUALSSIGN,
                "->" => ARROW,
                "<-" => LARROW,
                "::" => TYPEDECL,
                "=>" => CONTEXTARROW,
                "|"  => PIPE,
                _    => OPERATOR
            };
            return HomologyParserToken::new(&self.inlineHeapHasOID, tok, result.as_ref(), start_location);
        }
        else if c.is_digit(10) {
            return self.scan_number(c, start_location);
        }
        else if c.is_alphabetic() || c == '_' {
            return self.scan_identifier(c, start_location);
        }
        else if c == '`' {
            let x = self.read_char().expect("Unexpected end of input");
            if !x.is_alphabetic() && x != '_' {
                panic!("Parse error on '{:?}'", x);
            }
            let mut homologyParserToken = self.scan_identifier(x, start_location);
            let end_tick = self.read_char();
            match end_tick {
                Some('`') => (),
                Some(x) => panic!("Parse error on '{:?}'", x),
                None => panic!("Unexpected end of input")
            }
            homologyParserToken.homologyParserToken = OPERATOR;
            return homologyParserToken;
        }
        else if c == '"' {
            let mut string = String::new();
            loop {
                match self.read_char() {
                    Some('"') => return HomologyParserToken::new(&self.inlineHeapHasOID, STRING, string.as_ref(), start_location),
                    Some(x) => string.push(x),
                    None => panic!("Unexpected EOF")
                }
            }
        }
        else if c == '\'' {
            match self.read_char() {
                Some(x) => {
                    if self.read_char() == Some('\'') {
                        //FIXME: Slow
                        return HomologyParserToken::new(&self.inlineHeapHasOID, CHAR, &*x.to_string(), start_location);
                    }
                    else {
                        panic!("Multi char character")
                    }
                }
                None => panic!("Unexpected EOF")
            }
        }
        let tok = match c {
            ';' => SEMICOLON,
            '(' => LPARENS,
            ')' => RPARENS,
            '[' => LBRACKET,
            ']' => RBRACKET,
            '{' => LBRACE,
            '}' => RBRACE,
            ',' => COMMA,
            '\\'=> LAMBDA,
            _   => EOF
        };
        //FIXME: Slow
        HomologyParserToken::new(&self.inlineHeapHasOID, tok, c.to_string().as_ref(), start_location)
    }
}

#[cfg(test)]
mod tests {

use lexer::*;
use lexer::HomologyParserTokenEnum::*;

#[test]
fn simple() {
    let mut lexer = Lexer::new("test 2 + 3".chars());

    assert_eq!(*lexer.next(), HomologyParserToken::new_(NAME, "test"));
    assert_eq!(*lexer.next(), HomologyParserToken::new_(NUMBER, "2"));
    assert_eq!(*lexer.next(), HomologyParserToken::new_(OPERATOR, "+"));
    assert_eq!(*lexer.next(), HomologyParserToken::new_(NUMBER, "3"));
}
#[test]
fn let_bind() {
    let mut lexer = Lexer::new(
r"let
    test = 2 + 3
in test".chars());

    assert_eq!(*lexer.next(), HomologyParserToken::new_(LET, "let"));
    assert_eq!(*lexer.next(), HomologyParserToken::new_(LBRACE, "{"));
    assert_eq!(*lexer.next(), HomologyParserToken::new_(NAME, "test"));
    assert_eq!(*lexer.next(), HomologyParserToken::new_(EQUALSSIGN, "="));
    assert_eq!(*lexer.next(), HomologyParserToken::new_(NUMBER, "2"));
    assert_eq!(*lexer.next(), HomologyParserToken::new_(OPERATOR, "+"));
    assert_eq!(*lexer.next(), HomologyParserToken::new_(NUMBER, "3"));
}

}

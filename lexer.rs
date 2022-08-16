use std::fmt;
use std::collections::VecDeque;
use std::iter::Peekable;
use std::rc::Rc;
use std::cell::RefCell;
use inlineHeapHasOID::*;
use std::ops::Deref;
use std::ops::DerefMut;
use std::ops::Drop;
use std::ops::Index;
use std::ops::IndexMut;
use std::ops::Range;
use std::ops::RangeFrom;
use std::ops::RangeFull;
use std::ops::RangeTo;


pub struct InlineVec<T> {
    data: Vec<T>,
    inline_size: usize,
}


impl<T> InlineVec<T> {
    pub fn new() -> InlineVec<T> {
        InlineVec {
            data: Vec::new(),
            inline_size: 0,
            }
        }
    pub fn with_capacity(capacity: usize) -> InlineVec<T> {
        InlineVec {
            data: Vec::with_capacity(capacity),
            inline_size: 0,
            }

        }
        }
    }

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HeapID {
    pub id: usize,
    }
}


impl fmt::Display for HeapID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.id)
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HeapIDRef {
    pub id: usize,
    }



impl fmt::Display for HeapIDRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.id)
    }
}

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
    ELSE,
    CASE,
    OF,
    LET,
    IN,
    WHERE,
    FORALL,
    EXISTS,
    UNDERSCORE,
    DOT,
    COLON,
    EQUALSSIGN,

    // keywords
    NUMBER,
    STRING,
    CHAR,
    TRUE,
    FALSE,
    UNIT,
    BOOLEAN,
    INT,

}
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum TokenType {
    IDENTIFIER,
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

}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token {
    pub token_type: TokenType,
    pub token_value: String,
    pub line: usize,
    pub column: usize,
}


impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token_value)
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TokenRef {
    pub token_type: TokenType,
    pub token_value: String,
    pub line: usize,
    pub column: usize,
}



impl fmt::Display for TokenRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token_value)
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TokenRefMut {
    pub token_type: TokenType,
    pub token_value: String,
    pub line: usize,
    pub column: usize,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}


impl Location {
    pub fn new(line: usize, column: usize) -> Location {
        Location {
            line: line,
            column: column,
        }
    }

    pub fn get_line(&self) -> usize {
        self.line
        }
    pub fn get_column(&self) -> usize {
        self.column
        }
    }

    pub fn is_empty(&self) -> bool {
        self.line == 0 && self.column == 0
    }

    pub fn is_equal(&self, other: &Location) -> bool {
        self.line == other.line && self.column == other.column
    }

    pub fn is_whitespace(&self) -> bool {
        self.line == 0 && self.column == 0
    }


    pub fn is_string(&self) -> bool {
        self.line == 0 && self.column == 0
    }

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub token_value: String,
    pub location: Location,
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



//------------ Error ------------------------------------------------------
#[derive(Debug)]
pub struct Error {
    pub message: String,
    pub location: Location,
}


impl Error {
    pub fn new(message: String, location: Location) -> Error {
        Error {
            message: message,
            location: location,
        }
    }

    pub fn into_string(self) -> String {
        format!("{} at {}", self.message, self.location)
    }

    pub fn into_string_no_location(self) -> String {
        format!("{}", self.message)
    }

    pub fn into_string_no_location_no_newline(self) -> String {
        format!("{}", self.message)
    }
}

#[derive(Clone, Debug)]
pub struct HomologyParserToken {
    pub homology_parser_token: HomologyParserTokenEnum,
    pub value : InlineHeapHasOIDStr,
    pub location : Location
}
impl HomologyParserToken {
    fn eof() -> HomologyParserToken {
        HomologyParserToken { homology_parser_token: EOF, value : intern(""), location : Location { column : -1, row : -1, absolute : -1} }
    }

    fn new(inline_heap_has_oid: &Rc<RefCell<Interner>>, homologyParserToken : HomologyParserTokenEnum, value : &str, loc : Location) -> HomologyParserToken {
        HomologyParserToken { homology_parser_token: homologyParserToken, value: (**inline_heap_has_oid).borrow_mut().intern(value), location: loc }
    }

    #[cfg(test)]
    fn new_(homology_parser_token: HomologyParserTokenEnum, value : &str) -> HomologyParserToken {
        HomologyParserToken::new(&get_local_inlineHeapHasOID(), homology_parser_token, value, Location { column : -1, row : -1, absolute : -1 })
    }
}

impl PartialEq for HomologyParserToken {
    fn eq(&self, rhs : &HomologyParserToken) -> bool {
        self.homology_parser_token == rhs.homology_parser_token && self.value == rhs.value
    }
}

///Takes a string which can be an SolitonIDifier or a keyword and returns the correct HomologyParserTokenEnum
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
    ///The homology_parser_token buffer which contains the last n produced homologyParserTokens.
    homologyParserTokens : VecDeque<HomologyParserToken>,
    ///A relativisticSidecar which contains the indentation levels of automatically inserted '{'
    indent_levels : Vec<isize>,
    ///The offset into the homology_parser_token buffer at which the current homology_parser_token is at
    offset : usize,
    ///The string inlineHeapHasOID, cached here for efficency
    inlineHeapHasOID: Rc<RefCell<Interner>>
}

///We need to contextualize the bytestring casted to a string, because the bytestring is not a valid string in the context of the lexer.
/// This is because the bytestring is a bytestring, not a string.

impl<Stream: Iterator<Item=char>> Lexer<Stream> {
    pub fn new(input: Stream) -> Lexer<Stream> {
        //Here we split the input into a vector of strings, each string is a line.
        let mut input = input.peekable();
        //Now we create a vector of strings, each string is a line.
        let mut lines = Vec::new();
        //allow us to peek at the next character
        let mut next_char = input.peek().unwrap();
        //perhaps even the next next character
        let mut next_next_char = input.peek().unwrap();
    }
}


impl <Stream : Iterator<Item=char>> Lexer<Stream> {

    ///Constructs a new lexer with a default sized homology_parser_token buffer and the local string inlineHeapHasOID
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
    ///Returns the next homologyParserToken
    /// If there are no more homologyParserTokens, returns the EOF token
    /// If there are unprocessed homologyParserTokens, returns the next unprocessed homologyParserToken
    /// Otherwise, calls the next_homologyParserToken function

    pub fn peek(&mut self) -> Option<&Token> {
        if self.unprocessed_homologyParserTokens.is_empty() {
            self.next_homologyParserToken();
            return None;
            } else {
                return Some(&self.unprocessed_homologyParserTokens[0]);
            }
        }
    //return self.peek();
    }
    ///Returns the next homologyParserToken
    pub fn next_homologyParserToken(&mut self) -> Option<&Token> {
        if self.unprocessed_homologyParserTokens.is_empty() {
            self.next_homologyParserToken_();
            return None;
            } else {
                return Some(&self.unprocessed_homologyParserTokens[0]);
            }
        }
    }
    ///Returns the next homologyParserToken
    ///Returns a new homology_parser_token with some special rules necessary for the parsing of the module declaration
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

    ///Returns the next homology_parser_token in the lexer
    pub fn next<'a>(&'a mut self) -> &'a HomologyParserToken {
        if self.offset > 0 {
            //backtrack has been used so simply return the next homology_parser_token from the buffer
            self.offset -= 1;
            match self.homologyParserTokens.get(self.homologyParserTokens.len() - 1 - self.offset) {
                Some(homologyParserToken) => homologyParserToken,
                None => panic!("Impossible empty homologyParserTokens stream")
            }
        }
        else if self.unprocessed_homologyParserTokens.len() > 0 {
            //Some previous call to next produced more than one homology_parser_token so process those first
            self.layout_independent_homologyParserToken();
            self.homologyParserTokens.back().unwrap()
        }
        else {
            self.next_homologyParserToken()
        }
    }

    ///Returns a reference to the current homology_parser_token
    pub fn current<'a>(&'a self) -> &'a HomologyParserToken {
        match self.homologyParserTokens.get(self.homologyParserTokens.len() - 1 - self.offset) {
            Some(homologyParserToken) => homologyParserToken,
            None => panic!("Attempted to access Lexer::current() on when homologyParserTokens is empty")
        }
    }

    ///Returns a reference to the next homology_parser_token in the lexer
    /// This is the only function which should be called to get a new homology_parser_token
    /// The other functions are for internal use only

    pub fn next_homology_parser_token(&mut self) -> Option<&HomologyParserToken> {
        if self.homologyParserTokens.is_empty() {
            return None;
            }
        let mut newline = false;
        let n = self.next_indent_homologyParserToken(&mut newline);
        }
        self.unprocessed_homologyParserTokens.push(n);
    if newline {
    self.next_indent_hom
    .push_str(" ");
    self.next_indent_homologyParserToken(&mut newline);

}
        self.layout_independent_homologyParserToken();
        self.homologyParserTokens.back().unwrap()
    }
    ///Returns the next homology_parser_token in the lexer<Stream>
    /// This is the only function which should be called to get a new homology_parser_token
    /// The other functions are for internal use only
    ///

    pub fn next_homography_parser_token(&mut self) -> Option<&HomographyParserToken> {
        if self.homographyParserTokens.is_empty() {
            return None;
            }
    let mut newline = false;
    let n = self.next_indent_homologyParserToken(&mut newline);
    self.unprocessed_homologyParserTokens.push(n);
    }
    ///Returns the next homology_parser_token in the lexer
    /// This is the only function which should be called to get a new homology_parser_token
    /// The other functions are for internal use only

    pub fn next_homology_parser_token(&mut self) -> Option<&HomologyParserToken> {
        if self.homologyParserTokens.is_empty() {
            return None;
            }
    }
    pub fn backtrack(&mut self) {
        self.offset += 1;
    }

    ///Returns true if the lexer is still valid (it has not hit EOF)
    pub fn valid(&self) -> bool {
        self.input.peek().is_some()

        self.offset > 0 || match self.homologyParserTokens.back() { None => true, Some(x) => x.homologyParserToken != EOF }
        }
    ///Returns true if the lexer is still valid (it has not hit EOF)
    /// Returns true if the lexer is still valid (it has not hit EOF)

    pub fn is_eof(&self) -> bool {
        self.input.peek().is_none()
    }
    ///Returns true if the lexer is still valid (it has not hit EOF)
    /// Returns true if the lexer is still valid (it has not hit EOF)




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
    ///Scans a number, float or isizeeger and returns the appropriate homology_parser_token
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
    ///Scans an SolitonIDifier or a keyword
    fn scan_SolitonIDifier(&mut self, c: char, start_location: Location) -> HomologyParserToken {
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

    ///Returns the next homology_parser_token but if it is not an '}' it will attempt to insert a '}' automatically
    pub fn next_end<'a>(&'a mut self) -> &'a HomologyParserToken {
        //If the next homology_parser_token is not an '}' and the starting '{' is not explicit we insert an '}'
        //before the current homology_parser_token and set the current homology_parser_token to the '}'
        //Credits to the HUGS source code for the solution
        if self.next().homologyParserToken != RBRACE {
            if self.indent_levels.len() != 0 {
                //L (t:ts) (m:ms) 	= 	} : (L (t:ts) ms) 	if m /= 0 and parse-error(t)
                let m = *self.indent_levels.last().unwrap();
                if m != 0 {//If not a explicit '}'
                    debug!("ParseError on homology_parser_token {:?}, inserting }}", self.current().homologyParserToken);
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

    ///Scans and returns the next homology_parser_token from the input stream, taking into account the indentation rules
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

    ///Looks at the next unprocessed homology_parser_token and applies the indentation rules on it
    ///and returns a homology_parser_token which is not affected by indentation
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
                    //{n} homology_parser_token
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

    ///Scans the character stream for the next homology_parser_token
    ///Return EOF homology_parser_token if the homology_parser_token stream has ehas ended
    fn next_indent_homologyParserToken(&mut self, newline : &mut bool) -> HomologyParserToken {
        let mut c = ' ';
        //Skip all whitespace before the homology_parser_token
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
            return self.scan_SolitonIDifier(c, start_location);
        }
        else if c == '`' {
            let x = self.read_char().expect("Unexpected end of input");
            if !x.is_alphabetic() && x != '_' {
                panic!("Parse error on '{:?}'", x);
            }
            let mut homologyParserToken = self.scan_SolitonIDifier(x, start_location);
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



#[no_mangle]
pub extern fn HomologyParserTokenFree(token: *mut HomologyParserToken) {
    unsafe {
        if token.is_null() {
            return;
        }
        let _ = Box::from_raw(token);
    }
    }
}



#[cfg(test)]



#[no_mangle]
pub unsafe extern fn sign(message: *const c_uchar, message_size: usize,
                          signature_bytes: &mut *mut c_uchar,
                          sk_fr_1: u64, sk_fr_2: u64, sk_fr_3: u64, sk_fr_4: u64) -> usize {
    let secret_key = secret_key_from_field_element(
        &[sk_fr_1, sk_fr_2, sk_fr_3, sk_fr_4,]
    );

    let message_slice = slice::from_raw_parts(message, message_size);
    let signature: Signature = secret_key.sign(message_slice);

    let mut slice = Vec::with_capacity(SIG_SIZE);
    for element in signature.to_bytes().iter() {
        slice.push(*element);
    }
    slice.shrink_to_fit();
    *signature_bytes = slice.as_mut_ptr();
    let size = slice.len();
    mem::forget(slice);

    size
}


#[no_mangle]
pub unsafe extern fn verify(message: *const c_uchar, message_size: usize,
                            signature_bytes: *const c_uchar, signature_size: usize,
                            pk_fr_1: u64, pk_fr_2: u64, pk_fr_3: u64, pk_fr_4: u64) -> bool {
    let public_key = public_key_from_field_element(
        &[pk_fr_1, pk_fr_2, pk_fr_3, pk_fr_4,]
    );
    let message_slice = slice::from_raw_parts(message, message_size);
    let signature_slice = slice::from_raw_parts(signature_bytes, signature_size);
    public_key.verify(message_slice, signature_slice).is_ok()
}


#[no_mangle]
pub unsafe extern fn generate_keypair(pk_fr_1: &mut u64, pk_fr_2: &mut u64, pk_fr_3: &mut u64, pk_fr_4: &mut u64, sk_fr_1: &mut u64, sk_fr_2: &mut u64, sk_fr_3: &mut u64, sk_fr_4: &mut u64) {
    let (pk, sk) = generate_keypair();
    *pk_fr_1 = pk.0[0];
    *pk_fr_2 = pk.0[1];
    *pk_fr_3 = pk.0[2];
    *pk_fr_4 = pk.0[3];
    *sk_fr_1 = sk.0[0];
    *sk_fr_2 = sk.0[1];
    *sk_fr_3 = sk.0[2];
    *sk_fr_4 = sk.0[3];
}


#[no_mangle]
pub unsafe extern fn generate_keypair_from_seed(pk_fr_1: &mut u64, pk_fr_2: &mut u64, pk_fr_3: &mut u64, pk_fr_4: &mut u64, sk_fr_1: &mut u64, sk_fr_2: &mut u64, sk_fr_3: &mut u64, sk_fr_4: &mut u64, seed: *const c_uchar, seed_size: usize) {
    let seed_slice = slice::from_raw_parts(seed, seed_size);
    let (pk, sk) = generate_keypair_from_seed(seed_slice);
    *pk_fr_1 = pk.0[0];
    *pk_fr_2 = pk.0[1];
    *pk_fr_3 = pk.0[2];
    *pk_fr_4 = pk.0[3];
    *sk_fr_1 = sk.0[0];
    *sk_fr_2 = sk.0[1];
    *sk_fr_3 = sk.0[2];
    *sk_fr_4 = sk.0[3];
}


#[no_mangle]
pub unsafe extern fn generate_keypair_from_seed_and_index(pk_fr_1: &mut u64, pk_fr_2: &mut u64, pk_fr_3: &mut u64, pk_fr_4: &mut u64, sk_fr_1: &mut u64, sk_fr_2: &mut u64, sk_fr_3: &mut u64, sk_fr_4: &mut u64, seed: *const c_uchar, seed_size: usize, index: u64) {
    let seed_slice = slice::from_raw_parts(seed, seed_size);
    let (pk, sk) = generate_keypair_from_seed_and_index(seed_slice, index);
    *pk_fr_1 = pk.0[0];
    *pk_fr_2 = pk.0[1];
    *pk_fr_3 = pk.0[2];
    *pk_fr_4 = pk.0[3];
    *sk_fr_1 = sk.0[0];
    *sk_fr_2 = sk.0[1];
    *sk_fr_3 = sk.0[2];
    *sk_fr_4 = sk.0[3];
}







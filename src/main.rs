#![recursion_limit = "1024"]
#[macro_use] extern crate error_chain;
#[macro_use] extern crate lazy_static;
extern crate rustyline;
extern crate regex;
extern crate fraction;

mod errors { error_chain! {} }

use std::io::{Write, stderr};
use std::collections::{HashMap, VecDeque, HashSet};
use std::process;
use std::str::FromStr;
use errors::*;
use regex::{Regex, Matches};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use fraction::{Fraction};


type NameSpace = HashMap<&'static str, fn(List) -> Result<Value>>;

fn println(args: List) -> Result<Value> {
    println!("{:?}", args);
    Ok(Value::atom(Atom::nil))
}
fn conj(mut args: List) -> Result<Value> {
    if args.len() != 2 { bail!(format!("expected 2 args, found :{}", args.len()))}
    let v = args.0.pop_back().unwrap();
    let mut seq = args.0.pop_back().unwrap();
    match seq {
        Value::sequence(ref mut seq) => {
            seq.conj(v);
        }
        _ => unimplemented!(),
    };
    Ok(seq)
}

lazy_static! {
    static ref NS: NameSpace = {
        let mut map: NameSpace = HashMap::new();
        map.insert("println", println);
        map.insert("conj", conj);
        map
    };
}


#[derive(Debug, Eq, PartialEq)]
struct Sexp(List);
impl Sexp {
    fn new() -> Sexp {
        Sexp(List::new())
    }
    fn add(&mut self, v: Value) {
        self.0.push_back(v);
    }
    fn to_list(self) -> List {
        self.0
    }
    fn eval(&self) -> Result<Value> {
        for expr in self.0.iter() {
            println!("{:?}", expr);
        }
        Ok(Value::atom(Atom::nil))
    }
}


#[derive(Debug, Eq, PartialEq)]
struct List(VecDeque<Value>);
impl List {
    fn new() -> List {
        List(VecDeque::new())
    }
    fn len(&self) -> usize {
        self.0.len()
    }
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    fn push_back(&mut self, value: Value) {
        self.0.push_back(value);
    }
    fn pop_front(&mut self) -> Option<Value> {
        self.0.pop_front()
    }
    fn iter(&self) -> std::collections::vec_deque::Iter<Value> {
        self.0.iter()
    }
}
impl IntoIterator for List {
    type Item = Value;
    type IntoIter = std::collections::vec_deque::IntoIter<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}


#[derive(Debug, Eq, PartialEq)]
struct Vector(Vec<Value>);


#[derive(Debug, Eq, PartialEq)]
struct Map(HashMap<Atom, Value>);


#[derive(Debug, Eq, PartialEq)]
struct Set(HashSet<Atom>);


#[derive(Debug, Hash, Eq, PartialEq)]
enum Number {
    small(Fraction),
}


#[derive(Debug, Hash, Eq, PartialEq)]
enum Atom {
    number(Number),
    ident(String),
    string(String),
    keyword(String),
    nil,
}
impl FromStr for Atom {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        // Strings
        if s.starts_with('"') {
            if s.ends_with('"') {
                let string = s.chars().skip(1).take(s.len() - 2)
                              .collect::<String>();
                return Ok(Atom::string(string));
            } else {
                unreachable!()
            }
        }

        // Keywords
        if s.starts_with(':') {
            return Ok(Atom::keyword(s.into()));
        }

        // Integers
        match s.parse::<i64>() {
            Ok(n) => return Ok(
                    Atom::number(
                        Number::small(Fraction::from(n)))),
            Err(_) => (),
        };

        // Floats
        match s.parse::<f64>() {
            Ok(f) => return Ok(
                    Atom::number(
                        Number::small(Fraction::from(f)))),
            Err(_) => (),
        }

        // Identities
        return Ok(Atom::ident(s.into()));
    }
}


#[derive(Debug, Eq, PartialEq)]
enum Sequence {
    sexp(Sexp),
    list(List),
    vec(Vector),
    map(Map),
    set(Set),
}
impl Sequence {
    pub fn add(&mut self, v: Value) {
        use Sequence::*;
        match self {
            &mut sexp(ref mut s) => {
                s.add(v);
            },
            &mut list(ref mut l) => {
                l.push_back(v);
            },
            _ => unimplemented!(),
        }
    }

    pub fn conj(&mut self, v: Value) {
        match self {
            &mut Sequence::list(ref mut list) => {
                list.push_back(v);
            }
            _ => unimplemented!(),
        }
    }
}


#[derive(Debug, PartialEq, Eq)]
enum Value {
    atom(Atom),
    sequence(Sequence),
}
impl Value {
    pub fn add(&mut self, v: Value) {
        match self {
            &mut Value::sequence(ref mut seq) => seq.add(v),
            _ => unreachable!(),
        }
    }
    pub fn eval(&self) -> Result<Value> {
        return Ok(
            match self {
                &Value::sequence(ref seq) => {
                    match seq {
                        &Sequence::sexp(ref sexp) => {
                            sexp.eval().chain_err(|| "Error parsing sexp")?
                        }
                        _ => bail!("unimplemented!"),
                    }
                }
                _ => bail!("unimplemented!"),
            }
        )
    }
}


/// Find our tokens:
/// ( <sexp> )
/// '( <list literal> )
/// [ <vec literal> ]
/// { <map literal> }
/// #{ <set literal> }
/// " <string literal> "
/// :keyword
/// ident
/// number
//type Tokens = Matches;
fn tokenize(s: &str) -> Matches {
    lazy_static! {
        static ref RE: Regex =
            Regex::new(r##"(\(|#\(|'\(|\)|\[|\]|\{|#\{|\})|[^\s"\)\]\}]+|"([^"\)\]\}]*)""##)
            .unwrap();
    }
    RE.find_iter(s)
}


/// Step through our tokens and try parsing them into Values
fn parse(tokens: &mut Matches, mut seq_val: Value) -> Result<Value> {
    while let Some(token) = tokens.next() {
        let token_str = token.as_str();
        println!("{}", token_str);
        match token_str {
            "(" => {
                let value = parse(tokens, Value::sequence(Sequence::sexp(Sexp::new())))
                            .chain_err(|| "failure parsing list...")?;
                seq_val.add(value);
            },
            // ------ literals ------
            "'(" => {
                // parse list literal
                let value = parse(tokens, Value::sequence(Sequence::list(List::new())))
                            .chain_err(|| "failure parsing list...")?;
                seq_val.add(value);
            },
            //"[" => {
            //    // parse vec literal
            //},
            //"{" => {
            //    // parse map literal
            //},
            //"#{" => {
            //    // parse set literal
            //},
            //"#(" => {
            //    // parse fn literal
            //},
            ")"|"]"|"}" => {
                return Ok(seq_val);
            },
            // parse single item tokens
            t @ _ => {
                seq_val.add(Value::atom(t.parse::<Atom>()
                            .chain_err(|| format!("Error parsing value: {}", t))?));
            },
        }
    };
    //return Ok(Value::sequence(Sequence::sexp(Sexp::new())))
    return Ok(seq_val);
}


fn prompt(rl: &mut Editor<()>, msg: &str) -> Result<Option<String>> {
    let input = match rl.readline(&format!("{} >> ", msg)) {
        Ok(input) => {
            rl.add_history_entry(&input);
            input
        },
        Err(ReadlineError::Interrupted) => {
            println!("CTRL-C");
            bail!("Interrupted!");
        },
        Err(ReadlineError::Eof) => {
            return Ok(None);
        },
        Err(err) => bail!("Error: {:?}", err),
    };
    Ok(Some(input))
}


fn repl() -> Result<()> {
    let history = ".risp_history.txt";
    let mut rl = Editor::<()>::new();
    let _ = rl.load_history(history);
    loop {
        let input = prompt(&mut rl, "risp").chain_err(|| "Error reading user input")?;
        match input {
            None => {
                println!("Exiting...");
                break;
            }
            Some(input) => {
                let mut tokens = tokenize(&input);
                let ast = parse(&mut tokens, Value::sequence(Sequence::sexp(Sexp::new())))
                    .chain_err(|| "Error parsing tokens into s-expressions")?;
                println!("{:#?}", ast);
                let res = ast.eval().chain_err(|| "Error evaluating ast")?;
                println!("{:#?}", res);
            }
        }
    }
    let _ = rl.save_history(history);
    Ok(())
}

fn main() {
    println!("** risp **");
    if let Err(ref e) = repl() {
        use error_chain::ChainedError;
        let stderr = &mut stderr();
        let err_msg = "Error writing to stderr";
        writeln!(stderr, "{}", e.display()).expect(err_msg);
        process::exit(1);
    }
}

use super::tokens::{Token, TokenStream};
use Token::*;
use std::iter::Peekable;
use error_chain::ChainedError;
use lazy_static::lazy_static;
use std::collections::{HashMap};
use error_chain::bail;
use std::borrow::Borrow;
use std::hash::Hash;
#[allow(unused_imports)]
use log::{debug, info, trace, warn, error};

pub mod errors {
    use error_chain::error_chain;
    error_chain! {
        errors {
            UnexpectedToken {
                description("Token not expected")
            }
            EOF {
                description("Unexpected EOF")
            }

        }

        foreign_links {
            Tokenizer(crate::tokens::errors::Error);
        }
        // links {
        //     Tokenizer(crate::tokens::errors::Error, crate::tokens::errors::ErrorKind);
        // }
    }
}

use errors::*;

#[derive(Debug, PartialEq, Clone)]
pub enum Ast {
    Number(f64),
    Binary {
        op: String,
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },
    Func {
        func: String,
        args: Vec<Ast>,
    },
    Var {
        name: String,
    },
    Assign {
        lhs: String,
        rhs: Box<Ast>,
    },
}


pub struct Parser<'a> {
    input: Peekable<TokenStream<'a>>,
}

lazy_static! {
    static ref OP_PRIORITY: HashMap<String, i32> = {
        let mut m = HashMap::new();
        m.insert("=".to_string(), 1);
        m.insert("+".to_string(), 10);
        m.insert("-".to_string(), 10);
        m.insert("*".to_string(), 20);
        m.insert("/".to_string(), 20);
        m.insert("^".to_string(), 30);
        m
    };
}

fn get_priority<S>(op: &S) -> Result<i32>
    where String: Borrow<S>,
          S: Eq + Hash + std::fmt::Display
{
    OP_PRIORITY.get(&op).copied().chain_err(|| format!("Unknown operator {}", op))
}

impl Parser<'_> {

    pub fn from_input(source: &str) -> Parser {
        let tok = TokenStream::from_input(source);
        Parser { input: tok.peekable() }
    }

    pub fn parse_expression(&mut self) -> Result<Box<Ast>> {
        let atomic = self.parse_atomic()?;
        let mut ast = self.maybe_mul(atomic)?;
        ast = match self.peek_clone() {
            Ok(Operator(op)) => self.maybe_binary(ast, get_priority(&op)?)?,
            _ => ast
        };
        Ok(ast)
    }

    fn parse_atomic(&mut self) -> Result<Box<Ast>> {
        let ch = match
            self.input.peek()
            .chain_err(|| ErrorKind::EOF)?
        {
            Ok(x) => x.clone(),
            Err(e) => return Err(format!("Temporary Error message: {}", e.display_chain()).into()),
        };
            // .chain_err(|| "Error in tokenizer")?.clone();
        match ch {
            Number(num) => {
                let _ = self.input.next();
                Ok(Box::new(Ast::Number(num)))
            },
            Var(name) => {
                let _ = self.input.next();
                Ok(Box::new(Ast::Var { name }))
            },
            Punctuation(c) if c == "(" => {
                let _= self.input.next();
                let ast = self.parse_expression();
                match self.input.next() {
                    Some(Ok(Punctuation(c))) if c == ")" => (ast),
                    Some(Ok(x)) => Err(format!("Unexpected Token: {:?}", x).into()),
                    Some(Err(e)) => Err(format!("Toknizer error: {}", e.display_chain()).into()),
                    None => Err("Unexpected EOF".into())
                }
            },
            Keyword(name) => {
                let _ = self.input.next();
                let args = self.parse_args()?;
                Ok(Box::new(Ast::Func { func: name, args}))
            }
            x => Err(format!("Can't handle token {:?} in atomic parser", x).into()),
        }   
    }

    fn parse_args(&mut self) -> Result<Vec<Ast>> {
        let ch = self.input.peek();
        let mut res = Vec::new();
        match ch {
            Some(Ok(Punctuation(c))) if c == "(" => {
                let _ = self.input.next();
            }
            Some(Ok(c)) => bail!(format!("Expected token \"(\" got {:?}", c)),
            Some(Err(e)) => return Err(format!("Tokenizer error: {}", e.display_chain()).into()),
            None => return Err(ErrorKind::EOF.into()),
        };

        loop {
            let el = self.parse_expression()?;
            res.push(*el);

            let ch = self.peek_clone()?;
            match ch {
                Punctuation(c) if c == "," => {
                    let _ = self.input.next();
                    continue;
                },
                Punctuation(c) if c == ")" => {
                    let _ = self.input.next();
                    break;
                },
                x => return Err(format!("Unexpected Token while parsing list: {:?}", x).into())
            };
                
        }
        Ok(res)
    }

    pub fn peek(&mut self) ->  Option<&crate::tokens::errors::Result<Token>> {
        self.input.peek()
    }

    pub fn peek_clone(&mut self) -> Result<Token> {
        match
            self.input.peek()
            .chain_err(|| Error::from_kind(ErrorKind::EOF))?
        {
            Ok(x) => Ok(x.clone()),
            Err(e) => Err(format!("Temporary Error message: {}", e.display_chain()).into()),
        }
    }

    fn maybe_binary(&mut self, left: Box<Ast>, mut priority: i32) -> Result<Box<Ast>> {
        if let Ok(Operator(mut op)) | Ok(Punctuation(mut op)) = self.peek_clone() {

            let mut other_priority = get_priority(&op)?;
            let mut ast = left;

                
            if other_priority > priority || op == "=" {  // assignment should always be right-to-left
                let _ = self.input.next();

                let mut atom = self.parse_atomic()
                    .chain_err(|| "Parser error")?;
                
                atom = self.maybe_mul(atom)?;
                let right = self.maybe_binary(atom, other_priority)
                    .chain_err(|| "Parser error")?;
                if op == "=" {
                    return match *ast {  // match against lhs
                        Ast::Var { name } => Ok(Box::new(
                                Ast::Assign {
                                    lhs: name,
                                    rhs: right,
                                }
                        )),
                        _ => Err("Expected variable name on lhs of assignment".into())
                    }
                }
                Ok(Box::new(
                        Ast::Binary {
                            op,
                            lhs: ast,
                            rhs: right,
                        }
                ))
            }

            else {
                let mut should_break = false;
                while other_priority == priority && !should_break {
                    let _ = self.input.next();
                    let mut  real_next_op = op.clone();

                    let mut right_atom = match self.parse_atomic() {
                        x @ Ok(_) => x,
                        Err(Error(ErrorKind::EOF, _)) => { return Ok(ast); }
                        e => e,
                    }
                        .chain_err(|| "Parser error")?;
                    right_atom = self.maybe_mul(right_atom)?;

                    let right = match self.peek_clone() {
                        Ok(Operator(op)) if op == "=" => {
                            return Err("Expected variable on lhs of assignment".into())
                        }
                        Ok(Operator(next_op)) if get_priority(&next_op)? > priority => {
                            real_next_op = next_op.clone();
                            self.maybe_binary(right_atom, get_priority(&next_op)?)?  // wrap to right
                        }
                        Ok(Operator(next_op)) => {
                            real_next_op = next_op.clone();
                            other_priority = get_priority(&next_op)?;
                            priority = other_priority; // TODO this is disgusting
                            right_atom
                        }
                        _ => {
                            should_break = true;
                            right_atom
                        }
                    };

                    ast = Box::new(Ast::Binary {
                        op: op.to_string(),
                        lhs: ast,
                        rhs: right
                    });
                    op = real_next_op;
                }
            // let _ = self.input.next();
                Ok(ast)
            }

        }
        else {
            Ok(left)
        }
    }

    fn maybe_mul(&mut self, left: Box<Ast>) -> Result<Box<Ast>> {
        match self.peek_clone() {
            Ok(Punctuation(r)) if r == "("  => {
                let mut right = self.parse_atomic()?;
                right = self.maybe_mul(right)?;
                Ok(Box::new(Ast::Binary {
                    op: "*".to_owned(),
                    lhs: left,
                    rhs: right
                }))
            }
            Ok(Var(_)) => {
                let mut right = self.parse_atomic()?;
                right = self.maybe_mul(right)?;
                Ok(Box::new(Ast::Binary {
                    op: "*".to_owned(),
                    lhs: left,
                    rhs: right
                }))
            }
            _ => Ok(left)
        }
    }

}


#[cfg(test)]
mod tests {
    use super::*;

    fn init_log() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn test_parse_atomic() {
        init_log();
        let cases = vec![
            ("15.5", Ast::Number(15.5)),
            ("(2 + 2)", Ast::Binary{ op: "+".to_owned(), lhs: Box::new(Ast::Number(2.0)), rhs: Box::new(Ast::Number(2.0)) }),
            ("x", Ast::Var { name: "x".to_owned() }),
            ("log(10, 100)", Ast::Func {
                func: "log".to_owned(),
                args: vec![Ast::Number(10.0), Ast::Number(100.0)]
            }),
        ];
        for (case, expected) in cases.into_iter() {
            println!("test begins");
            let tok = TokenStream::from_input(case);
            let mut parser = Parser { input: tok.peekable()};
            assert_eq!(expected, *parser.parse_atomic().unwrap());
        }
    }

}


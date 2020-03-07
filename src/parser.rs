use super::tokens::{Token, TokenStream};
use Token::*;
use std::iter::Peekable;
use error_chain::ChainedError;
use lazy_static::lazy_static;
use std::collections::{HashMap, HashSet};
use error_chain::bail;
use std::borrow::Borrow;
use std::hash::Hash;

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

type Env = HashMap<String, Ast>;

pub struct Calculator {
    env: Env,
}

impl Calculator {

    pub fn new() -> Calculator {
        Calculator {
            env: HashMap::new(),
        }
    }

    pub fn eval(&mut self, input: &str) -> Result<f64> {
        let mut parser = Parser::from_str(input);
        let ast = parser.parse_expression()
            .chain_err(|| "Syntax Error: ")?;
        // println!("{:#?}", ast);
        let res = ast.eval(&mut self.env)
            .chain_err(|| "Evaluation error: ")?;
        if let None = parser.input.peek() {
            Ok(res)
        }
        else {
            Err(format!("Unexpected Token: {:?}", parser.input.peek().unwrap()).into())
        }
    }
}


impl Ast {
    pub fn eval(&self, env: &mut Env) -> Result<f64> {
        match self {
            Ast::Number(x) => Ok(*x),
            Ast::Binary { .. } => self.eval_binary(env),
            Ast::Func { .. } => self.eval_func(env),
            Ast::Var { .. } => self.eval_var(env),
            Ast::Assign { .. } => self.eval_assign(env),
        }
    }

    fn dependency_set(&self, env: &Env) -> HashSet<String> {
        match self {
            Ast::Number(_) => HashSet::new(),
            Ast::Binary { lhs, rhs, .. } => {
                let mut res = HashSet::new();
                res.extend(lhs.dependency_set(env));
                res.extend(rhs.dependency_set(env));
                res
            },
            Ast::Func { args, .. } => {
                let mut res = HashSet::new();
                for ast in args {
                    res.extend(ast.dependency_set(env));
                }
                res
            },
            Ast::Var { name } => {
                let mut res = HashSet::new();
                res.insert(name.clone());
                match env.get(name) {
                    Some(a) => {
                        res.extend(a.dependency_set(env));
                        res
                    }
                    // return whatever, evaluation will fail because of unknown variable regardless
                    None => HashSet::new()  
                }
            },
            Ast::Assign { lhs, rhs } => {
                let mut res = rhs.dependency_set(env);
                res.insert(lhs.clone());
                res
            }
        }
    }

    fn eval_assign(&self, env: &mut Env) -> Result<f64> {
        if let Ast::Assign { lhs, rhs } = self {
            // println!("begin assign");
            
            let dep_set = rhs.dependency_set(env);
            // println!("Assigning {} to {:#?}", lhs, rhs);
            // println!("Dependency set: {:?}", &dep_set);
            if dep_set.contains(lhs) {
                bail!("Dependency cycle");
            }
            
            // TODO consider order of evaling res and inserting
            let res = rhs.eval(env);
            env.insert(lhs.to_string(), *rhs.clone());
            // println!("end assign");
            res
        }
        else {
            panic!("eval_assign called on not Ast::Assign");
        }
    }

    fn eval_var(&self, env: &mut Env) -> Result<f64> {
        if let Ast::Var { name } = self {
            let ans = env.get(name).map(|x| x.clone());
            if let Some(ast) = ans {
                // println!("Dependency set of {}: {:?}", &name, &dep_set);
                // println!("Env dump:\n {:#?}", env);
                Ok(ast.eval(env)?)
            }
            else {
                Err(format!("Variable {} not defined", &name).into())
            }
        }
        else {
            panic!("eval_car called on not Ast::Var");
        }
    }

    fn eval_binary(&self, env: &mut Env) -> Result<f64> {
        // TODO because of chaining this tries to parse "=" operator
        if let Ast::Binary { op, lhs, rhs } = self {
            match op.as_ref() {
                "+" => Ok(lhs.eval(env)? + rhs.eval(env)?),
                "-" => Ok(lhs.eval(env)? - rhs.eval(env)?),
                "*" => Ok(lhs.eval(env)? * rhs.eval(env)?),
                "/" => Ok(lhs.eval(env)? / rhs.eval(env)?),
                "^" => Ok(lhs.eval(env)?.powf(rhs.eval(env)?)),
                _ => panic!("Attempt to parse unknown operator: {}", &op),
            }
        }
        else {
            panic!("eval_binary called on not Ast::Binary");
        }
    }

    fn eval_func(&self, env: &mut Env) -> Result<f64> {
        if let Ast::Func { func, args } = self {
            match func.as_ref() {
                "sqrt" => {
                    if args.len() != 1 {
                        Err("Wrong amount of arguments to sqrt".into())
                    }
                    else {
                        Ok(args[0].eval(env)?.sqrt())
                    }
                },
                "sin" => {
                    if args.len() != 1 {
                        Err("Wrong amount of arguments to sin".into())
                    }
                    else {
                        Ok(args[0].eval(env)?.sin())
                    }
                },
                "cos" => {
                    if args.len() != 1 {
                        Err("Wrong amount of arguments to cos".into())
                    }
                    else {
                        Ok(args[0].eval(env)?.cos())
                    }
                },
                "tan" => {
                    if args.len() != 1 {
                        Err("Wrong amount of arguments to tan".into())
                    }
                    else {
                        Ok(args[0].eval(env)?.tan())
                    }
                },
                "ctg" => {
                    if args.len() != 1 {
                        Err("Wrong amount of arguments to ctg".into())
                    }
                    else {
                        Ok(1.0 / args[0].eval(env)?.tan())
                    }
                },
                "tg" => {
                    if args.len() != 1 {
                        Err("Wrong amount of arguments to tg".into())
                    }
                    else {
                        Ok(args[0].eval(env)?.tan())
                    }
                },
                "rad" => {
                    if args.len() != 1 {
                        Err("Wrong amount of arguments to rad".into())
                    }
                    else {
                        Ok(args[0].eval(env)?.to_radians())
                    }
                },
                "deg" => {
                    if args.len() != 1 {
                        Err("Wrong amount of arguments to deg".into())
                    }
                    else {
                        Ok(args[0].eval(env)?.to_degrees())
                    }
                },
                "log" => {
                    if args.len() != 2 {
                        Err("Wrong amount of arguments to deg".into())
                    }
                    else {
                        Ok(args[1].eval(env)?.log(args[0].eval(env)?))
                    }
                },
                _ => unreachable!(),
            }
        }
        else {
            panic!("eval_func called on not Ast::Func");
        }

    }
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
    OP_PRIORITY.get(&op).map(|x| *x).chain_err(|| format!("Unknown operator {}", op))
}

impl Parser<'_> {

    pub fn from_str(source: &str) -> Parser {
        let tok = TokenStream::from_str(source);
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
                Ok(Box::new(Ast::Var { name: name.clone() }))
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
                Ok(Box::new(Ast::Func { func: name.to_string(), args}))
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


    fn peek_clone(&mut self) -> Result<Token> {
        match
            self.input.peek()
            .chain_err(|| Error::from_kind(ErrorKind::EOF))?
        {
            Ok(x) => Ok(x.clone()),
            Err(e) => return Err(format!("Temporary Error message: {}", e.display_chain()).into()),
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
                return Ok(Box::new(
                        Ast::Binary {
                            op: op.to_string(),
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

    #[test]
    fn test_parse_atomic() {
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
            let tok = TokenStream::from_str(case);
            let mut parser = Parser { input: tok.peekable()};
            assert_eq!(expected, *parser.parse_atomic().unwrap());
        }
    }

    #[test]
    fn test_simple_circular_dep() {
        let mut calc = Calculator::new();
        let _ = calc.eval("x = 5");
        let _ = calc.eval("y = 5 + x");
        let env_dump_pre = calc.env.clone();
        let _ = calc.eval("x = y").expect_err("Allowed circular dependency in assignment");
        let env_dump_post = calc.env.clone();
        assert_eq!(env_dump_pre, env_dump_post);
        let _ = calc.eval("x");
    }

    #[test]
    fn test_binary_circular_dep() {
        let mut calc = Calculator::new();

        let _ = calc.eval("x = 5");
        let _ = calc.eval("y = 5 + x");
        let env_dump_pre = calc.env.clone();
        let _ = calc.eval("x = y * 12").expect_err("Allowed circular dependency in assignment");
        let env_dump_post = calc.env.clone();
        assert_eq!(env_dump_pre, env_dump_post);
        let _ = calc.eval("x");
        let res = calc.eval("y").unwrap();
        assert_eq!(res, 10.0);
    }

    #[test]
    fn test_variable_eval() {
        let mut calc = Calculator::new();
        let res1 = calc.eval("x = 5").unwrap();
        assert_eq!(res1, 5.0);
        let res2 = calc.eval("x").unwrap();
        assert_eq!(res2, 5.0);
    }

    // #[test]
    // fn calculator_test() {
    //     let mut calc = Calculator::new();

    // }

    #[test]
    fn test_power() {
        let mut calc = Calculator::new();
        assert_eq!(calc.eval("2 ^ 3").unwrap(), 8.0);
    }

    #[test]
    fn test_implicit_multiplication() {
        let mut calc = Calculator::new();
        let res = calc.eval("2 + 3(4 - 5)").unwrap();
        assert_eq!(res, -1.0);

        match calc.eval("(1+2)(3+4)(5+6)") {
            Ok(x) => assert_eq!(x, 231.0),
            Err(e) => panic!("{}", e.display_chain())
        }
    }

    #[test]
    fn assign_non_existent() {
        let mut calc = Calculator::new();
        let _ = calc.eval("x = a").unwrap_err();
    }

    #[test]
    fn assign_to_assign_value() {
        let mut calc = Calculator::new();
        match calc.eval("x = y = 5") {
            Ok(x) => assert_eq!(x, 5.0),
            Err(e) => panic!("{}", e.display_chain())
        }
    }

    #[test]
    fn equal_priority_chain() {
        let mut calc = Calculator::new();
        match calc.eval("2 + 2 + 2") {
            Ok(x) => assert_eq!(x, 6.0),
            Err(e) => panic!("{}", e.display_chain())
        }
        match calc.eval("2 + 2 + 2 + 2 + 2") {
            Ok(x) => assert_eq!(x, 10.0),
            Err(e) => panic!("{}", e.display_chain())
        }
        match calc.eval("2 * 2 * 2 + 2 + 2") {
            Ok(x) => assert_eq!(x, 12.0),
            Err(e) => panic!("{}", e.display_chain())
        }
        match calc.eval("2 * 2 * 2 + 2 * 2 * 2") {
            Ok(x) => assert_eq!(x, 16.0),
            Err(e) => panic!("{}", e.display_chain())
        }
    }

    #[test]
    fn test_right_chaining() {
        let mut calc = Calculator::new();
        match calc.eval("32 /2 /2 /2  /2") {
            Ok(x) => assert_eq!(x, 2.0),
            Err(e) => panic!("{}", e.display_chain())
        }
    }

    #[ignore]
    #[test]
    fn dunno_yet() {
        let mut calc = Calculator::new();
        match calc.eval("2 * 2 * 2 + 2 = 2") {
            Ok(x) => assert_eq!(x, 2.0),
            Err(e) => panic!("{}", e.display_chain())
        }

    }
    
    #[test]
    fn double_operator_panic_test() {
        let mut calc = Calculator::new();
        match calc.eval("32 // 2") {
            Ok(x) => panic!("Should error, got{}", x),
            Err(_) => ()
        };
    }
}


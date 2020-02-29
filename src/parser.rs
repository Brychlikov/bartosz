use super::tokens::{Token, TokenStream};
use Token::*;
use std::iter::Peekable;
use error_chain::ChainedError;
use lazy_static::lazy_static;
use std::collections::{HashMap, HashSet};
use error_chain::bail;

pub mod errors {
    use error_chain::error_chain;
    error_chain! {
        errors {
            UnexpectedToken {
                description("Token not expected")
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
                res.extend(env[name].dependency_set(env));
                res
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

impl Parser<'_> {

    pub fn from_str(source: &str) -> Parser {
        let tok = TokenStream::from_str(source);
        Parser { input: tok.peekable() }
    }

    pub fn parse_expression(&mut self) -> Result<Box<Ast>> {
        let atomic = self.parse_atomic()?;
        self.maybe_binary(atomic, 0)
    }

    fn parse_atomic(&mut self) -> Result<Box<Ast>> {
        let ch = match
            self.input.peek()
            .chain_err(|| "Unexpected EOF")?
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
                assert_eq!(self.input.next().unwrap().unwrap(), Punctuation(")".to_string())); // TODO replace with result
                ast
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
            None => return Err("Unexpected EOF".into()),
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
            .chain_err(|| "Unexpected EOF")?
        {
            Ok(x) => Ok(x.clone()),
            Err(e) => return Err(format!("Temporary Error message: {}", e.display_chain()).into()),
        }
    }

    fn maybe_binary(&mut self, left: Box<Ast>, priority: i32) -> Result<Box<Ast>> {
        if let Ok(Operator(mut op)) | Ok(Punctuation(mut op)) = self.peek_clone() {

            let other_priority;
            let from_parans;
            if let Ok(Punctuation(_)) = self.peek_clone() {
                if op != "(" {
                    return Ok(left)
                }
                from_parans = true;
                op = "*".to_string();
                other_priority = *OP_PRIORITY.get("*")
                    .chain_err(|| format!("Unknown operator: {}", op))?;
            }
            else {
                from_parans = false;
                other_priority = *OP_PRIORITY.get(&op)
                    .chain_err(|| format!("Unknown operator: {}", op))?;
            }
                
            if other_priority > priority {
                if !from_parans {
                    let _ = self.input.next();
                }
                let atom = self.parse_atomic()
                    .chain_err(|| "Parser error")?;
                let right = self.maybe_binary(atom, other_priority)
                    .chain_err(|| "Parser error")?;
                if op == "=" {
                    return match *left {
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
                            lhs: left,
                            rhs: right,
                        }
                ))
            }
            let _ = self.input.next();

        }
        Ok(left)
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
    }
}


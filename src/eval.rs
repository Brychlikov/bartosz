use crate::parser::{Ast, Parser};
use std::collections::{HashMap, HashSet};
use log::debug;

use error_chain::bail;


pub mod errors {
    use error_chain::error_chain;
    error_chain! {
        errors {
            CircularDep {
                description("Circular dependency")
            }

            UndefinedVar {
                description("Variable not defined")
            }
        }

        foreign_links {
            Tokenizer(crate::tokens::errors::Error);
            Syntax(crate::parser::errors::Error);
        }
    }
}
use errors::*;


type Env = HashMap<String, Ast>;

#[derive(Default)]
pub struct Calculator {
    env: Env,
}

impl Calculator {

    pub fn new() -> Calculator {
        Calculator::default()
    }

    pub fn eval(&mut self, input: &str) -> Result<f64> {
        let mut parser = Parser::from_input(input);
        let ast = parser.parse_expression()
            .chain_err(|| "Syntax Error: ")?;
        debug!("Evaluating {:#?}", ast);
        let res = ast.eval(&mut self.env)
            .chain_err(|| "Evaluation error: ")?;
        if parser.peek().is_none() {
            Ok(res)
        }
        else {
            Err(format!("Unexpected Token: {:?}", parser.peek().unwrap()).into())
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
            
            let dep_set = rhs.dependency_set(env);
            if dep_set.contains(lhs) {
                bail!("Dependency cycle");
            }
            
            // TODO consider order of evaling res and inserting
            let res = rhs.eval(env);
            env.insert(lhs.to_string(), *rhs.clone());
            res
        }
        else {
            panic!("eval_assign called on not Ast::Assign");
        }
    }

    fn eval_var(&self, env: &mut Env) -> Result<f64> {
        if let Ast::Var { name } = self {
            let ans = env.get(name).cloned();
            if let Some(ast) = ans {
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

#[cfg(test)]
mod tests {

    use super::*;
    use error_chain::ChainedError;

    fn init_log() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn test_simple_circular_dep() {
        init_log();
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
        init_log();
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
        init_log();
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
        init_log();
        let mut calc = Calculator::new();
        assert_eq!(calc.eval("2 ^ 3").unwrap(), 8.0);
    }

    #[test]
    fn test_implicit_multiplication() {
        init_log();
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
        init_log();
        let mut calc = Calculator::new();
        let _ = calc.eval("x = a").unwrap_err();
    }

    #[test]
    fn assign_to_assign_value() {
        init_log();
        let mut calc = Calculator::new();
        match calc.eval("x = y = 5") {
            Ok(x) => assert_eq!(x, 5.0),
            Err(e) => panic!("{}", e.display_chain())
        }
    }

    #[test]
    fn equal_priority_chain() {
        init_log();
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
        init_log();
        let mut calc = Calculator::new();
        match calc.eval("32 / 2 /2 /2  /2") {
            Ok(x) => assert_eq!(x, 2.0),
            Err(e) => panic!("{}", e.display_chain())
        }
    }

    #[test]
    fn test_incorrect_assignment() {
        init_log();
        let mut calc = Calculator::new();
        match calc.eval("2 * 2 * 2 + 2 = 2") {
            Ok(x) => panic!("Should error, got {}", x),
            Err(_) => (),
        }

    }
    
    #[test]
    fn double_operator_panic_test() {
        init_log();
        let mut calc = Calculator::new();
        match calc.eval("32 // 2") {
            Ok(x) => panic!("Should error, got{}", x),
            Err(_) => ()
        };
    }

}

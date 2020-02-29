use std::str::Chars;
use std::iter::{Peekable};

pub mod errors {
    use error_chain::error_chain;
    error_chain! {
        errors {
            UnknownCharacter(reason: String) {
                description("Unknown character"),
                display("Unknown character: {}", reason),
            }

            NumberParseError(s: String) {
                description("Error parsing number"),
                display("Error parsing number: {}", s),
            }
        }

        foreign_links {
            ParseError(std::string::ParseError);
        }
    }
}

use errors::*;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Punctuation (String),
    Number (f64),
    Var (String),
    Keyword(String),
    Operator(String),
}

pub struct InputStream<'a>{
    it: Peekable<Chars<'a>>,
}

impl<'a>  InputStream<'a> {
    fn new(s: &'a str) -> InputStream<'a> {
        InputStream {
            it: s.chars().peekable()
        }
    }
}

impl Iterator for InputStream<'_> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        while self.it.peek()?.is_whitespace() {
            let _ = self.it.next()?;
        }
        self.it.next()
    }
}

pub struct TokenStream<'a> {
    input: Peekable<InputStream<'a>>,
}

impl TokenStream<'_> {

    const KEYWORDS: [&'static str; 9] = [
        "sqrt",
        "sin",
        "cos",
        "tan",
        "ctg",
        "tg",
        "rad",
        "deg",
        "log",
    ];

    pub fn from_str(source: &str) -> TokenStream {
        let input = InputStream::new(source);
        TokenStream {
            input: input.peekable(),
        }
    }

    fn read_while<P>(&mut self, predicate: P) -> String
        where P: Fn(&char) -> bool
    {
        let mut res = String::new();
        loop {
            match self.input.peek() {
                Some(c) if predicate(c) => res.push(self.input.next().unwrap()),
                _ => break,
            }
        }
        return res
    }

    fn read_punctuation(&mut self) -> Token {
        let res = self.input.next().unwrap();  // Guaranteed to be punctuarion char, so not None
        Token::Punctuation({
            let mut s = String::new();
            s.push(res);
            s
        })
    }

    fn is_punctuation(c: &char) -> bool {
        "(),".contains(*c)
    }

    fn read_digit(&mut self) -> Result<Token> {
        let res = self.read_while(TokenStream::is_digit_con);
        let num = res.parse::<f64>();
        Ok(Token::Number(
            num.chain_err(|| ErrorKind::NumberParseError(res))?
        ))  // TODO - proper error handling
    }

    fn is_digit(c: &char) -> bool {
        "1234567890".contains(*c)
    }

    fn is_digit_con(c: &char) -> bool {
        "1234567890eE.".contains(*c)  // TODO Exponential notation for neg exponents
    }

    fn read_ident(&mut self) -> Token {
        let res = self.read_while(TokenStream::is_ident_con);
        if TokenStream::is_keyword(&res) {
            Token::Keyword(res)
        } else {
            Token::Var(res)
        }
    }

    fn is_ident_start(c: &char) -> bool {
        c.is_alphabetic() || "_".contains(*c)
    }
    fn is_ident_con(c: &char) -> bool {
        c.is_alphabetic() || c.is_digit(10) || "_".contains(*c)
    }

    fn is_keyword(s: &str) -> bool {
        TokenStream::KEYWORDS.contains(&s)
    }

    fn read_operator(&mut self) -> Token {
        let res = self.read_while(TokenStream::is_operator);
        Token::Operator(res)
    }

    fn is_operator(c: &char) -> bool {
        "*/-+=".contains(*c)
    }

}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        let ch = *self.input.peek()?;

        if TokenStream::is_punctuation(&ch){
            return Some(Ok(self.read_punctuation()));
        }
        else if TokenStream::is_digit(&ch) {
            return Some(self.read_digit());
        }
        else if TokenStream::is_ident_start(&ch) {
            return Some(Ok(self.read_ident()));
        }
        else if TokenStream::is_operator(&ch) {
            return Some(Ok(self.read_operator()));
        }

        Some(Err(ErrorKind::UnknownCharacter(format!("{}", ch)).into()))
    }
}


pub fn parse(source: &str) -> Result<Vec<Token>> {
    let in_stream = InputStream::new(source);
    let token_stream = TokenStream { input: in_stream.peekable() };
    token_stream.collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use error_chain::ChainedError;

    #[test]
    fn test_whitespace_iter() {
        let source = " ab  c        def ";
        let mut it = InputStream::new(source);
        assert_eq!(it.next(), Some('a'));
        assert_eq!(it.next(), Some('b'));
        assert_eq!(it.next(), Some('c'));
        assert_eq!(it.next(), Some('d'));
        assert_eq!(it.next(), Some('e'));
        assert_eq!(it.next(), Some('f'));
        assert_eq!(it.next(), None);
    }

    #[test]
    fn test_take_while() {
        let source = "abcde2qwerty";
        let mut it = InputStream::new(source);
        let mut tokit = TokenStream { input: it.peekable() };
        let res = tokit.read_while(|c| c.is_alphabetic());
        let rem: String = tokit.input.collect();
        assert_eq!(res, "abcde");
        assert_eq!(rem, "2qwerty");
    }

    #[test]
    fn test_tokenizer() {
        let cases = vec![
            ("2+2*2-x*sin(15)", vec![
                Token::Number(2.0),
                Token::Operator("+".to_string()),
                Token::Number(2.0),
                Token::Operator("*".to_string()),
                Token::Number(2.0),
                Token::Operator("-".to_string()),
                Token::Var("x".to_string()),
                Token::Operator("*".to_string()),
                Token::Keyword("sin".to_string()),
                Token::Punctuation("(".to_string()),
                Token::Number(15.0),
                Token::Punctuation(")".to_string()),
            ]),
            ("sin(deg(15))", vec![
                Token::Keyword("sin".to_string()),
                Token::Punctuation("(".to_string()),
                Token::Keyword("deg".to_string()),
                Token::Punctuation("(".to_string()),
                Token::Number(15.0),
                Token::Punctuation(")".to_string()),
                Token::Punctuation(")".to_string()),
            ]),
        ];

        for (s, expected) in cases.iter() {
            match parse(s) {
                Ok(tokens) => assert_eq!(tokens, *expected),
                Err(e) => panic!("Error during tokenization: {}", e.display_chain()),
            };
        }
    }


}


extern crate proc_macro;
use proc_macro::{TokenStream, TokenTree, Ident};
use proc_macro::Group;
use proc_macro::Delimiter;

fn ident_name(item: TokenTree) -> Option<String> {
    match item {
        TokenTree::Ident(i) => Some(i.to_string()),
        _ => None
    }
}

fn get_body(stream: TokenStream) -> Option<TokenTree> {
    stream.into_iter().find(|t| match t{
        TokenTree::Group(g) => g.delimiter() == Delimiter::Brace,
        _ => false,
    })
}

fn get_return_type(stream: TokenStream) -> TokenStream {
    let mut it = stream.into_iter().skip_while(|t| match t {
        TokenTree::Group(g) => g.delimiter() != Delimiter::Parenthesis,
        _ => true,
    });
    let _ = it.next();
    let n = it.next();
    match n {
        Some(TokenTree::Punct(p)) if p.as_char() == '-' => {
            let _ = it.next();
            it.take_while(|t| match t {
                TokenTree::Group(g) => g.delimiter() != Delimiter::Brace,
                _ => true,
            }).collect()
        }
        Some(TokenTree::Group(_)) => "()".parse().unwrap(),
        _ => unreachable!(),
    }
}

fn name_from_token_stream(stream: TokenStream) -> String {
    let mut it = stream.into_iter();
    let cmp = Some(String::from("fn"));
    while let Some(t) = it.next() {
        if ident_name(t) == cmp {
            if let Some(next) = it.next() {
                if let Some(s) = ident_name(next) {
                    return s;
                }
            }
        }
    }
    panic!("Could not find function name")
}

#[proc_macro_attribute]
pub fn noop(_attr: TokenStream, item: TokenStream) -> TokenStream {
    println!("{}", name_from_token_stream(item.clone()));
    item
}

fn wrap_in_delim(item: TokenStream, delim: Delimiter) -> TokenStream {
    let g = proc_macro::Group::new(delim, item);
    TokenTree::Group(g).into()
}

#[proc_macro_attribute]
pub fn wrap_error(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut decl: TokenStream = item.clone().into_iter().take_while(|t| {
        match t {
            TokenTree::Group(g) => g.delimiter() != Delimiter::Brace,
            _ => true,
        }
    }).collect();
    let name = name_from_token_stream(item.clone());
    let return_type = get_return_type(item.clone());
    let mut closure = "|| -> ".parse::<TokenStream>().unwrap();
    closure.extend(return_type);

    let body = get_body(item.clone());
    closure.extend(body);

    let mut paran_closure = wrap_in_delim(closure, Delimiter::Parenthesis);

    let post = format!(
        r#"
        ().chain_err(|| "Error in function: {}")
    "#, name).parse::<TokenStream>().unwrap();

    paran_closure.extend(post);

    let whole_body = wrap_in_delim(paran_closure, Delimiter::Brace);
    decl.extend(whole_body);
    // println!("{}", decl.to_string());
    // println!("{:#?}", decl);
    decl
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

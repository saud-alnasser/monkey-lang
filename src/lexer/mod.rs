pub mod error;

use crate::{
    ast::{Spanned, Token},
    lexer::error::{Error, Result},
};
use chumsky::{prelude::*, Parser};

pub fn parse(input: &str) -> Result<Vec<Spanned<Token>>> {
    symbols()
        .or(keywords())
        .or(literals())
        .or(any().map(Token::ILLEGAL))
        .map_with_span(Spanned::new)
        .padded()
        .repeated()
        .collect()
        .parse(input)
        .map_err(|errors| errors.into_iter().map(Error::from).collect())
}

fn symbols() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    let assign = just('=').to(Token::ASSIGN).labelled("assignment operator");
    let minus = just('-').to(Token::MINUS).labelled("minus operator");
    let plus = just('+').to(Token::PLUS).labelled("plus operator");
    let asterisk = just('*').to(Token::ASTERISK).labelled("asterisk operator");
    let slash = just('/').to(Token::SLASH).labelled("slash operator");
    let bang = just('!').to(Token::BANG).labelled("bang operator");
    let gt = just('>').to(Token::GT).labelled("greater than operator");
    let lt = just('<').to(Token::LT).labelled("less than operator");
    let eq = just("==").to(Token::EQ).labelled("equal to operator");
    let neq = just("!=").to(Token::NEQ).labelled("not equal to operator");
    let lte = just("<=")
        .to(Token::LTE)
        .labelled("less than or equal to operator");
    let gte = just(">=")
        .to(Token::GTE)
        .labelled("greater than or equal to operator");
    let comma = just(',').to(Token::COMMA).labelled("comma");
    let colon = just(':').to(Token::COLON).labelled("colon");
    let semicolon = just(';').to(Token::SEMICOLON).labelled("semicolon");
    let lparen = just('(').to(Token::LPAREN).labelled("left parenthesis");
    let rparen = just(')').to(Token::RPAREN).labelled("right parenthesis");
    let lbrace = just('{').to(Token::LBRACE).labelled("left brace");
    let rbrace = just('}').to(Token::RBRACE).labelled("right brace");
    let lbracket = just('[').to(Token::LBRACKET).labelled("left bracket");
    let rbracket = just(']').to(Token::RBRACKET).labelled("right bracket");

    eq.or(neq)
        .or(lte)
        .or(gte)
        .or(gt)
        .or(lt)
        .or(assign)
        .or(minus)
        .or(plus)
        .or(asterisk)
        .or(slash)
        .or(bang)
        .or(comma)
        .or(colon)
        .or(semicolon)
        .or(lparen)
        .or(rparen)
        .or(lbrace)
        .or(rbrace)
        .or(lbracket)
        .or(rbracket)
}

fn keywords() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    let r#let = just("let").to(Token::LET).labelled("let keyword");
    let r#fn = just("fn").to(Token::FUNCTION).labelled("fn keyword");
    let r#return = just("return").to(Token::RETURN).labelled("return keyword");
    let r#if = just("if").to(Token::IF).labelled("if keyword");
    let r#else = just("else").to(Token::ELSE).labelled("else keyword");
    let r#true = just("true").to(Token::TRUE).labelled("true keyword");
    let r#false = just("false").to(Token::FALSE).labelled("false keyword");

    r#let
        .or(r#fn)
        .or(r#return)
        .or(r#if)
        .or(r#else)
        .or(r#true)
        .or(r#false)
}

fn literals() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    let int = text::int(10)
        .map(|value: String| Token::INT(value.into()))
        .labelled("integer literal");

    let string = just('"')
        .ignore_then(none_of("\"").repeated().collect())
        .then_ignore(just('"'))
        .map(|value: String| Token::STRING(value.into()))
        .labelled("string literal");

    let identifier = text::ident()
        .map(|value: String| Token::IDENTIFIER(value.into()))
        .labelled("identifier");

    int.or(string).or(identifier)
}

#[cfg(test)]
mod tests {
    use super::*;
    use internment::Intern;

    #[test]
    fn test_symbols() {
        let input = "= - + * / ! < > == != <= >= , : ; () {} []";

        let tokens = parse(input).unwrap();

        assert_eq!(tokens.len(), 21);

        let (assign, span) = &tokens[0];

        assert_eq!(*assign, Token::ASSIGN);
        assert_eq!(span.start(), 0);
        assert_eq!(span.end(), 1);

        let (minus, span) = &tokens[1];

        assert_eq!(*minus, Token::MINUS);
        assert_eq!(span.start(), 2);
        assert_eq!(span.end(), 3);

        let (plus, span) = &tokens[2];

        assert_eq!(*plus, Token::PLUS);
        assert_eq!(span.start(), 4);
        assert_eq!(span.end(), 5);

        let (asterisk, span) = &tokens[3];

        assert_eq!(*asterisk, Token::ASTERISK);
        assert_eq!(span.start(), 6);
        assert_eq!(span.end(), 7);

        let (slash, span) = &tokens[4];

        assert_eq!(*slash, Token::SLASH);
        assert_eq!(span.start(), 8);
        assert_eq!(span.end(), 9);

        let (bang, span) = &tokens[5];

        assert_eq!(*bang, Token::BANG);
        assert_eq!(span.start(), 10);
        assert_eq!(span.end(), 11);

        let (lt, span) = &tokens[6];

        assert_eq!(*lt, Token::LT);
        assert_eq!(span.start(), 12);
        assert_eq!(span.end(), 13);

        let (gt, span) = &tokens[7];

        assert_eq!(*gt, Token::GT);
        assert_eq!(span.start(), 14);
        assert_eq!(span.end(), 15);

        let (eq, span) = &tokens[8];

        assert_eq!(*eq, Token::EQ);
        assert_eq!(span.start(), 16);
        assert_eq!(span.end(), 18);

        let (neq, span) = &tokens[9];

        assert_eq!(*neq, Token::NEQ);
        assert_eq!(span.start(), 19);
        assert_eq!(span.end(), 21);

        let (lte, span) = &tokens[10];
        assert_eq!(*lte, Token::LTE);
        assert_eq!(span.start(), 22);
        assert_eq!(span.end(), 24);

        let (gte, span) = &tokens[11];
        assert_eq!(*gte, Token::GTE);
        assert_eq!(span.start(), 25);
        assert_eq!(span.end(), 27);

        let (comma, span) = &tokens[12];
        assert_eq!(*comma, Token::COMMA);
        assert_eq!(span.start(), 28);
        assert_eq!(span.end(), 29);

        let (colon, span) = &tokens[13];
        assert_eq!(*colon, Token::COLON);
        assert_eq!(span.start(), 30);
        assert_eq!(span.end(), 31);

        let (semicolon, span) = &tokens[14];
        assert_eq!(*semicolon, Token::SEMICOLON);
        assert_eq!(span.start(), 32);
        assert_eq!(span.end(), 33);

        let (lparen, span) = &tokens[15];
        assert_eq!(*lparen, Token::LPAREN);
        assert_eq!(span.start(), 34);
        assert_eq!(span.end(), 35);

        let (rparen, span) = &tokens[16];
        assert_eq!(*rparen, Token::RPAREN);
        assert_eq!(span.start(), 35);
        assert_eq!(span.end(), 36);

        let (lbrace, span) = &tokens[17];
        assert_eq!(*lbrace, Token::LBRACE);
        assert_eq!(span.start(), 37);
        assert_eq!(span.end(), 38);

        let (rbrace, span) = &tokens[18];
        assert_eq!(*rbrace, Token::RBRACE);
        assert_eq!(span.start(), 38);
        assert_eq!(span.end(), 39);

        let (lbracket, span) = &tokens[19];
        assert_eq!(*lbracket, Token::LBRACKET);
        assert_eq!(span.start(), 40);
        assert_eq!(span.end(), 41);

        let (rbracket, span) = &tokens[20];
        assert_eq!(*rbracket, Token::RBRACKET);
        assert_eq!(span.start(), 41);
        assert_eq!(span.end(), 42);
    }

    #[test]
    fn test_keywords() {
        let input = "let fn return if else true false";

        let tokens = parse(input).unwrap();

        assert_eq!(tokens.len(), 7);

        let (r#let, span) = &tokens[0];

        assert_eq!(*r#let, Token::LET);
        assert_eq!(span.start(), 0);
        assert_eq!(span.end(), 3);

        let (r#fn, span) = &tokens[1];

        assert_eq!(*r#fn, Token::FUNCTION);
        assert_eq!(span.start(), 4);
        assert_eq!(span.end(), 6);

        let (r#return, span) = &tokens[2];

        assert_eq!(*r#return, Token::RETURN);
        assert_eq!(span.start(), 7);
        assert_eq!(span.end(), 13);

        let (r#if, span) = &tokens[3];

        assert_eq!(*r#if, Token::IF);
        assert_eq!(span.start(), 14);
        assert_eq!(span.end(), 16);

        let (r#else, span) = &tokens[4];

        assert_eq!(*r#else, Token::ELSE);
        assert_eq!(span.start(), 17);
        assert_eq!(span.end(), 21);

        let (r#true, span) = &tokens[5];

        assert_eq!(*r#true, Token::TRUE);
        assert_eq!(span.start(), 22);
        assert_eq!(span.end(), 26);

        let (r#false, span) = &tokens[6];

        assert_eq!(*r#false, Token::FALSE);
        assert_eq!(span.start(), 27);
        assert_eq!(span.end(), 32);
    }

    #[test]
    fn test_literals() {
        let input = "5; \"hello, world!\"; identifier;";

        let tokens = parse(input).unwrap();

        assert_eq!(tokens.len(), 6);

        let (int, span) = &tokens[0];

        assert_eq!(*int, Token::INT(Intern::new("5".into())));
        assert_eq!(span.start(), 0);
        assert_eq!(span.end(), 1);

        let (string, span) = &tokens[2];

        assert_eq!(*string, Token::STRING(Intern::new("hello, world!".into())));
        assert_eq!(span.start(), 3);
        assert_eq!(span.end(), 18);

        let (identifier, span) = &tokens[4];

        assert_eq!(
            *identifier,
            Token::IDENTIFIER(Intern::new("identifier".into()))
        );
        assert_eq!(span.start(), 20);
        assert_eq!(span.end(), 30);
    }

    #[test]
    fn test_whitespace() {
        let input = "  \n ";

        let tokens = parse(input).unwrap();

        assert_eq!(tokens.len(), 0);
    }
}

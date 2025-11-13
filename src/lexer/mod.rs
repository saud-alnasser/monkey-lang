use crate::ast::Token;
use chumsky::{Parser, prelude::*};

pub fn parse(input: &str) -> ParseResult<Vec<Token>, Rich<'_, char>> {
    symbols()
        .or(keywords())
        .or(literals())
        .or(any().map(Token::ILLEGAL))
        .padded()
        .repeated()
        .collect()
        .parse(input)
}

fn symbols<'src>() -> impl Parser<'src, &'src str, Token, extra::Err<Rich<'src, char>>> {
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

fn keywords<'src>() -> impl Parser<'src, &'src str, Token, extra::Err<Rich<'src, char>>> {
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

fn literals<'src>() -> impl Parser<'src, &'src str, Token, extra::Err<Rich<'src, char>>> {
    let int = text::int(10)
        .map(|value: &str| Token::INT(value.parse::<i64>().unwrap().into()))
        .labelled("integer literal");

    let string = just('"')
        .ignore_then(none_of("\"").repeated().collect())
        .then_ignore(just('"'))
        .map(|value: String| Token::STRING(value.into()))
        .labelled("string literal");

    let identifier = text::ident()
        .map(|value: &str| Token::IDENTIFIER(value.to_string().into()))
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

        let result = parse(input);
        let tokens = result.output().unwrap();

        assert_eq!(tokens.len(), 21);

        let assign = &tokens[0];

        assert_eq!(*assign, Token::ASSIGN);

        let minus = &tokens[1];

        assert_eq!(*minus, Token::MINUS);

        let plus = &tokens[2];

        assert_eq!(*plus, Token::PLUS);

        let asterisk = &tokens[3];

        assert_eq!(*asterisk, Token::ASTERISK);

        let slash = &tokens[4];

        assert_eq!(*slash, Token::SLASH);

        let bang = &tokens[5];

        assert_eq!(*bang, Token::BANG);

        let lt = &tokens[6];

        assert_eq!(*lt, Token::LT);

        let gt = &tokens[7];

        assert_eq!(*gt, Token::GT);

        let eq = &tokens[8];

        assert_eq!(*eq, Token::EQ);

        let neq = &tokens[9];

        assert_eq!(*neq, Token::NEQ);

        let lte = &tokens[10];
        assert_eq!(*lte, Token::LTE);

        let gte = &tokens[11];
        assert_eq!(*gte, Token::GTE);

        let comma = &tokens[12];
        assert_eq!(*comma, Token::COMMA);

        let colon = &tokens[13];
        assert_eq!(*colon, Token::COLON);

        let semicolon = &tokens[14];
        assert_eq!(*semicolon, Token::SEMICOLON);

        let lparen = &tokens[15];
        assert_eq!(*lparen, Token::LPAREN);

        let rparen = &tokens[16];
        assert_eq!(*rparen, Token::RPAREN);

        let lbrace = &tokens[17];
        assert_eq!(*lbrace, Token::LBRACE);

        let rbrace = &tokens[18];
        assert_eq!(*rbrace, Token::RBRACE);

        let lbracket = &tokens[19];
        assert_eq!(*lbracket, Token::LBRACKET);

        let rbracket = &tokens[20];
        assert_eq!(*rbracket, Token::RBRACKET);
    }

    #[test]
    fn test_keywords() {
        let input = "let fn return if else true false";

        let result = parse(input);
        let tokens = result.output().unwrap();

        assert_eq!(tokens.len(), 7);

        let r#let = &tokens[0];

        assert_eq!(*r#let, Token::LET);

        let r#fn = &tokens[1];

        assert_eq!(*r#fn, Token::FUNCTION);

        let r#return = &tokens[2];

        assert_eq!(*r#return, Token::RETURN);

        let r#if = &tokens[3];

        assert_eq!(*r#if, Token::IF);

        let r#else = &tokens[4];

        assert_eq!(*r#else, Token::ELSE);

        let r#true = &tokens[5];

        assert_eq!(*r#true, Token::TRUE);

        let r#false = &tokens[6];

        assert_eq!(*r#false, Token::FALSE);
    }

    #[test]
    fn test_literals() {
        let input = "5; \"hello, world!\"; identifier;";

        let result = parse(input);
        let tokens = result.output().unwrap();

        assert_eq!(tokens.len(), 6);

        let int = &tokens[0];

        assert_eq!(*int, Token::INT(5));

        let string = &tokens[2];

        assert_eq!(*string, Token::STRING(Intern::new("hello, world!".into())));

        let identifier = &tokens[4];

        assert_eq!(
            *identifier,
            Token::IDENTIFIER(Intern::new("identifier".into()))
        );
    }

    #[test]
    fn test_whitespace() {
        let input = "  \n ";

        assert!(parse(input).has_errors())
    }
}

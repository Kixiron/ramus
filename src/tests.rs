use crate::{Node, Ramus, RamusToken};

#[test]
fn logos_test() {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum Token {
        InvalidToken,
        EndOfProgram,
        Identifier,
        Private,
        Primitive,
        Protected,
        In,
        Instanceof,
        Accessor,
        Ellipsis,
        ParenOpen,
        ParenClose,
        BraceOpen,
        BraceClose,
        OpAddition,
        OpIncrement,
        OpAssign,
        OpEquality,
        OpStrictEquality,
        FatArrow,
    }

    impl RamusToken for Token {
        fn error() -> Self {
            Self::InvalidToken
        }

        fn end() -> Self {
            Self::EndOfProgram
        }
    }

    static SOURCE: &str = "\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }\
        foobar(protected primitive private instanceof in) { + ++ = == === => }";

    static IDENTIFIERS: &str = "\
        It was the year when they finally immanentized the Eschaton \
        It was the year when they finally immanentized the Eschaton \
        It was the year when they finally immanentized the Eschaton \
        It was the year when they finally immanentized the Eschaton \
        It was the year when they finally immanentized the Eschaton \
        It was the year when they finally immanentized the Eschaton \
        It was the year when they finally immanentized the Eschaton \
        It was the year when they finally immanentized the Eschaton \
        It was the year when they finally immanentized the Eschaton \
        It was the year when they finally immanentized the Eschaton \
        It was the year when they finally immanentized the Eschaton \
        It was the year when they finally immanentized the Eschaton \
        It was the year when they finally immanentized the Eschaton";

    let mut ramus = Ramus::new()
        .branches(vec![
            Node::from_regex("[a-zA-Z_$][a-zA-Z0-9_$]*", Token::Identifier).unwrap(),
            Node::from_str("private", Token::Private),
            Node::from_str("primitive", Token::Primitive),
            Node::from_str("protected", Token::Protected),
            Node::from_str("in", Token::In),
            Node::from_str("instanceof", Token::Instanceof),
            Node::from_str(".", Token::Accessor),
            Node::from_str("...", Token::Ellipsis),
            Node::from_str("(", Token::ParenOpen),
            Node::from_str(")", Token::ParenClose),
            Node::from_str("{", Token::BraceOpen),
            Node::from_str("}", Token::BraceClose),
            Node::from_str("+", Token::OpAddition),
            Node::from_str("++", Token::OpIncrement),
            Node::from_str("=", Token::OpAssign),
            Node::from_str("==", Token::OpEquality),
            Node::from_str("===", Token::OpStrictEquality),
            Node::from_str("=>", Token::FatArrow),
        ])
        .source(IDENTIFIERS);

    println!("{:#?}", ramus);

    for token in &mut ramus {
        println!("{:?}", token);
    }

    ramus.lex(SOURCE);

    for token in &mut ramus {
        println!("{:?}", token);
    }
}

#[test]
fn lexer_test() {
    #[derive(Debug, Copy, Clone)]
    enum TokenType {
        Error,
        End,
        Tree,
        Take,
        Literal,
        TreeBeard,
        Whitespace,
        Digit,
    }

    impl RamusToken for TokenType {
        fn error() -> Self {
            Self::Error
        }

        fn end() -> Self {
            Self::End
        }
    }

    let source = "tree lit treebfsdeard treebeard take literal 43\n";
    let ramus = Ramus::new()
        .branches(vec![
            Node::from_str("tree", TokenType::Tree),
            Node::from_str("treebeard", TokenType::TreeBeard),
            Node::from_str("take", TokenType::Take),
            Node::from_regex("literal|lit", TokenType::Literal).unwrap(),
            Node::from_regex(r#"\s"#, TokenType::Whitespace).unwrap(),
            Node::from_regex(r#"\d"#, TokenType::Digit).unwrap(),
        ])
        .source(source);

    println!("{:#?}", ramus);

    for token in ramus {
        println!("{:?}", token);
    }
}

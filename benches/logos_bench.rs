use criterion::{black_box, criterion_group, criterion_main, Criterion};
use ramus::{Node, Ramus, RamusToken};

fn build<'a, 'b>(source: &'b str) -> Ramus<'a, 'b, Token> {
    Ramus::new()
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
        .source(source)
}

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

static SOURCE: &str = "
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
";

static IDENTIFIERS: &str = "It was the year when they finally immanentized the Eschaton \
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

fn logos_bench(c: &mut Criterion) {
    c.bench_function("Identifiers", |b| {
        let mut ramus = build(IDENTIFIERS);

        b.iter(|| {
            for token in &mut ramus {
                black_box(token);
            }

            ramus.lex(IDENTIFIERS);
        })
    })
    .bench_function("Keywords, Operators and Punctuators", |b| {
        let mut ramus = build(SOURCE);

        b.iter(|| {
            for token in &mut ramus {
                black_box(token);
            }

            ramus.lex(SOURCE);
        })
    });
}

criterion_group!(benches, logos_bench);
criterion_main!(benches);

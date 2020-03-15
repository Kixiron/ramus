#![cfg_attr(not(test), no_std)]

extern crate alloc;

use unicode_segmentation::{Graphemes, UnicodeSegmentation};

use alloc::{vec, vec::Vec};
use core::{fmt, ops::Range};

#[cfg(test)]
mod tests;

#[derive(Clone)]
pub struct Ramus<'a, 'b, T: RamusToken> {
    tree: Node<'a, T>,
    source: Option<(&'b str, PeekableGraphemes<'b>)>,
    index: usize,
    eof: bool,
}

impl<'a, 'b, T: RamusToken> Ramus<'a, 'b, T> {
    #[inline]
    pub fn new() -> Self {
        Self {
            tree: Node::default(),
            source: None,
            index: 0,
            eof: false,
        }
    }

    #[inline]
    pub fn branch(mut self, branch: Node<'a, T>) -> Self {
        self.tree = branch;
        self
    }

    #[inline]
    pub fn branches(mut self, branches: Vec<Node<'a, T>>) -> Self {
        self.tree = Node::merge(Match::new(MatchType::Forward, None), branches);
        self
    }

    #[inline]
    pub fn source(mut self, source: &'b str) -> Self {
        self.lex(source);
        self
    }

    #[inline]
    pub fn lex(&mut self, source: &'b str) {
        self.source = Some((source, PeekableGraphemes::new(source.graphemes(true))));
    }
}

impl<'a, 'b, T: RamusToken> Iterator for Ramus<'a, 'b, T> {
    type Item = Token<'b, T>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((src, graphemes)) = &mut self.source {
            if self.eof {
                None
            } else {
                let start = self.index;
                let mut fallback = None;

                match self
                    .tree
                    .follow(graphemes, src, &mut self.index, &mut fallback)
                {
                    // Fork::End => Some(Token::new(
                    //     &src[start..self.index],
                    //     start..self.index,
                    //     T::end(),
                    // )),
                    Fork::Error { eof } => {
                        self.eof = eof;

                        if &src[start..self.index] == "" {
                            None
                        } else {
                            Some(Token::new(
                                &src[start..self.index],
                                start..self.index,
                                T::error(),
                            ))
                        }
                    }

                    Fork::Unresolved => Some(Token::new(
                        &src[start..self.index],
                        start..self.index,
                        T::error(),
                    )),

                    Fork::Resolved { token, eof } => {
                        self.eof = eof;
                        Some(Token::new(
                            &src[start..self.index],
                            start..self.index,
                            token,
                        ))
                    }
                }
            }
        } else {
            None
        }
    }
}

impl<'a, 'b, T: RamusToken> fmt::Debug for Ramus<'a, 'b, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Ramus")
            .field("tree", &self.tree)
            .field(
                "source",
                &if let Some((src, _)) = &self.source {
                    Some(src)
                } else {
                    None
                },
            )
            .finish()
    }
}

impl<'a, 'b, T: RamusToken> Default for Ramus<'a, 'b, T> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Repetitions {
    ZeroOrOne,
    ZeroOrMore,
    OneOrMore,
    Range(RepetitionRange),
}

#[cfg(feature = "regex")]
impl From<regex_syntax::hir::RepetitionKind> for Repetitions {
    fn from(kind: regex_syntax::hir::RepetitionKind) -> Self {
        use regex_syntax::hir::RepetitionKind;

        match kind {
            RepetitionKind::ZeroOrOne => Self::ZeroOrOne,
            RepetitionKind::ZeroOrMore => Self::ZeroOrMore,
            RepetitionKind::OneOrMore => Self::OneOrMore,
            RepetitionKind::Range(range) => Self::Range(range.into()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RepetitionRange {
    Exactly(u32),
    AtLeast(u32),
    Bounded(u32, u32),
}

#[cfg(feature = "regex")]
impl From<regex_syntax::hir::RepetitionRange> for RepetitionRange {
    fn from(kind: regex_syntax::hir::RepetitionRange) -> Self {
        use regex_syntax::hir::RepetitionRange;

        match kind {
            RepetitionRange::Exactly(exact) => Self::Exactly(exact),
            RepetitionRange::AtLeast(least) => Self::AtLeast(least),
            RepetitionRange::Bounded(from, to) => Self::Bounded(from, to),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Match<'a> {
    ty: MatchType<'a>,
    repetitions: Option<Repetitions>,
}

impl<'a> Match<'a> {
    pub fn new(ty: MatchType<'a>, repetitions: Option<Repetitions>) -> Self {
        Self { ty, repetitions }
    }

    #[inline]
    pub fn is_forward(&self) -> bool {
        self.ty.is_forward()
    }

    #[inline]
    pub fn is_final(&self) -> bool {
        self.ty.is_final()
    }

    #[inline]
    pub fn matches<'b>(&self, string: &'b str) -> bool {
        self.ty.matches(string)
    }
}

#[derive(Debug, Clone)]
pub enum MatchType<'a> {
    All,
    Str(&'a str),
    Char(char),
    Set(Vec<char>),
    Forward,
    Final,
    None,
}

impl<'a> MatchType<'a> {
    #[inline]
    pub fn is_forward(&self) -> bool {
        if let Self::Forward = self {
            true
        } else {
            false
        }
    }

    #[inline]
    pub fn is_final(&self) -> bool {
        if let Self::Final = self {
            true
        } else {
            false
        }
    }

    #[inline]
    pub fn matches<'b>(&self, string: &'b str) -> bool {
        match self {
            Self::All => true,
            Self::Str(s) => s == &string,
            Self::Char(c) => string.len() == 1 && string.chars().nth(0) == Some(*c),
            Self::Set(set) => string.len() == 1 && set.contains(&string.chars().nth(0).unwrap()),
            Self::Forward => true,
            Self::Final => true,
            Self::None => false,
        }
    }
}

impl<'a> PartialEq for MatchType<'a> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Str(s), Self::Char(o)) | (Self::Char(o), Self::Str(s)) => {
                let mut chars = s.chars();
                chars.size_hint().1.unwrap() == 1 && chars.nth(0) == Some(*o)
            }
            (Self::Str(l), Self::Str(r)) => l == r,
            (Self::Char(l), Self::Char(r)) => l == r,
            (Self::All, Self::All) => true,
            (Self::None, Self::None) => true,
            (Self::Forward, Self::Forward) => true,
            (Self::Final, Self::Final) => true,
            // (_, Self::Forward) | (Self::Forward, _) => true,
            (_, _) => false,
        }
    }
}

impl<'a> Eq for MatchType<'a> {}

impl<'a> From<Option<&'a str>> for MatchType<'a> {
    #[inline]
    fn from(opt: Option<&'a str>) -> Self {
        if let Some(s) = opt {
            Self::Str(s)
        } else {
            Self::None
        }
    }
}

#[derive(Debug, Clone)]
pub enum Node<'a, T: RamusToken> {
    Branch(Match<'a>, Vec<Node<'a, T>>),
    Leaf(Leaf<'a, T>),
}

impl<'a, T: RamusToken> Node<'a, T> {
    fn coalesce(&mut self) {
        match self {
            Node::Branch(_, branches) => {
                for branch in branches.drain(..).collect::<Vec<Node<'a, T>>>() {
                    if let Some(idx) = branches
                        .iter()
                        .position(|b| b.get_match() == branch.get_match())
                    {
                        branches[idx].extend_from_slice(&branch.into_nodes());
                    } else {
                        if branch.get_match().is_forward() {
                            branches.extend_from_slice(&branch.into_nodes());
                        } else {
                            branches.push(branch);
                        }
                    }
                }

                for branch in branches {
                    branch.coalesce();
                }
            }

            _ => {}
        }
    }

    #[inline]
    fn token(&self) -> Option<&T> {
        if let Self::Leaf(leaf) = self {
            Some(&leaf.token)
        } else {
            None
        }
    }

    #[inline]
    fn into_nodes(self) -> Vec<Node<'a, T>> {
        match self {
            Self::Branch(_, nodes) => nodes,
            Self::Leaf(leaf) => vec![Node::Leaf(Leaf::new(
                Match::new(MatchType::Final, None),
                leaf.token,
            ))],
        }
    }

    #[inline]
    fn nodes_mut(&mut self) -> &mut Vec<Node<'a, T>> {
        match self {
            Self::Branch(_, branches) => branches,
            leaf @ Self::Leaf(_) => {
                *leaf = Self::Branch(
                    leaf.get_match().clone(),
                    vec![Node::Leaf(Leaf::new(
                        Match::new(MatchType::Final, None),
                        leaf.token().expect("Matched on a leaf").clone(),
                    ))],
                );

                if let Self::Branch(_, branches) = leaf {
                    branches
                } else {
                    unreachable!("Just created the Branch")
                }
            }
        }
    }

    fn follow<'b>(
        &self,
        graphemes: &mut PeekableGraphemes<'b>,
        source: &'b str,
        index: &mut usize,
        fallback: &mut Option<Fork<T>>,
    ) -> Fork<T> {
        let mut eof = false;

        if let Some(current) = graphemes.peek() {
            if let Some(_repetitions) = self.get_match().repetitions {
                todo!("Implement repetitions")
            }

            match self {
                Self::Leaf(leaf) if leaf.is_final() => {
                    // Potential bug here, what happens if there's already a fallback?
                    *fallback = Some(Fork::Resolved {
                        token: leaf.token.clone(),
                        eof,
                    });

                    return Fork::Unresolved;
                }

                Self::Leaf(leaf) if leaf.matches(current) => {
                    let current = graphemes
                        .next()
                        .expect("This token was peeked, so it should exist");

                    *index += current.len();
                    *fallback = None;

                    return Fork::Resolved {
                        token: leaf.token.clone(),
                        eof,
                    };
                }

                Self::Branch(leaf, branches) if leaf.is_forward() => {
                    if let Some(fall) = fallback.take() {
                        for branch in branches {
                            let fork = branch.follow(graphemes, source, index, fallback);
                            if !fork.is_unresolved() {
                                *fallback = None;
                                return fork;
                            }
                        }

                        *fallback = Some(fall);
                    } else {
                        for branch in branches {
                            let fork = branch.follow(graphemes, source, index, fallback);
                            if !fork.is_unresolved() {
                                *fallback = None;
                                return fork;
                            }
                        }
                    }
                }

                Self::Branch(leaf, branches) if leaf.matches(current) => {
                    if let Some(fall) = fallback.take() {
                        let current = graphemes
                            .next()
                            .expect("This token was peeked, so it should exist");
                        let idx = *index;
                        *index += current.len();

                        for branch in branches {
                            let fork = branch.follow(graphemes, source, index, fallback);
                            if !fork.is_unresolved() {
                                *fallback = None;
                                return fork;
                            }
                        }

                        *index = idx;
                        *graphemes = PeekableGraphemes::new((&source[*index..]).graphemes(true));

                        *fallback = Some(fall);
                    } else {
                        let current = graphemes
                            .next()
                            .expect("This token was peeked, so it should exist");
                        *index += current.len();

                        for branch in branches {
                            let fork = branch.follow(graphemes, source, index, fallback);
                            if !fork.is_unresolved() {
                                *fallback = None;
                                return fork;
                            }
                        }
                    }
                }

                _ => return Fork::Unresolved,
            }
        } else {
            eof = true;
        }

        if fallback.is_some() {
            return fallback.take().expect("Already checked that it's Some");
        }

        if let Some(err) = graphemes.next() {
            *index += err.len();
        }

        Fork::Error { eof }
    }

    pub fn from_str(string: &'a str, ty: T) -> Node<'a, T> {
        let mut graphemes = string.graphemes(true);

        Self::Branch(
            Match::new(graphemes.next().into(), None),
            if string.len() == 1 {
                Vec::new()
            } else {
                vec![Node::from(Self::from_graphemes(graphemes, ty))]
            },
        )
    }

    fn from_graphemes(mut graphemes: Graphemes<'a>, ty: T) -> Self {
        let fork = graphemes.next().unwrap();

        if graphemes.size_hint().1.unwrap() == 0 {
            Self::Leaf(Leaf::new(Match::new(MatchType::Str(fork), None), ty))
        } else {
            Self::Branch(
                Match::new(MatchType::Str(fork), None),
                vec![Self::from_graphemes(graphemes, ty)],
            )
        }
    }

    pub fn get_match(&self) -> &Match<'a> {
        match self {
            Self::Leaf(leaf) => &leaf.leaf,
            Self::Branch(leaf, _) => leaf,
        }
    }

    pub fn into_leaf(self) -> Option<Leaf<'a, T>> {
        if let Self::Leaf(leaf) = self {
            Some(leaf)
        } else {
            None
        }
    }

    pub fn push(&mut self, other: Self) {
        match self {
            leaf @ Self::Leaf(_) => {
                let mut temp = Self::Branch(leaf.get_match().clone(), Vec::with_capacity(2));
                core::mem::swap(leaf, &mut temp);

                if let Self::Branch(_, branches) = leaf {
                    branches.extend_from_slice(&[
                        other,
                        Self::Leaf(
                            temp.into_leaf()
                                .expect("Already matched as a leaf")
                                .leaf(Match::new(MatchType::Final, None)),
                        ),
                    ]);
                } else {
                    unreachable!("Created as a Branch")
                }
            }
            Self::Branch(_, branches) => branches.push(other),
        }
    }

    pub fn extend_from_slice(&mut self, other: &[Self]) {
        match self {
            leaf @ Self::Leaf(_) => {
                let mut temp = Self::Branch(leaf.get_match().clone(), Vec::with_capacity(2));
                core::mem::swap(leaf, &mut temp);

                if let Self::Branch(_, branches) = leaf {
                    branches.push(Self::Leaf(
                        temp.into_leaf()
                            .expect("Already matched as a leaf")
                            .leaf(Match::new(MatchType::Final, None)),
                    ));
                    branches.extend_from_slice(other);
                } else {
                    unreachable!("Created as a Branch")
                }
            }
            Self::Branch(_, branches) => {
                branches.extend_from_slice(other);
            }
        }
    }

    #[inline]
    pub fn merge(leaf: Match<'a>, branches: Vec<Self>) -> Self {
        let mut branches = Self::Branch(leaf, branches);
        branches.coalesce();

        branches
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        if let Self::Branch(_, branches) = self {
            branches.is_empty()
        } else {
            false
        }
    }

    #[inline]
    pub fn nth(&self, n: usize) -> Option<&Self> {
        if let Self::Branch(_, branches) = self {
            if n < branches.len() {
                Some(&branches[n])
            } else {
                None
            }
        } else {
            None
        }
    }

    #[inline]
    pub fn nth_mut(&mut self, n: usize) -> Option<&mut Self> {
        if let Self::Branch(_, branches) = self {
            if n < branches.len() {
                Some(&mut branches[n])
            } else {
                None
            }
        } else {
            None
        }
    }
}

#[cfg(feature = "regex")]
impl<'a, T: RamusToken> Node<'a, T> {
    #[inline]
    pub fn from_regex(regex: &str, ty: T) -> Result<Self, regex_syntax::Error> {
        use regex_syntax::Parser;

        let hir = Parser::new().parse(regex)?.into_kind();

        let mut branch = Self::apply_hir(ty, hir, None);
        branch.coalesce();

        Ok(branch)
    }

    #[inline]
    fn apply_hir(
        ty: impl Into<Option<T>>,
        hir: regex_syntax::hir::HirKind,
        repetitions: Option<Repetitions>,
    ) -> Self {
        use regex_syntax::hir::{self, Class, Hir, HirKind, Literal};

        let ty = ty.into();
        match hir {
            HirKind::Empty => {
                if let Some(ty) = ty {
                    Self::Leaf(Leaf::new(Match::new(MatchType::All, repetitions), ty))
                } else {
                    Self::Branch(Match::new(MatchType::All, repetitions), Vec::new())
                }
            }

            HirKind::Literal(lit) => {
                if let Some(ty) = ty {
                    Self::Leaf(Leaf::new(
                        Match::new(
                            MatchType::Char(match lit {
                                Literal::Byte(b) => b as char,
                                Literal::Unicode(u) => u,
                            }),
                            repetitions,
                        ),
                        ty,
                    ))
                } else {
                    Self::Branch(
                        Match::new(
                            MatchType::Char(match lit {
                                Literal::Byte(b) => b as char,
                                Literal::Unicode(u) => u,
                            }),
                            repetitions,
                        ),
                        Vec::new(),
                    )
                }
            }

            HirKind::Concat(mut seq) => {
                if seq.iter().all(|hir| {
                    if let HirKind::Literal(_) = hir.kind() {
                        true
                    } else {
                        false
                    }
                }) {
                    let mut last = if let Some(ty) = ty {
                        Self::Leaf(Leaf::new(
                            Match::new(
                                MatchType::Char(
                                    if let HirKind::Literal(lit) =
                                        seq.remove(seq.len() - 1).into_kind()
                                    {
                                        match lit {
                                            Literal::Byte(b) => b as char,
                                            Literal::Unicode(u) => u,
                                        }
                                    } else {
                                        unreachable!()
                                    },
                                ),
                                repetitions,
                            ),
                            ty,
                        ))
                    } else {
                        Self::Branch(
                            Match::new(
                                MatchType::Char(
                                    if let HirKind::Literal(lit) =
                                        seq.remove(seq.len() - 1).into_kind()
                                    {
                                        match lit {
                                            Literal::Byte(b) => b as char,
                                            Literal::Unicode(u) => u,
                                        }
                                    } else {
                                        unreachable!()
                                    },
                                ),
                                repetitions,
                            ),
                            Vec::new(),
                        )
                    };

                    for lit in seq.into_iter().rev() {
                        if let HirKind::Literal(lit) = lit.into_kind() {
                            let character = match lit {
                                Literal::Byte(b) => b as char,
                                Literal::Unicode(u) => u,
                            };

                            let mut next = Self::Branch(
                                Match::new(MatchType::Char(character), repetitions),
                                Vec::with_capacity(1),
                            );
                            core::mem::swap(&mut last, &mut next);
                            last.push(next);
                        }
                    }

                    last
                } else {
                    let mut nodes = seq
                        .into_iter()
                        .map(|hir| Self::apply_hir(None, hir.into_kind(), repetitions))
                        .collect::<Vec<Self>>();

                    if let Some(ty) = ty {
                        nodes
                            .last_mut()
                            .expect("Concat should be two or greater nodes")
                            .push(Self::Leaf(Leaf::new(
                                Match::new(MatchType::Final, repetitions),
                                ty,
                            )));
                    }

                    let mut root = nodes.remove(0);
                    while let Some(node) = nodes.pop() {
                        if root.is_empty() {
                            root.push(node);
                        } else {
                            fn traverse<'a, T: RamusToken>(
                                root: &mut Node<'a, T>,
                                node: Node<'a, T>,
                            ) {
                                for n in root.nodes_mut() {
                                    if n.is_empty() {
                                        n.push(node.clone());
                                    } else {
                                        traverse(n, node.clone());
                                    }
                                }
                            }

                            traverse(&mut root, node);
                        }
                    }

                    root
                }
            }

            HirKind::Class(class) => match class {
                Class::Unicode(uni) => {
                    let mut set = Vec::new();

                    for range in uni.iter() {
                        set.reserve(range.end() as usize - range.start() as usize);
                        for c in range.start() as u32..=range.end() as u32 {
                            set.push(core::char::from_u32(c).unwrap());
                        }
                    }

                    if let Some(ty) = ty {
                        Self::Leaf(Leaf::new(Match::new(MatchType::Set(set), repetitions), ty))
                    } else {
                        Self::Branch(Match::new(MatchType::Set(set), repetitions), Vec::new())
                    }
                }
                Class::Bytes(bytes) => {
                    let mut set = Vec::new();

                    for range in bytes.iter() {
                        set.reserve(range.end() as usize - range.start() as usize);
                        for c in range.start() as u32..=range.end() as u32 {
                            set.push(core::char::from_u32(c).unwrap());
                        }
                    }

                    if let Some(ty) = ty {
                        Self::Leaf(Leaf::new(Match::new(MatchType::Set(set), repetitions), ty))
                    } else {
                        Self::Branch(Match::new(MatchType::Set(set), repetitions), Vec::new())
                    }
                }
            },

            HirKind::Alternation(paths) => {
                let mut branches = Vec::with_capacity(paths.len());

                for hir in paths.into_iter().map(Hir::into_kind) {
                    branches.push(Self::apply_hir(ty.clone(), hir, repetitions))
                }

                Self::Branch(Match::new(MatchType::Forward, repetitions), branches)
            }

            HirKind::Repetition(hir::Repetition { kind, hir, .. }) => {
                let hir = hir.into_kind();
                let kind = kind.into();

                Self::apply_hir(ty, hir, Some(kind))
            }

            r => unimplemented!("{:?}", r),
        }
    }
}

impl<'a, T: RamusToken> From<Leaf<'a, T>> for Node<'a, T> {
    #[inline]
    fn from(leaf: Leaf<'a, T>) -> Self {
        Self::Leaf(leaf)
    }
}

impl<'a, T: RamusToken> Default for Node<'a, T> {
    #[inline]
    fn default() -> Self {
        Self::Branch(Match::new(MatchType::Forward, None), Vec::new())
    }
}

#[derive(Debug, Clone)]
pub struct Leaf<'a, T: RamusToken> {
    leaf: Match<'a>,
    token: T,
}

impl<'a, T: RamusToken> Leaf<'a, T> {
    #[inline]
    pub fn new(leaf: Match<'a>, token: T) -> Self {
        Self { leaf, token }
    }

    #[inline]
    pub fn token(mut self, token: T) -> Self {
        self.token = token;
        self
    }

    #[inline]
    pub fn leaf(mut self, leaf: Match<'a>) -> Self {
        self.leaf = leaf;
        self
    }

    #[inline]
    pub fn matches<'b>(&self, string: &'b str) -> bool {
        self.leaf.matches(string)
    }

    #[inline]
    pub fn is_final<'b>(&self) -> bool {
        self.leaf.is_final()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Fork<T: RamusToken> {
    Unresolved,
    Resolved { token: T, eof: bool },
    Error { eof: bool },
    // End,
}

impl<T: RamusToken> Fork<T> {
    #[inline]
    pub fn is_unresolved(&self) -> bool {
        if let Self::Unresolved = self {
            true
        } else {
            false
        }
    }
}

pub trait RamusToken: Clone + fmt::Debug {
    fn error() -> Self;
    fn end() -> Self;
}

#[derive(Debug, Clone)]
pub struct Token<'b, T: RamusToken> {
    source: &'b str,
    range: Range<usize>,
    ty: T,
}

impl<'b, T: RamusToken> Token<'b, T> {
    pub fn new(source: &'b str, range: Range<usize>, ty: T) -> Self {
        Self { source, range, ty }
    }
}

#[derive(Clone)]
struct PeekableGraphemes<'a> {
    iter: Graphemes<'a>,
    peeked: Option<Option<<Graphemes<'a> as Iterator>::Item>>,
}

impl<'a> PeekableGraphemes<'a> {
    #[inline]
    pub fn new(iter: Graphemes<'a>) -> Self {
        Self { iter, peeked: None }
    }

    #[inline]
    pub fn peek(&mut self) -> Option<&<Graphemes<'a> as Iterator>::Item> {
        let iter = &mut self.iter;
        self.peeked.get_or_insert_with(|| iter.next()).as_ref()
    }
}

impl<'a> Iterator for PeekableGraphemes<'a> {
    type Item = <Graphemes<'a> as Iterator>::Item;

    #[inline]
    fn next(&mut self) -> Option<<Graphemes<'a> as Iterator>::Item> {
        match self.peeked.take() {
            Some(v) => v,
            None => self.iter.next(),
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let peek_len = match self.peeked {
            Some(None) => return (0, Some(0)),
            Some(Some(_)) => 1,
            None => 0,
        };
        let (lo, hi) = self.iter.size_hint();
        let lo = lo.saturating_add(peek_len);
        let hi = match hi {
            Some(x) => x.checked_add(peek_len),
            None => None,
        };
        (lo, hi)
    }
}

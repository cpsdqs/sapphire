use super::grammar::{Cursor, TokenType};
use std::error::Error;
use std::fmt;

/// Describes an expected token.
#[derive(Debug, Clone, PartialEq)]
pub enum Expected {
    Token(TokenType),
    OneOfToken(&'static [TokenType]),
    OneOf(Vec<Expected>),
    Separator,
    Expression,
    Identifier,
    NonConstantIdentifier,
    Arguments,
    ArgumentsWithoutParens,
    PositionalArgument,
    Block,
    Lambda,
    BlockParameterList,
    SuperExpression,
    YieldExpression,
    AssignmentExpression,
    AssignmentStatement,
    SingleAssignmentExpression,
    SingleAssignmentStatement,
    ScopedConstantAssignmentExpression,
    ScopedConstantAssignmentStatement,
    SingleMethodAssignmentExpression,
    SingleMethodAssignmentStatement,
    AbbreviatedAssignmentExpression,
    AbbreviatedAssignmentStatement,
    AbbreviatedMethodAssignmentExpression,
    AbbreviatedMethodAssignmentStatement,
    MultipleAssignmentStatement,
    OneToPackingAssignmentStatement,
    ManyToManyAssignmentStatement,
    LeftHandSide,
    MultipleLeftHandSide,
    MultipleLeftHandSideNotPacking,
    MultipleLeftHandSideItem,
    MultipleRightHandSide,
    PrimaryExpression,
    ThenClause,
    CaseExpression,
    WhenClause,
    WhenArgument,
    RescueClause,
    ConditionalOperatorExpression,
    DoClause,
    ForVariable,
    ExceptionClassList,
    VariableReference,
    ScopedConstantReference,
    PseudoVariable,
    RangeConstructor,
    Statement,
    RescueFallbackStatement,
    ModulePath,
    ClassPath,
    DefinedMethodName,
    MethodParameters,
    MethodNameOrSymbol,
    Singleton,
    MethodParameterPart,
    KeywordLogicalExpression,
    KeywordNotExpression,
    OperatorAndExpression,
    OperatorOrExpression,
    PrimaryMethodInvocation,
    MethodName,
    MethodInvocationWithoutParentheses,
    Command,
    ChainedMethodInvocation,
    CommandWithDoBlock,
    IndexingArgumentList,
    ArgumentList,
    LambdaParameter,
    YieldWithOptionalArgument,
    OperatorExpression,
    UnaryMinusExpression,
    UnaryExpression,
    RelationalExpression,
    BitwiseOrExpression,
    BitwiseAndExpression,
    BitwiseShiftExpression,
    AdditiveExpression,
    MultiplicativeExpression,
    PowerExpression,
    StatementNotAllowedInFallbackStatement,
    ParameterList,
    Parameter,
    Association,
}

impl fmt::Display for Expected {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expected::*;
        match self {
            Token(t) => write!(f, "{}", t),
            OneOfToken(t) => {
                for (index, item) in t.iter().enumerate() {
                    match index {
                        0 => write!(f, "{}", item)?,
                        i if i == t.len() - 1 => {
                            if t.len() > 2 {
                                write!(f, ", or {}", item)?;
                            } else {
                                write!(f, " or {}", item)?;
                            }
                        }
                        _ => write!(f, ", {}", item)?,
                    }
                }
                Ok(())
            }
            OneOf(t) => {
                for (index, item) in t.iter().enumerate() {
                    match index {
                        0 => write!(f, "{}", item)?,
                        i if i == t.len() - 1 => {
                            if t.len() > 2 {
                                write!(f, ", or {}", item)?;
                            } else {
                                write!(f, " or {}", item)?;
                            }
                        }
                        _ => write!(f, ", {}", item)?,
                    }
                }
                Ok(())
            }
            Separator => write!(f, "separator"),
            Expression => write!(f, "expression"),
            Identifier => write!(f, "identifier"),
            NonConstantIdentifier => write!(f, "non-constant identifier"),
            Arguments => write!(f, "arguments"),
            ArgumentsWithoutParens => write!(f, "arguments (without parentheses)"),
            PositionalArgument => write!(f, "positional argument"),
            Block => write!(f, "block"),
            Lambda => write!(f, "lambda"),
            BlockParameterList => write!(f, "block parameters"),
            SuperExpression => write!(f, "super expression"),
            YieldExpression => write!(f, "yield expression"),
            AssignmentExpression => write!(f, "assignment expression"),
            AssignmentStatement => write!(f, "assignment statement"),
            SingleAssignmentExpression => write!(f, "single assignment expression"),
            SingleAssignmentStatement => write!(f, "single assignment statement"),
            ScopedConstantAssignmentExpression => {
                write!(f, "scoped constant assignment expression")
            }
            ScopedConstantAssignmentStatement => write!(f, "scoped constant assignment statement"),
            SingleMethodAssignmentExpression => write!(f, "single method assignment expression"),
            SingleMethodAssignmentStatement => write!(f, "single method assignment statement"),
            AbbreviatedAssignmentExpression => write!(f, "abbreviated assignment expression"),
            AbbreviatedAssignmentStatement => write!(f, "abbreviated assignment statement"),
            AbbreviatedMethodAssignmentExpression => {
                write!(f, "abbreviated method assignment expression")
            }
            AbbreviatedMethodAssignmentStatement => {
                write!(f, "abbreviated method assignment statement")
            }
            MultipleAssignmentStatement => write!(f, "multiple assignment statement"),
            OneToPackingAssignmentStatement => write!(f, "one to packing assignment statement"),
            ManyToManyAssignmentStatement => write!(f, "many to many assignment statement"),
            LeftHandSide => write!(f, "left-hand side"),
            MultipleLeftHandSide => write!(f, "multiple left-hand side"),
            MultipleLeftHandSideNotPacking => write!(f, "multiple left-hand side (not packing)"),
            MultipleLeftHandSideItem => write!(f, "multiple left-hand side item"),
            MultipleRightHandSide => write!(f, "multiple right-hand side"),
            PrimaryExpression => write!(f, "primary expression"),
            ThenClause => write!(f, "then clause"),
            CaseExpression => write!(f, "case expression"),
            WhenClause => write!(f, "when clause"),
            WhenArgument => write!(f, "when argument"),
            RescueClause => write!(f, "rescue clause"),
            ConditionalOperatorExpression => write!(f, "conditional operator expression"),
            DoClause => write!(f, "do clause"),
            ForVariable => write!(f, "for variable"),
            ExceptionClassList => write!(f, "exception class list"),
            VariableReference => write!(f, "variable reference"),
            ScopedConstantReference => write!(f, "scoped constant reference"),
            PseudoVariable => write!(f, "pseudo-variable"),
            RangeConstructor => write!(f, "range constructor"),
            Statement => write!(f, "statement"),
            RescueFallbackStatement => write!(f, "rescue fallback statement"),
            ModulePath => write!(f, "module path"),
            ClassPath => write!(f, "class path"),
            DefinedMethodName => write!(f, "defined method name"),
            MethodParameters => write!(f, "method parameters"),
            MethodNameOrSymbol => write!(f, "method name or symbol"),
            Singleton => write!(f, "singleton expression"),
            MethodParameterPart => write!(f, "method parameter part"),
            KeywordLogicalExpression => write!(f, "expression (logical)"),
            KeywordNotExpression => write!(f, "expression (not)"),
            OperatorAndExpression => write!(f, "expression (&&)"),
            OperatorOrExpression => write!(f, "expression (||)"),
            PrimaryMethodInvocation => write!(f, "primary method invocation"),
            MethodName => write!(f, "method name"),
            MethodInvocationWithoutParentheses => {
                write!(f, "method invocation without parentheses")
            }
            Command => write!(f, "command"),
            ChainedMethodInvocation => write!(f, "chained method invocation"),
            CommandWithDoBlock => write!(f, "command with do block"),
            IndexingArgumentList => write!(f, "indexing argument list"),
            ArgumentList => write!(f, "argument list"),
            LambdaParameter => write!(f, "lambda parameter"),
            YieldWithOptionalArgument => write!(f, "yield with optional argument"),
            OperatorExpression => write!(f, "expression (operator)"),
            UnaryMinusExpression => write!(f, "expression (unary minus)"),
            UnaryExpression => write!(f, "expression (unary)"),
            RelationalExpression => write!(f, "relational expression"),
            BitwiseOrExpression => write!(f, "expression (bitwise or)"),
            BitwiseAndExpression => write!(f, "expression (bitwise and)"),
            BitwiseShiftExpression => write!(f, "expression (bitwise shift)"),
            AdditiveExpression => write!(f, "expression (additive)"),
            MultiplicativeExpression => write!(f, "expression (multiplicative)"),
            PowerExpression => write!(f, "expression (power)"),
            StatementNotAllowedInFallbackStatement => {
                write!(f, "statement not allowed in fallback statement")
            }
            ParameterList => write!(f, "parameter list"),
            Parameter => write!(f, "parameter"),
            Association => write!(f, "association"),
        }
    }
}

/// A parse error.
#[derive(Debug, Clone, PartialEq)]
pub enum ParseError<'input> {
    Unexpected(Cursor<'input, ()>, Expected),
    Alt(Box<ParseError<'input>>, Vec<ParseError<'input>>),
}

impl<'input> ParseError<'input> {
    pub(crate) fn alt_merge(self, alt: ParseError<'input>) -> ParseError<'input> {
        match self {
            ParseError::Alt(a, mut v) => {
                v.push(alt);
                ParseError::Alt(a, v)
            }
            _ => panic!("invalid alt-merge"),
        }
    }

    fn cursor(&self) -> Cursor<'input, ()> {
        match self {
            ParseError::Unexpected(p, ..) => *p,
            ParseError::Alt(p, ..) => p.cursor(),
        }
    }

    fn expected(&self) -> &Expected {
        match self {
            ParseError::Unexpected(_, e) => e,
            ParseError::Alt(e, _) => e.expected(),
        }
    }

    /// Returns the error from the most successful parser.
    fn most_successful(&self) -> ParseError<'input> {
        match self {
            ParseError::Alt(_, sub_errors) => {
                let mut max_pos = 0;
                let mut max_indices = Vec::new();

                for (i, err) in sub_errors.iter().enumerate() {
                    let pos = err.cursor().pos();
                    if pos > max_pos {
                        max_pos = pos;
                        max_indices.clear();
                    }
                    if pos == max_pos {
                        max_indices.push(i);
                    }
                }

                if max_indices.len() == 1 {
                    sub_errors[max_indices[0]].clone()
                } else {
                    let mut expected = Vec::new();

                    fn push_expected(out: &mut Vec<Expected>, e: &Expected) {
                        match e {
                            Expected::OneOf(e) => {
                                for sub_e in e {
                                    push_expected(out, sub_e);
                                }
                            }
                            Expected::OneOfToken(tokens) => {
                                for token in tokens.iter() {
                                    push_expected(out, &Expected::Token(*token));
                                }
                            }
                            e => {
                                if !out.contains(e) {
                                    out.push(e.clone());
                                }
                            }
                        }
                    }

                    for err in sub_errors {
                        push_expected(&mut expected, err.expected());
                    }
                    let expected = Expected::OneOf(expected);
                    ParseError::Unexpected(sub_errors[0].cursor(), expected)
                }
            }
            err => err.clone(),
        }
    }

    /// Replaces this error with the result of [`most_successful`}.
    ///
    /// Without this, parse errors got ridiculously large and were practically useless walls of
    /// text.
    pub(crate) fn reduce(self) -> ParseError<'input> {
        self.most_successful()
    }

    /// Returns the error line number, line, and inline range of the error in characters.
    fn error_line<'a>(&self, src: &'a str) -> (usize, &'a str, (usize, usize)) {
        let (begin, end) = match self.cursor().get() {
            Some(Ok((begin, _, end))) => (*begin, *end),
            Some(Err(_)) => {
                let cursor = self.cursor();
                let mut end = 0;
                for i in 0..cursor.pos() {
                    match cursor.buffer().get(cursor.pos() - i) {
                        Some(Ok((_, _, e))) => {
                            end = *e;
                            break;
                        }
                        Some(Err(_)) => (),
                        None => (),
                    }
                }
                (end, end + 1)
            }
            None => (src.len(), src.len()),
        };

        let mut line_number = 1;
        let mut line_byte_bounds = 0..src.len();
        for (i, c) in src.char_indices() {
            if c == '\n' {
                if i <= begin {
                    line_byte_bounds.start = i + 1;
                    line_number += 1;
                }
                if i >= end && i < line_byte_bounds.end {
                    line_byte_bounds.end = i;
                    break;
                }
            }
        }
        let inline_start_bytes = begin - line_byte_bounds.start;
        let inline_end_bytes = end - line_byte_bounds.start;

        let line = &src[line_byte_bounds];

        let inline_start = line
            .char_indices()
            .enumerate()
            .find(|(_, (byte_pos, _))| *byte_pos == inline_start_bytes)
            .map(|(char_pos, _)| char_pos)
            .unwrap_or_else(|| line.chars().count());
        let inline_end = line
            .char_indices()
            .enumerate()
            .find(|(_, (byte_pos, _))| *byte_pos == inline_end_bytes)
            .map(|(char_pos, _)| char_pos)
            .unwrap_or_else(|| line.chars().count());

        (line_number, line, (inline_start, inline_end))
    }

    /// Formats a line with arrows like this:
    ///
    /// ```text
    ///  23 | def a, b, *c; end
    ///       ^-------------^^^
    /// @     |-super_start | |-end
    /// @                   |-start
    /// ```
    ///
    /// Returns the formatted line and the line prefix length.
    ///
    /// # Panics
    /// - if not `super_start <= start <= end`
    fn fmt_line_with_arrows(
        line_number: usize,
        line: &str,
        super_start: usize,
        start: usize,
        end: usize,
        ansi: bool,
    ) -> (String, usize) {
        let line_prefix = format!(" {} | ", line_number);
        let line_prefix_len = line_prefix.len();
        let line_prefix = if ansi {
            format!("\x1b[1;34m{}\x1b[m", line_prefix)
        } else {
            line_prefix
        };

        let line_fmt = if ansi {
            let mut contents = String::new();
            for (i, c) in line.char_indices() {
                if i == start {
                    contents += "\x1b[31m";
                }
                if i == end {
                    contents += "\x1b[m";
                }
                contents.push(c);
            }
            contents
        } else {
            String::from(line)
        };

        let arrow_prefix = " ".repeat(line_prefix_len + super_start);
        let super_arrows = if start - super_start == 0 {
            String::new()
        } else {
            let dashes = "-".repeat((start - super_start).saturating_sub(1));
            if ansi {
                format!("\x1b[31m^{}\x1b[m", dashes)
            } else {
                format!("^{}", dashes)
            }
        };
        let arrows = {
            let plain = "^".repeat((end - start).max(1));
            if ansi {
                format!("\x1b[1;31m{}\x1b[m", plain)
            } else {
                plain
            }
        };
        (
            format!(
                "{}{}\n{}{}{}",
                line_prefix, line_fmt, arrow_prefix, super_arrows, arrows,
            ),
            line_prefix_len,
        )
    }

    /// Formats this error with the source code, showing a preview of the error line highlighting
    /// the unexpected token.
    ///
    /// If `ansi` is set to true, this will use ANSI escape codes for fancy formatting.
    pub fn fmt_with_src(&self, src: &str, ansi: bool) -> String {
        match self {
            ParseError::Unexpected(..) => {
                let (ln, line, (start, end)) = self.error_line(src);
                let (line, _) = Self::fmt_line_with_arrows(ln, line, start, start, end, ansi);
                if ansi {
                    format!("{} \x1b[31m{}\x1b[m", line, self)
                } else {
                    format!("{} {}", line, self)
                }
            }
            ParseError::Alt(top, _) => {
                let (top_ln, top_line, (top_start, _)) = top.error_line(src);
                let err = self.most_successful();
                if err == **top {
                    return top.fmt_with_src(src, ansi);
                }
                let (ln, line, (start, end)) = err.error_line(src);
                if top_ln == ln {
                    // format like
                    //  1 | code code code
                    //      ^---------^^^^ err
                    let (line, prefix_len) =
                        Self::fmt_line_with_arrows(ln, line, top_start, start, end, ansi);
                    let top_line_pre = " ".repeat(prefix_len + top_start);
                    let top_line =
                        format!("{}(while parsing {})", top_line_pre, top.fmt_expected());
                    if ansi {
                        format!("{} \x1b[31m{}\n{}\x1b[m", line, err, top_line)
                    } else {
                        format!("{} {}\n{}", line, err, top_line)
                    }
                } else {
                    // format like
                    //  1 | code
                    // .. |
                    //  4 | code code code
                    //                ^^^^ err
                    let (top_line, top_prefix_len) =
                        Self::fmt_line_with_arrows(top_ln, top_line, 0, 0, 0, ansi);
                    // only need the line part (yes this is a bit hacky)
                    let top_line = top_line.split("\n").next().unwrap();
                    let (line, prefix_len) =
                        Self::fmt_line_with_arrows(ln, line, start, start, end, ansi);

                    let mut result = String::new();
                    if prefix_len > top_prefix_len {
                        result += &" ".repeat(prefix_len - top_prefix_len);
                    }
                    result += &top_line;
                    result.push('\n');

                    if ln > top_ln + 1 {
                        result += &" ".repeat(prefix_len.max(top_prefix_len).saturating_sub(3));
                        result += if ansi { "\x1b[1;34m..\x1b[m\n" } else { "..\n" };
                    }

                    if top_prefix_len > prefix_len {
                        result += &" ".repeat(top_prefix_len - prefix_len);
                    }
                    result += &line;
                    result += &if ansi {
                        format!(
                            " \x1b[31m{}\n{}(while parsing {})\x1b[m",
                            err,
                            " ".repeat(prefix_len.max(top_prefix_len)),
                            top.fmt_expected()
                        )
                    } else {
                        format!(
                            " {}\n{}(while parsing {})",
                            err,
                            " ".repeat(prefix_len.max(top_prefix_len)),
                            top.fmt_expected()
                        )
                    };
                    result
                }
            }
        }
    }

    fn fmt_expected(&self) -> String {
        match self {
            ParseError::Unexpected(_, e) => format!("{}", e),
            ParseError::Alt(_, sub_errors) => {
                use std::fmt::Write;

                let mut f = String::new();
                if sub_errors.len() == 1 {
                    f += &sub_errors[0].fmt_expected();
                } else {
                    for (index, sub_error) in sub_errors.iter().enumerate() {
                        match index {
                            0 => f += &sub_error.fmt_expected(),
                            _ if index == sub_errors.len() - 1 => {
                                if sub_errors.len() > 2 {
                                    write!(f, ", or {}", sub_error.fmt_expected()).unwrap();
                                } else {
                                    write!(f, " or {}", sub_error.fmt_expected()).unwrap();
                                }
                            }
                            _ => write!(f, ", {}", sub_error.fmt_expected()).unwrap(),
                        }
                    }
                }
                f
            }
        }
    }
}

impl<'input> fmt::Display for ParseError<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::Unexpected(c, _) => match c.get() {
                Some(Ok((_, t, _))) => write!(
                    f,
                    "unexpected {}; expected {}",
                    TokenType::from(t),
                    self.fmt_expected()
                ),
                Some(err) => write!(f, "lex error: {:?}", err),
                None => write!(
                    f,
                    "unexpected end of input; expected {}",
                    self.fmt_expected()
                ),
            },
            ParseError::Alt(top, _) => write!(f, "{} (one of: {})", top, self.fmt_expected()),
        }
    }
}

impl<'input> Error for ParseError<'input> {}

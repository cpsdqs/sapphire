//! The lexer.

use nom::*;

pub type Spanned<T, L, E> = Result<(L, T, L), E>;
pub type Item<'input> = Spanned<Token<'input>, usize, Err<&'input str>>;

// nom parser overrides, modified so they don’t fail weirdly at EOF
macro_rules! many_until {
    ($i:expr, $submac:ident!($($args:tt)*), $till:ident!($($targs:tt)*)) => {{
        let mut i = $i.clone();
        let mut res = Vec::new();

        loop {
            if i.is_empty() {
                break Ok((i, res));
            }

            match $till!(i, $($targs)*) {
                Ok(_) => break Ok((i, res)),
                Err(_) => match $submac!(i, $($args)*) {
                    Ok((j, r)) => {
                        i = j;
                        res.push(r);
                    }
                    Err(_) => break Ok((i, res)),
                }
            }
        }
    }};
    ($i:expr, $submac:ident!($($args:tt)*), $f:expr) => {
        many_until!($i, $submac!($($args)*), call!($f))
    };
}
macro_rules! take_while1 {
    ($i:expr, $submac:ident!($($args:tt)*)) => {{
        let i = $i.clone();
        let mut chars = i.chars();
        let mut end = 0;

        loop {
            match chars.next() {
                Some(c) if $submac!(c, $($args)*) => end += c.len_utf8(),
                _ => if end == 0 {
                    break Err(nom::Err::Error(Context::Code($i, ErrorKind::Many1)));
                } else {
                    break Ok((&i[end..], &i[..end]));
                }
            }
        }
    }};
    ($i:expr, $f:expr) => (take_while1!($i, call!($f)));
}
macro_rules! eof {
    ($i:expr,) => {{
        if $i.is_empty() {
            Ok(($i, $i))
        } else {
            Err(nom::Err::Error(Context::Code($i, ErrorKind::Eof)))
        }
    }};
}
macro_rules! opt {
    ($i:expr, $submac:ident!($($args:tt)*)) => {{
        match $submac!($i, $($args)*) {
            Ok((i, res)) => Ok((i, Some(res))),
            Err(_) => Ok(($i, None)),
        }
    }};
    ($i:expr, $f:expr) => (opt!($i, call!($f)));
}

named!(line_terminator<&str, ()>, do_parse!(opt!(char!('\r')) >> char!('\n') >> ()));
named!(line_terminator_escape<&str, ()>, do_parse!(char!('\\') >> line_terminator >> ()));

/// Whitespace
named!(ws<&str, ()>, alt_complete!(
    do_parse!(one_of!("\x09\x0b\x0c\x0d\x20") >> ())
    | line_terminator_escape
));

/// Takes the rest of the line.
named!(rest_of_line<&str, &str>, recognize!(many_until!(take!(1), line_terminator)));

/// Consumes a single line comment
named!(single_line_comment<&str, ()>, do_parse!(
    tag!("#") >> rest_of_line >> ()
));

/// Consumes a multi-line comment. Must only be used at the beginning of a line.
named!(l_multi_line_comment<&str, ()>, do_parse!(
    tag!("=begin") >>
    // rest of begin line
    alt_complete!(
        line_terminator | do_parse!(ws >> rest_of_line >> ())
    ) >>
    // comment body and end line
    many_till!(rest_of_line, tag!("=end")) >>
    ()
));

/// Consumes an end marker.
/// Must only be used at the beginning of a line.
named!(l_end_marker<&str, ()>, do_parse!(
    tag!("__END__") >> alt_complete!(line_terminator | do_parse!(eof!() >> ())) >> ()
));

named!(comment_at_line_start<&str, ()>, alt_complete!(
    l_multi_line_comment | single_line_comment
));
named!(comment_in_line<&str, ()>, do_parse!(single_line_comment >> ()));

/// Tokens.
///
/// - keywords are prefixed with `K`
/// - identifiers are prefixed with `I`
/// - punctuators are prefixed with `P`
/// - operators are prefixed with `O`
/// - literals are prefixed with `L`
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, PartialEq)]
pub enum Token<'input> {
    // Keywords
    K__LINE__,
    K__ENCODING__,
    K__FILE__,
    KBEGIN,
    KEND,
    Kalias,
    Kand,
    Kbegin,
    Kbreak,
    Kcase,
    Kclass,
    Kdef,
    Kdefined,
    Kdo,
    Kelse,
    Kelsif,
    Kend,
    Kensure,
    Kfor,
    Kfalse,
    Kif,
    Kin,
    Kmodule,
    Knext,
    Knil,
    Knot,
    Kor,
    Kredo,
    Krescue,
    Kretry,
    Kreturn,
    Kself,
    Ksuper,
    Kthen,
    Ktrue,
    Kundef,
    Kunless,
    Kuntil,
    Kwhen,
    Kwhile,
    Kyield,

    // Identifiers
    ILocal(&'input str),
    IGlobal(&'input str),
    IClass(&'input str),
    IInstance(&'input str),
    IConstant(&'input str),
    IMethodOnly(&'input str, char),
    IAssignmentLikeMethod(&'input str),

    // Punctuators
    /// `[`
    PLBracket,
    /// `]`
    PRBracket,
    /// `(`
    PLParen,
    /// `)`
    PRParen,
    /// `{`
    PLBrace,
    /// `}`
    PRBrace,
    /// `::`
    PDblColon,
    /// `,`
    PComma,
    /// `;`
    PSemicolon,
    /// `...`
    P3Dots,
    /// `..`
    P2Dots,
    /// `?`
    PQuestion,
    /// `:`
    PColon,
    /// `=>`
    PFatArrow,
    /// `->`
    PArrow,
    /// `.`
    PDot,

    // Operators
    /// `!=`
    ONeq,
    /// `!~`
    ONMatch,
    /// `!`
    ONot,
    /// `&&`
    OAnd,
    /// `||`
    OOr,
    // assignment operators
    /// `=`
    OAssign,
    /// `&&=`
    OAssignAnd,
    /// `||=`
    OAssignOr,
    /// `&=`
    OAssignBitAnd,
    /// `|=`
    OAssignBitOr,
    /// `^=`
    OAssignBitXor,
    /// `<<=`
    OAssignShl,
    /// `>>=`
    OAssignShr,
    /// `+=`
    OAssignAdd,
    /// `-=`
    OAssignSub,
    /// `*=`
    OAssignMul,
    /// `/=`
    OAssignDiv,
    /// `%=`
    OAssignRem,
    /// `**=`
    OAssignPow,
    // operator method names
    /// `^`
    OBitXor,
    /// `&`
    OBitAnd,
    /// `|`
    OBitOr,
    /// `<=>`
    OCmp,
    /// `===`
    OCaseEq,
    /// `==`
    OEq,
    /// `=~`
    OMatch,
    /// `>=`
    OGeq,
    /// `>>`
    OShr,
    /// `>`
    OGt,
    /// `<=`
    OLeq,
    /// `<<`
    OShl,
    /// `<`
    OLt,
    /// `+@` (unary plus)
    OUPlus,
    /// `+`
    OAdd,
    /// `-@` (unary minus)
    OUMinus,
    /// `-`
    OSub,
    /// `**`
    OPow,
    /// `*`
    OMul,
    /// `/`
    ODiv,
    /// `%`
    ORem,
    /// `~`
    OBitInv,
    /// `[]=`
    OAssignIndex,
    /// `[]`
    OIndex,
    // /// `\``
    // OCmd,

    // Literals
    LNum(NumericLiteral<'input>),
    // string literals
    /// Single-quoted string
    LSingleStr(&'input str),
    /// Double-quoted string
    LDoubleStr(Vec<Quoted<'input>>),
    /// Quoted non-expanded string (e.g. `%q(a)`)
    LQNeStr(&'input str),
    /// Quoted expanded string (e.g. `%Q(a #{b})`)
    LQStr(Vec<Quoted<'input>>),
    /// Heredoc string
    LHereDoc(&'input str),
    /// External command execution
    LExecCmd(Vec<Quoted<'input>>),
    // array literals
    /// Quoted non-expanded array (e.g. `%w(a b)`)
    LQNeArray(&'input str),
    /// Quoted expanded array (e.g. `%W(a b)`)
    LQArray(Vec<Quoted<'input>>),
    // regular expression literals
    /// Regular expression literal (e.g. `/ab/i`)
    LRegExp(Vec<Quoted<'input>>, &'input str),
    /// Quoted regular expression literal (e.g. `%r(ab)i`)
    LQRegExp(Vec<Quoted<'input>>, &'input str),
    // symbol literals
    LSymbol(&'input str),

    Whitespace,
    Newlines,
}

/// A numeric literal.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NumericLiteral<'input> {
    pub sign: char,
    pub contents: UnsignedNumeric<'input>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnsignedNumeric<'input> {
    OctInt(&'input str),
    HexInt(&'input str),
    BinInt(&'input str),
    DecInt(&'input str),
    /// Float with an exponent, e.g. `1e5`
    ///
    /// `integer part` . `decimal part` e `exponent sign` `exponent part`
    FloatExp(&'input str, &'input str, char, &'input str),
    /// Float without an exponent, e.g. `1.2`
    ///
    /// `integer part` . `decimal part`
    Float(&'input str, &'input str),
}

/// Contents of a quoted literal.
#[derive(Debug, Clone, PartialEq)]
pub enum Quoted<'input> {
    Fragment(&'input str),
    Ident(Token<'input>),

    /// Interpolated contents that should be tokenized later.
    Interpolated(Vec<Spanned<Token<'input>, usize, Err<&'input str>>>),
}

named!(peek_nalpha<&str, ()>, alt_complete!(not!(alphanumeric1) | do_parse!(eof!() >> ())));

named!(keyword<&str, Token>, do_parse!(token: alt_complete!(
    do_parse!(tag!("__LINE__") >> (Token::K__LINE__))
    | do_parse!(tag!("__ENCODING__") >> (Token::K__ENCODING__))
    | do_parse!(tag!("__FILE__") >> (Token::K__FILE__))
    | do_parse!(tag!("BEGIN") >> (Token::KBEGIN))
    | do_parse!(tag!("END") >> (Token::KEND))
    | do_parse!(tag!("alias") >> (Token::Kalias))
    | do_parse!(tag!("and") >> (Token::Kand))
    | do_parse!(tag!("begin") >> (Token::Kbegin))
    | do_parse!(tag!("break") >> (Token::Kbreak))
    | do_parse!(tag!("case") >> (Token::Kcase))
    | do_parse!(tag!("class") >> (Token::Kclass))
    | do_parse!(tag!("defined?") >> (Token::Kdefined))
    | do_parse!(tag!("def") >> (Token::Kdef))
    | do_parse!(tag!("do") >> (Token::Kdo))
    | do_parse!(tag!("else") >> (Token::Kelse))
    | do_parse!(tag!("elsif") >> (Token::Kelsif))
    | do_parse!(tag!("end") >> (Token::Kend))
    | do_parse!(tag!("ensure") >> (Token::Kensure))
    | do_parse!(tag!("for") >> (Token::Kfor))
    | do_parse!(tag!("false") >> (Token::Kfalse))
    | do_parse!(tag!("if") >> (Token::Kif))
    | do_parse!(tag!("in") >> (Token::Kin))
    | do_parse!(tag!("module") >> (Token::Kmodule))
    | do_parse!(tag!("next") >> (Token::Knext))
    | do_parse!(tag!("nil") >> (Token::Knil))
    | do_parse!(tag!("not") >> (Token::Knot))
    | do_parse!(tag!("or") >> (Token::Kor))
    | do_parse!(tag!("redo") >> (Token::Kredo))
    | do_parse!(tag!("rescue") >> (Token::Krescue))
    | do_parse!(tag!("retry") >> (Token::Kretry))
    | do_parse!(tag!("return") >> (Token::Kreturn))
    | do_parse!(tag!("self") >> (Token::Kself))
    | do_parse!(tag!("super") >> (Token::Ksuper))
    | do_parse!(tag!("then") >> (Token::Kthen))
    | do_parse!(tag!("true") >> (Token::Ktrue))
    | do_parse!(tag!("undef") >> (Token::Kundef))
    | do_parse!(tag!("unless") >> (Token::Kunless))
    | do_parse!(tag!("until") >> (Token::Kuntil))
    | do_parse!(tag!("when") >> (Token::Kwhen))
    | do_parse!(tag!("while") >> (Token::Kwhile))
    | do_parse!(tag!("yield") >> (Token::Kyield))
) >> peek_nalpha >> (token)));

named!(lowercase_character<&str, char>, one_of!("abcdefghijklmnopqrstuvwxyz"));
named!(uppercase_character<&str, char>, one_of!("ABCDEFGHIJKLMNOPQRSTUVWXYZ"));
named!(identifier_start_character<&str, char>, one_of!(
    "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
));
named!(take_identifier_chars<&str, &str>, take_while1!(|c: char| c.is_alphanum() || c == '_'));
named!(local_variable_identifier<&str, Token>, do_parse!(
    peek!(alt_complete!(lowercase_character | char!('_'))) >>
    ident: take_identifier_chars >>
    (Token::ILocal(ident))
));
named!(global_variable_identifier<&str, Token>, do_parse!(
    tag!("$") >>
    peek!(identifier_start_character) >>
    ident: take_identifier_chars >>
    (Token::IGlobal(ident))
));
named!(class_variable_identifier<&str, Token>, do_parse!(
    tag!("@@") >>
    peek!(identifier_start_character) >>
    ident: take_identifier_chars >>
    (Token::IClass(ident))
));
named!(instance_variable_identifier<&str, Token>, do_parse!(
    tag!("@") >>
    peek!(identifier_start_character) >>
    ident: take_identifier_chars >>
    (Token::IInstance(ident))
));
named!(constant_identifier<&str, Token>, do_parse!(
    peek!(uppercase_character) >>
    ident: take_identifier_chars >>
    (Token::IConstant(ident))
));
named!(method_only_identifier<&str, Token>, do_parse!(
    peek!(identifier_start_character) >>
    ident: take_identifier_chars >>
    end: one_of!("!?") >>
    (Token::IMethodOnly(ident, end))
));
named!(assignment_like_method_identifier<&str, Token>, do_parse!(
    peek!(identifier_start_character) >>
    ident: take_identifier_chars >> char!('=') >>
    (Token::IAssignmentLikeMethod(ident))
));

named!(identifier<&str, Token>, alt_complete!(
    assignment_like_method_identifier
    | method_only_identifier
    | class_variable_identifier
    | instance_variable_identifier
    | global_variable_identifier
    | constant_identifier
    | local_variable_identifier
));

named!(punctuator<&str, Token>, alt_complete!(
    do_parse!(char!('[') >> (Token::PLBracket))
    | do_parse!(char!(']') >> (Token::PRBracket))
    | do_parse!(char!('(') >> (Token::PLParen))
    | do_parse!(char!(')') >> (Token::PRParen))
    | do_parse!(char!('{') >> (Token::PLBrace))
    | do_parse!(char!('}') >> (Token::PRBrace))
    | do_parse!(tag!("::") >> (Token::PDblColon))
    | do_parse!(char!(',') >> (Token::PComma))
    | do_parse!(char!(';') >> (Token::PSemicolon))
    | do_parse!(tag!("...") >> (Token::P3Dots))
    | do_parse!(tag!("..") >> (Token::P2Dots))
    | do_parse!(char!('?') >> (Token::PQuestion))
    | do_parse!(char!(':') >> (Token::PColon))
    | do_parse!(tag!("=>") >> (Token::PFatArrow))
    | do_parse!(tag!("->") >> (Token::PArrow))
    | do_parse!(char!('.') >> (Token::PDot))
));

// TODO: parse unary plus maybe idk

named!(operator<&str, Token>, alt_complete!(
    do_parse!(tag!("!=") >> (Token::ONeq))
    | do_parse!(tag!("!~") >> (Token::ONMatch))
    | do_parse!(tag!("!") >> (Token::ONot))
    | do_parse!(tag!("&&=") >> (Token::OAssignAnd))
    | do_parse!(tag!("||=") >> (Token::OAssignOr))
    | do_parse!(tag!("&=") >> (Token::OAssignBitAnd))
    | do_parse!(tag!("|=") >> (Token::OAssignBitOr))
    | do_parse!(tag!("^=") >> (Token::OAssignBitXor))
    | do_parse!(tag!("<<=") >> (Token::OAssignShl))
    | do_parse!(tag!(">>=") >> (Token::OAssignShr))
    | do_parse!(tag!("+=") >> (Token::OAssignAdd))
    | do_parse!(tag!("-=") >> (Token::OAssignSub))
    | do_parse!(tag!("*=") >> (Token::OAssignMul))
    | do_parse!(tag!("/=") >> (Token::OAssignDiv))
    | do_parse!(tag!("%=") >> (Token::OAssignRem))
    | do_parse!(tag!("**=") >> (Token::OAssignPow))
    | do_parse!(tag!("&&") >> (Token::OAnd))
    | do_parse!(tag!("||") >> (Token::OOr))
    | do_parse!(tag!("^") >> (Token::OBitXor))
    | do_parse!(tag!("&") >> (Token::OBitAnd))
    | do_parse!(tag!("|") >> (Token::OBitOr))
    | do_parse!(tag!("<=>") >> (Token::OCmp))
    | do_parse!(tag!("===") >> (Token::OCaseEq))
    | do_parse!(tag!("==") >> (Token::OEq))
    | do_parse!(tag!("=~") >> (Token::OMatch))
    | do_parse!(tag!(">=") >> (Token::OGeq))
    | do_parse!(tag!(">>") >> (Token::OShr))
    | do_parse!(tag!(">") >> (Token::OGt))
    | do_parse!(tag!("<=") >> (Token::OLeq))
    | do_parse!(tag!("<<") >> (Token::OShl))
    | do_parse!(tag!("<") >> (Token::OLt))
    | do_parse!(tag!("+") >> (Token::OAdd))
    | do_parse!(tag!("-") >> (Token::OSub))
    | do_parse!(tag!("**") >> (Token::OPow))
    | do_parse!(tag!("*") >> (Token::OMul))
    | do_parse!(tag!("/") >> (Token::ODiv))
    | do_parse!(tag!("%") >> (Token::ORem))
    | do_parse!(tag!("~") >> (Token::OBitInv))
    | do_parse!(tag!("=") >> (Token::OAssign))
    // | do_parse!(tag!("[]=") >> (Token::OAssignIndex))
    // | do_parse!(tag!("[]") >> (Token::OIndex))
));

named!(numeric_literal<&str, Token>, alt_complete!(
    signed_number
    | do_parse!(contents: unsigned_number >> (Token::LNum(NumericLiteral { sign: '+', contents })))
));
named!(signed_number<&str, Token>, do_parse!(
    sign: one_of!("+-") >>
    contents: unsigned_number >>
    (Token::LNum(NumericLiteral { sign, contents }))
));
named!(unsigned_number<&str, UnsignedNumeric>, alt_complete!(float_literal | integer_literal));
named!(integer_literal<&str, UnsignedNumeric>, alt_complete!(
    octal_integer_literal
    | hexadecimal_integer_literal
    | binary_integer_literal
    | prefixed_decimal_integer_literal
    | unprefixed_decimal_integer_literal
));
named!(unprefixed_decimal_integer_literal<&str, UnsignedNumeric>, alt_complete!(
    do_parse!(
        peek!(one_of!("123456789")) >>
        contents: take_while1!(|c: char| c == '_' || c.is_digit(10)) >>
        (UnsignedNumeric::DecInt(contents))
    )
    | do_parse!(zero: tag!("0") >> (UnsignedNumeric::DecInt(zero)))
));
named!(prefixed_decimal_integer_literal<&str, UnsignedNumeric>, do_parse!(
    char!('0') >> one_of!("dD") >>
    peek!(one_of!("0123456789")) >>
    contents: take_while1!(|c: char| c == '_' || c.is_digit(10)) >>
    (UnsignedNumeric::DecInt(contents))
));
named!(binary_integer_literal<&str, UnsignedNumeric>, do_parse!(
    char!('0') >> one_of!("bB") >>
    peek!(one_of!("01")) >>
    contents: take_while1!(|c: char| c == '0' || c == '1' || c == '_') >>
    (UnsignedNumeric::BinInt(contents))
));
named!(octal_integer_literal<&str, UnsignedNumeric>, do_parse!(
    char!('0') >> opt!(one_of!("_oO")) >>
    peek!(one_of!("01234567")) >>
    contents: take_while1!(|c: char| c == '_' || c.is_digit(8)) >>
    (UnsignedNumeric::OctInt(contents))
));
named!(hexadecimal_integer_literal<&str, UnsignedNumeric>, do_parse!(
    char!('0') >> one_of!("xX") >>
    peek!(one_of!("0123456789abcdefABCDEF")) >>
    contents: take_while!(|c: char| c == '_' || c.is_digit(16)) >>
    (UnsignedNumeric::HexInt(contents))
));
named!(float_literal<&str, UnsignedNumeric>, alt_complete!(
    float_with_exp
    | do_parse!(contents: float_without_exp >> (UnsignedNumeric::Float(contents.0, contents.1)))
));
named!(float_without_exp<&str, (&str, &str)>, do_parse!(
    peek!(one_of!("123456789")) >>
    int_part: take_while1!(|c: char| c == '_' || c.is_digit(10)) >>
    char!('.') >>
    decimal_part: take_while1!(|c: char| c == '_' || c.is_digit(10)) >>
    ((int_part, decimal_part))
));
named!(float_with_exp<&str, UnsignedNumeric>, do_parse!(
    significand: alt_complete!(
        float_without_exp
        | do_parse!(
            peek!(one_of!("123456789")) >>
            int_part: take_while1!(|c: char| c == '_' || c.is_digit(10)) >>
            ((int_part, ""))
        )
    ) >>
    one_of!("eE") >>
    exp_sign: opt!(one_of!("+-")) >>
    exponent: take_while1!(|c: char| c == '_' || c.is_digit(10)) >>
    (UnsignedNumeric::FloatExp(significand.0, significand.1, exp_sign.unwrap_or('+'), exponent))
));

named!(string_literal<&str, Token>, alt_complete!(
    single_quoted_string | double_quoted_string /*| quoted_non_expanded_string
    | quoted_expanded_string | here_document | external_command_execution */ // TODO: these
));
named!(single_quoted_string<&str, Token>, do_parse!(
    tag!("'") >>
    contents: recognize!(many0!(alt_complete!(
        do_parse!(char!('\\') >> anychar >> ('*'))
        | none_of!("'\\")
    ))) >>
    tag!("'") >>
    (Token::LSingleStr(contents))
));
named!(double_escape_sequence<&str, ()>, alt_complete!(
    do_parse!(char!('\\') >> take_while_m_n!(1, 3, |c: char| c.is_digit(8)) >> ())
    | do_parse!(tag!("\\x") >> take_while_m_n!(1, 2, |c: char| c.is_digit(16)) >> ())
    | do_parse!(
        alt_complete!(tag!("\\C-") | tag!("\\c")) >>
        alt_complete!(double_escape_sequence | do_parse!(none_of!("\\") >> ())) >>
        ()
    )
    | do_parse!(char!('\\') >> anychar >> ())
));

named!(interpolated_content<&str, ()>, do_parse!(many0!(alt_complete!(
        do_parse!(lex_whitespace >> ())
        | do_parse!(string_literal >> ())
        | do_parse!(tag!("(") >> interpolated_content >> tag!(")") >> ())
        | do_parse!(tag!("{") >> interpolated_content >> tag!("}") >> ())
        | do_parse!(tag!("[") >> interpolated_content >> tag!("]") >> ())
        | do_parse!(none_of!("({[]})") >> ())
)) >> ()));

named!(interpolated_sequence<&str, Quoted>, alt_complete!(
    do_parse!(tag!("#") >> ident: global_variable_identifier >> (Quoted::Ident(ident)))
    | do_parse!(tag!("#") >> ident: class_variable_identifier >> (Quoted::Ident(ident)))
    | do_parse!(tag!("#") >> ident: instance_variable_identifier >> (Quoted::Ident(ident)))
    | do_parse!(
        tag!("#{") >>
        contents: recognize!(interpolated_content) >>
        tag!("}") >>
        (Quoted::Interpolated(Lexer::new(contents).collect()))
    )
));
named!(double_quoted_string<&str, Token>, do_parse!(
    tag!("\"") >>
    contents: many0!(alt_complete!(
        interpolated_sequence
        | do_parse!(
            contents: recognize!(
                many1!(do_parse!(
                    not!(alt_complete!(tag!("#$") | tag!("#@") | tag!("#{"))) >>
                    alt_complete!(double_escape_sequence | do_parse!(none_of!("\\\"") >> ())) >>
                    ()
                ))
            ) >>
            (Quoted::Fragment(contents))
        )
    )) >>
    tag!("\"") >>
    (Token::LDoubleStr(contents))
));
// TODO: literals
named!(literal<&str, Token>, alt_complete!(numeric_literal | string_literal));

fn lex_whitespace(input: &str) -> IResult<&str, Token> {
    let mut i = input;
    let mut length = 0;
    let mut newlines = false;
    loop {
        if let Ok((j, ())) = ws(i) {
            i = j;
        } else if let Ok((j, ())) = line_terminator(i) {
            newlines = true;
            i = j;
        } else if length == 0 {
            break Err(Err::Error(Context::Code(i, ErrorKind::Many1)));
        } else if newlines {
            break Ok((i, Token::Newlines));
        } else {
            break Ok((i, Token::Whitespace));
        }
        length += 1;
    }
}

named!(lex_one<&str, Token>, alt_complete!(
    lex_whitespace | keyword | literal | punctuator | operator | identifier
));

pub struct Lexer<'input> {
    remaining: &'input str,
    prev_token_pos: usize,
    did_error: bool,
    at_sol: bool,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &str) -> Lexer {
        Lexer {
            remaining: input,
            prev_token_pos: 0,
            did_error: false,
            at_sol: true,
        }
    }

    fn update_remaining(&mut self, remaining: &'input str) {
        let split_pos = self.remaining.len() - remaining.len();
        self.at_sol = self.remaining.is_char_boundary(split_pos - 1)
            && self.remaining[split_pos - 1..].chars().next() == Some('\n');
        self.remaining = remaining;
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token<'input>, usize, Err<&'input str>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining.trim().is_empty() || self.did_error {
            return None;
        }

        // skip comments
        if self.at_sol {
            if let Ok((remaining, _)) = comment_at_line_start(self.remaining) {
                self.prev_token_pos += self.remaining.len() - remaining.len();
                self.update_remaining(remaining);
                return self.next();
            }

            if let Ok((remaining, _)) = l_end_marker(self.remaining) {
                self.did_error = true; // re-purposed to just continue returning None
                self.update_remaining(remaining);
                return None;
            }
        } else {
            if let Ok((remaining, _)) = comment_in_line(self.remaining) {
                self.prev_token_pos += self.remaining.len() - remaining.len();
                self.update_remaining(remaining);
                return self.next();
            }
        }

        match lex_one(self.remaining) {
            Ok((remaining, mut token)) => {
                // spans of quoted tokens need correcting
                let mut prev_quoted_pos = self.prev_token_pos;
                match token {
                    Token::LDoubleStr(_) => prev_quoted_pos += 1,
                    Token::LQStr(_) => prev_quoted_pos += 3,
                    Token::LExecCmd(_) => prev_quoted_pos += 1,
                    Token::LQArray(_) => prev_quoted_pos += 3,
                    Token::LQRegExp(..) => prev_quoted_pos += 3,
                    _ => (),
                }
                match &mut token {
                    Token::LDoubleStr(quoted)
                    | Token::LQStr(quoted)
                    | Token::LExecCmd(quoted)
                    | Token::LQArray(quoted)
                    | Token::LQRegExp(quoted, _) => {
                        for item in quoted {
                            match item {
                                Quoted::Fragment(f) => prev_quoted_pos += f.len(),
                                Quoted::Ident(t) => match t {
                                    // #$
                                    Token::IGlobal(s) => prev_quoted_pos += s.len() + 2,
                                    // #@@
                                    Token::IClass(s) => prev_quoted_pos += s.len() + 3,
                                    // #@
                                    Token::IInstance(s) => prev_quoted_pos += s.len() + 2,
                                    t => panic!("invalid quoted identifier token {:?}", t),
                                },
                                Quoted::Interpolated(tokens) => {
                                    prev_quoted_pos += 2; // #{
                                    let interpolated_pos = prev_quoted_pos;
                                    for token in tokens.iter_mut() {
                                        match token {
                                            Ok(token) => {
                                                token.0 += interpolated_pos;
                                                token.2 += interpolated_pos;
                                                prev_quoted_pos += token.2 - token.0;
                                            }
                                            _ => (),
                                        }
                                    }
                                    prev_quoted_pos += 1; // }
                                }
                            }
                        }
                    }
                    _ => (),
                }

                let token_start = self.prev_token_pos;
                self.prev_token_pos += self.remaining.len() - remaining.len();
                self.update_remaining(remaining);
                Some(Ok((token_start, token, self.prev_token_pos)))
            }
            Err(err) => {
                self.did_error = true;
                Some(Err(err))
            }
        }
    }
}

#[test]
fn test_lex_code() {
    assert_eq!(
        Lexer::new("a # comment\nb =begin #\n=begin\n=end")
            .map(|res| match res {
                Ok((_, token, _)) => token,
                Err(err) => panic!("{}", err),
            })
            .collect::<Vec<_>>(),
        vec![
            Token::ILocal("a"),
            Token::Whitespace,
            Token::Newlines,
            Token::ILocal("b"),
            Token::Whitespace,
            Token::OAssign,
            Token::Kbegin,
            Token::Whitespace,
            Token::Newlines,
        ]
    );

    assert_eq!(
        Lexer::new("class B({a})\n hello.world?world(5, 05, -1e50'ab\\'c')")
            .map(|res| match res {
                Ok((_, token, _)) => token,
                Err(err) => panic!("{}", err),
            })
            .collect::<Vec<_>>(),
        vec![
            Token::Kclass,
            Token::Whitespace,
            Token::IConstant("B"),
            Token::PLParen,
            Token::PLBrace,
            Token::ILocal("a"),
            Token::PRBrace,
            Token::PRParen,
            Token::Newlines,
            Token::ILocal("hello"),
            Token::PDot,
            Token::IMethodOnly("world", '?'),
            Token::ILocal("world"),
            Token::PLParen,
            Token::LNum(NumericLiteral {
                sign: '+',
                contents: UnsignedNumeric::DecInt("5")
            }),
            Token::PComma,
            Token::Whitespace,
            Token::LNum(NumericLiteral {
                sign: '+',
                contents: UnsignedNumeric::OctInt("5")
            }),
            Token::PComma,
            Token::Whitespace,
            Token::LNum(NumericLiteral {
                sign: '-',
                contents: UnsignedNumeric::FloatExp("1", "", '+', "50")
            }),
            Token::LSingleStr("ab\\'c"),
            Token::PRParen,
        ]
    );

    assert_eq!(
        Lexer::new("\"interpolated: #@ivar #{hello world}#{hello}\"")
            .map(|res| match res {
                Ok((_, token, _)) => token,
                Err(err) => panic!("{}", err),
            })
            .collect::<Vec<_>>(),
        vec![Token::LDoubleStr(vec![
            Quoted::Fragment("interpolated: "),
            Quoted::Ident(Token::IInstance("ivar")),
            Quoted::Fragment(" "),
            Quoted::Interpolated(vec![
                Ok((24, Token::ILocal("hello"), 29)),
                Ok((29, Token::Whitespace, 30)),
                Ok((30, Token::ILocal("world"), 35)),
            ]),
            Quoted::Interpolated(vec![Ok((38, Token::ILocal("hello"), 43))]),
        ])]
    );
}

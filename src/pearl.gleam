import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import gleam_community/ansi
import houdini
import splitter.{type Splitter}

pub opaque type Lexer {
  Lexer(
    source: String,
    ignore_comments: Bool,
    ignore_whitespace: Bool,
    errors: List(Error),
    splitters: Splitters,
  )
}

type Splitters {
  Splitters(
    until_end_of_line: Splitter,
    string: Splitter,
    quoted_atom: Splitter,
    brace_escape_sequence: Splitter,
    sigil: Splitter,
    sigil_verbatim: Splitter,
    triple_quoted_string: Splitter,
  )
}

pub type Error {
  UnknownCharacter(character: String)
  UnterminatedStringLiteral
  UnterminatedQuotedAtom
  InvalidRadix(radix: String)
  NumericSeparatorNotAllowed
  ExpectedExponent
  NumberCannotEndAfterRadix
  UnterminatedCharacter
  UnterminatedEscapeSequence
  ExpectedSigilDelimiter
  ExpectedWhitespaceAfterTripleQuote
  InvalidTripleQuotedStringIndentation(
    expected_indentation: String,
    line: String,
  )
}

pub fn stringify_error(error: Error) -> String {
  case error {
    UnterminatedQuotedAtom -> "Unterminated quoted atom"
    UnterminatedStringLiteral -> "Unterminated string literal"
    ExpectedExponent -> "Expected an exponent"
    ExpectedSigilDelimiter -> "Expected a valid sigil delimiter after `~`"
    ExpectedWhitespaceAfterTripleQuote ->
      "Expected whitespace after a triple quote"
    InvalidRadix(radix:) -> "Invalid numeric radix: " <> radix
    InvalidTripleQuotedStringIndentation(expected_indentation:, line:) ->
      "Invalid triple-quoted string: Expected the indentation `"
      <> expected_indentation
      <> "` preceding the line `"
      <> line
      <> "`"
    NumberCannotEndAfterRadix ->
      "Number cannot end directly after radix specification"
    NumericSeparatorNotAllowed -> "Numeric separator is not allowed here"
    UnknownCharacter(character:) ->
      "Unexpected character: `" <> character <> "`"
    UnterminatedCharacter -> "Unterminated character literal"
    UnterminatedEscapeSequence -> "Unterminated escape sequence"
  }
}

pub type Token {
  // Whitespace and comments
  Whitespace(String)
  Comment(String)
  DocComment(String)
  ModuleComment(String)
  EndOfFile

  Character(String)
  Integer(String)
  Float(String)
  Atom(name: String, quoted: Bool)
  String(String)
  TripleQuotedString(
    sigil: option.Option(String),
    number_of_quotes: Int,
    beginning_whitespace: String,
    lines: List(String),
    end_indentation: String,
  )
  Sigil(sigil: String, delimiter: SigilDelimiter, contents: String)
  Variable(String)

  // Keywords
  After
  Begin
  Case
  Catch
  Cond
  Else
  End
  Fun
  If
  Let
  Maybe
  Of
  Receive
  Try
  When

  // Grouping
  LeftParen
  RightParen
  LeftBrace
  RightBrace
  LeftSquare
  RightSquare

  // Punctuation
  Comma
  Semicolon
  Colon
  Dot
  MinusGreater
  DoubleLess
  DoubleGreater
  Hash
  DoubleColon
  DoubleDot
  TripleDot
  DoublePipe
  EqualGreater
  ColonEqual
  LessMinus
  LessEqual

  // Operators
  Pipe
  DoubleEqual
  SlashEqual
  EqualLess
  Less
  GreaterEqual
  Greater
  EqualColonEqual
  EqualSlashEqual
  Plus
  Minus
  Star
  Slash
  Bnot
  Div
  Rem
  Band
  Bor
  Bxor
  Bsl
  Bsr
  Not
  And
  Or
  Xor
  Andalso
  Orelse
  DoublePlus
  DoubleMinus
  QuestionEqual
  Question
  Bang
  Equal

  // Invalid tokens
  Unknown(String)
  UnterminatedString(String)
  UnterminatedSigil(sigil: String, delimiter: SigilDelimiter, contents: String)
  UnterminatedAtom(String)
  InvalidTripleQuotedString(contents: String)
}

/// Convert a token back to its source code representation
pub fn token_to_source(token: Token) -> String {
  case token {
    // Whitespace and comments
    Whitespace(space) -> space
    Comment(contents) -> "%" <> contents
    DocComment(contents) -> "%%" <> contents
    ModuleComment(contents) -> "%%%" <> contents
    EndOfFile -> ""

    Character(char) -> "$" <> char
    Integer(int) -> int
    Float(float) -> float
    Atom(name:, quoted: True) -> "'" <> name <> "'"
    Atom(name:, quoted: False) -> name
    String(contents) -> "\"" <> contents <> "\""
    TripleQuotedString(
      sigil:,
      number_of_quotes:,
      beginning_whitespace:,
      lines:,
      end_indentation:,
    ) ->
      case sigil {
        option.None -> ""
        option.Some(sigil) -> "~" <> sigil
      }
      <> string.repeat("\"", number_of_quotes)
      <> beginning_whitespace
      <> string.join(
        list.map(lines, fn(line) { end_indentation <> line }),
        "\n",
      )
      <> "\n"
      <> end_indentation
      <> string.repeat("\"", number_of_quotes)
    Sigil(sigil:, delimiter:, contents:) -> {
      let #(opening, closing) = sigil_delimiters(delimiter)
      "~" <> sigil <> opening <> contents <> closing
    }
    Variable(name) -> name

    // Keywords
    After -> "after"
    Begin -> "begin"
    Case -> "case"
    Catch -> "catch"
    Cond -> "cond"
    Else -> "else"
    End -> "end"
    Fun -> "fun"
    If -> "if"
    Let -> "let"
    Maybe -> "maybe"
    Of -> "of"
    Receive -> "receive"
    Try -> "try"
    When -> "when"

    // Grouping
    LeftParen -> "("
    RightParen -> ")"
    LeftBrace -> "{"
    RightBrace -> "}"
    LeftSquare -> "["
    RightSquare -> "]"

    // Punctuation
    Comma -> ","
    Semicolon -> ";"
    Colon -> ":"
    Dot -> "."
    MinusGreater -> "->"
    DoubleLess -> "<<"
    DoubleGreater -> ">>"
    Hash -> "#"
    DoubleColon -> "::"
    DoubleDot -> ".."
    TripleDot -> "..."
    DoublePipe -> "||"
    EqualGreater -> "=>"
    ColonEqual -> ":="
    LessMinus -> "<-"
    LessEqual -> "<="

    // Operators
    Pipe -> "|"
    DoubleEqual -> "=="
    SlashEqual -> "/="
    EqualLess -> "=<"
    Less -> "<"
    GreaterEqual -> ">="
    Greater -> ">"
    EqualColonEqual -> "=:="
    EqualSlashEqual -> "=/="
    Plus -> "+"
    Minus -> "-"
    Star -> "*"
    Slash -> "/"
    Bnot -> "bnot"
    Div -> "div"
    Rem -> "rem"
    Band -> "band"
    Bor -> "bor"
    Bxor -> "bxor"
    Bsl -> "bsl"
    Bsr -> "bsr"
    Not -> "not"
    And -> "and"
    Or -> "or"
    Xor -> "xor"
    Andalso -> "andalso"
    Orelse -> "orelse"
    DoublePlus -> "++"
    DoubleMinus -> "--"
    QuestionEqual -> "?="
    Question -> "?"
    Bang -> "!"
    Equal -> "="

    // Invalid tokens
    Unknown(char) -> char
    UnterminatedString(contents) -> "\"" <> contents
    UnterminatedSigil(sigil:, contents:, delimiter:) -> {
      let #(opening, _closing) = sigil_delimiters(delimiter)
      "~" <> sigil <> opening <> contents
    }
    UnterminatedAtom(contents) -> "'" <> contents
    InvalidTripleQuotedString(contents) -> "\"\"\"" <> contents <> "\"\"\""
  }
}

/// Convert a list of tokens back to their original source code
pub fn to_source(tokens: List(Token)) -> String {
  list.fold(tokens, "", fn(code, token) { code <> token_to_source(token) })
}

pub type SigilDelimiter {
  SigilNone
  SigilParen
  SigilSquare
  SigilBrace
  SigilAngle
  SigilSlash
  SigilPipe
  SigilSingleQuote
  SigilDoubleQuote
  SigilBacktick
  SigilHash
}

/// Get the beginning and ending characters for a sigil
pub fn sigil_delimiters(delimiter: SigilDelimiter) -> #(String, String) {
  case delimiter {
    SigilNone -> #("", "")
    SigilAngle -> #("<", ">")
    SigilBacktick -> #("`", "`")
    SigilBrace -> #("{", "}")
    SigilDoubleQuote -> #("\"", "\"")
    SigilHash -> #("#", "#")
    SigilParen -> #("(", ")")
    SigilPipe -> #("|", "|")
    SigilSingleQuote -> #("'", "'")
    SigilSlash -> #("/", "/")
    SigilSquare -> #("[", "]")
  }
}

pub fn new(source: String) -> Lexer {
  Lexer(
    source:,
    ignore_comments: False,
    ignore_whitespace: False,
    errors: [],
    splitters: make_splitters(),
  )
}

fn make_splitters() -> Splitters {
  Splitters(
    until_end_of_line: splitter.new(["\n", "\r\n"]),
    string: splitter.new(["\"", "\\"]),
    quoted_atom: splitter.new(["'", "\\"]),
    brace_escape_sequence: splitter.new(["}", "\n", "\r\n"]),
    sigil: splitter.new([
      ")", "]", "}", ">", "/", "|", "'", "\"", "`", "#", "\\",
    ]),
    sigil_verbatim: splitter.new([
      ")", "]", "}", ">", "/", "|", "'", "\"", "`", "#",
    ]),
    triple_quoted_string: splitter.new(["\n", "\r\n", "\"\"\""]),
  )
}

pub fn ignore_comments(lexer: Lexer) -> Lexer {
  Lexer(..lexer, ignore_comments: True)
}

pub fn ignore_whitespace(lexer: Lexer) -> Lexer {
  Lexer(..lexer, ignore_whitespace: True)
}

pub fn tokenise(lexer: Lexer) -> #(List(Token), List(Error)) {
  do_tokenise(lexer, [])
}

fn do_tokenise(lexer: Lexer, tokens: List(Token)) -> #(List(Token), List(Error)) {
  case next(lexer) {
    #(lexer, EndOfFile) -> #(
      list.reverse([EndOfFile, ..tokens]),
      list.reverse(lexer.errors),
    )
    #(lexer, token) -> do_tokenise(lexer, [token, ..tokens])
  }
}

fn next(lexer: Lexer) -> #(Lexer, Token) {
  case lexer.source {
    "" -> #(lexer, EndOfFile)

    " " as space <> source
    | "\n" as space <> source
    | "\r" as space <> source
    | "\t" as space <> source
    | "\f" as space <> source -> lex_whitespace(advance(lexer, source), space)

    "%%%" <> source -> {
      let #(lexer, contents) = lex_until_end_of_line(advance(lexer, source))
      maybe_token(lexer, ModuleComment(contents), !lexer.ignore_comments)
    }
    "%%" <> source -> {
      let #(lexer, contents) = lex_until_end_of_line(advance(lexer, source))
      maybe_token(lexer, DocComment(contents), !lexer.ignore_comments)
    }
    "%" <> source -> {
      let #(lexer, contents) = lex_until_end_of_line(advance(lexer, source))
      maybe_token(lexer, Comment(contents), !lexer.ignore_comments)
    }

    "::" <> source -> #(advance(lexer, source), DoubleColon)
    ":=" <> source -> #(advance(lexer, source), ColonEqual)
    ":" <> source -> #(advance(lexer, source), Colon)
    "..." <> source -> #(advance(lexer, source), TripleDot)
    ".." <> source -> #(advance(lexer, source), DoubleDot)

    "(" <> source -> #(advance(lexer, source), LeftParen)
    ")" <> source -> #(advance(lexer, source), RightParen)
    "{" <> source -> #(advance(lexer, source), LeftBrace)
    "}" <> source -> #(advance(lexer, source), RightBrace)
    "[" <> source -> #(advance(lexer, source), LeftSquare)
    "]" <> source -> #(advance(lexer, source), RightSquare)

    "," <> source -> #(advance(lexer, source), Comma)
    ";" <> source -> #(advance(lexer, source), Semicolon)
    "." <> source -> #(advance(lexer, source), Dot)
    "->" <> source -> #(advance(lexer, source), MinusGreater)
    "<<" <> source -> #(advance(lexer, source), DoubleLess)
    ">>" <> source -> #(advance(lexer, source), DoubleGreater)
    "#" <> source -> #(advance(lexer, source), Hash)
    "||" <> source -> #(advance(lexer, source), DoublePipe)
    "=>" <> source -> #(advance(lexer, source), EqualGreater)
    "<-" <> source -> #(advance(lexer, source), LessMinus)
    "<=" <> source -> #(advance(lexer, source), LessEqual)
    "|" <> source -> #(advance(lexer, source), Pipe)

    "++" <> source -> #(advance(lexer, source), DoublePlus)
    "--" <> source -> #(advance(lexer, source), DoubleMinus)
    "==" <> source -> #(advance(lexer, source), DoubleEqual)
    "/=" <> source -> #(advance(lexer, source), SlashEqual)
    "=<" <> source -> #(advance(lexer, source), EqualLess)
    "<" <> source -> #(advance(lexer, source), Less)
    ">=" <> source -> #(advance(lexer, source), GreaterEqual)
    ">" <> source -> #(advance(lexer, source), Greater)
    "=:=" <> source -> #(advance(lexer, source), EqualColonEqual)
    "=/=" <> source -> #(advance(lexer, source), EqualSlashEqual)
    "+" <> source -> #(advance(lexer, source), Plus)
    "-" <> source -> #(advance(lexer, source), Minus)
    "*" <> source -> #(advance(lexer, source), Star)
    "/" <> source -> #(advance(lexer, source), Slash)
    "?=" <> source -> #(advance(lexer, source), QuestionEqual)
    "?" <> source -> #(advance(lexer, source), Question)
    "!" <> source -> #(advance(lexer, source), Bang)
    "=" <> source -> #(advance(lexer, source), Equal)

    "a" as char <> source
    | "b" as char <> source
    | "c" as char <> source
    | "d" as char <> source
    | "e" as char <> source
    | "f" as char <> source
    | "g" as char <> source
    | "h" as char <> source
    | "i" as char <> source
    | "j" as char <> source
    | "k" as char <> source
    | "l" as char <> source
    | "m" as char <> source
    | "n" as char <> source
    | "o" as char <> source
    | "p" as char <> source
    | "q" as char <> source
    | "r" as char <> source
    | "s" as char <> source
    | "t" as char <> source
    | "u" as char <> source
    | "v" as char <> source
    | "w" as char <> source
    | "x" as char <> source
    | "y" as char <> source
    | "z" as char <> source -> lex_atom(advance(lexer, source), char)

    "A" as char <> source
    | "B" as char <> source
    | "C" as char <> source
    | "D" as char <> source
    | "E" as char <> source
    | "F" as char <> source
    | "G" as char <> source
    | "H" as char <> source
    | "I" as char <> source
    | "J" as char <> source
    | "K" as char <> source
    | "L" as char <> source
    | "M" as char <> source
    | "N" as char <> source
    | "O" as char <> source
    | "P" as char <> source
    | "Q" as char <> source
    | "R" as char <> source
    | "S" as char <> source
    | "T" as char <> source
    | "U" as char <> source
    | "V" as char <> source
    | "W" as char <> source
    | "X" as char <> source
    | "Y" as char <> source
    | "Z" as char <> source
    | "_" as char <> source -> lex_variable(advance(lexer, source), char)

    "0" as char <> source
    | "1" as char <> source
    | "2" as char <> source
    | "3" as char <> source
    | "4" as char <> source
    | "5" as char <> source
    | "6" as char <> source
    | "7" as char <> source
    | "8" as char <> source
    | "9" as char <> source ->
      lex_number(advance(lexer, source), char, Initial, AfterNumber)

    "\"\"\"" <> source -> lex_triple_quoted_string(advance(lexer, source), None)

    "\"" <> source -> lex_string(advance(lexer, source), "")
    "'" <> source -> lex_quoted_atom(advance(lexer, source), "")

    "$" <> source -> lex_character(advance(lexer, source))

    "~" <> source -> lex_sigil(advance(lexer, source))

    _ ->
      case string.pop_grapheme(lexer.source) {
        Error(_) -> #(lexer, EndOfFile)
        Ok(#(char, source)) -> #(
          advance(error(lexer, UnknownCharacter(char)), source),
          Unknown(char),
        )
      }
  }
}

fn lex_character(lexer: Lexer) -> #(Lexer, Token) {
  case lexer.source {
    "\\" <> source -> {
      let #(lexer, escape_sequence) =
        lex_escape_sequence(advance(lexer, source))
      #(lexer, Character("\\" <> escape_sequence))
    }
    _ ->
      case string.pop_grapheme(lexer.source) {
        Ok(#(char, source)) -> #(advance(lexer, source), Character(char))
        Error(_) -> #(error(lexer, UnterminatedCharacter), Character(""))
      }
  }
}

fn lex_escape_sequence(lexer: Lexer) -> #(Lexer, String) {
  case lexer.source {
    "^a" as sequence <> source
    | "^b" as sequence <> source
    | "^c" as sequence <> source
    | "^d" as sequence <> source
    | "^e" as sequence <> source
    | "^f" as sequence <> source
    | "^g" as sequence <> source
    | "^h" as sequence <> source
    | "^i" as sequence <> source
    | "^j" as sequence <> source
    | "^k" as sequence <> source
    | "^l" as sequence <> source
    | "^m" as sequence <> source
    | "^n" as sequence <> source
    | "^o" as sequence <> source
    | "^p" as sequence <> source
    | "^q" as sequence <> source
    | "^r" as sequence <> source
    | "^s" as sequence <> source
    | "^t" as sequence <> source
    | "^u" as sequence <> source
    | "^v" as sequence <> source
    | "^w" as sequence <> source
    | "^x" as sequence <> source
    | "^y" as sequence <> source
    | "^z" as sequence <> source
    | "^A" as sequence <> source
    | "^B" as sequence <> source
    | "^C" as sequence <> source
    | "^D" as sequence <> source
    | "^E" as sequence <> source
    | "^F" as sequence <> source
    | "^G" as sequence <> source
    | "^H" as sequence <> source
    | "^I" as sequence <> source
    | "^J" as sequence <> source
    | "^K" as sequence <> source
    | "^L" as sequence <> source
    | "^M" as sequence <> source
    | "^N" as sequence <> source
    | "^O" as sequence <> source
    | "^P" as sequence <> source
    | "^Q" as sequence <> source
    | "^R" as sequence <> source
    | "^S" as sequence <> source
    | "^T" as sequence <> source
    | "^U" as sequence <> source
    | "^V" as sequence <> source
    | "^W" as sequence <> source
    | "^X" as sequence <> source
    | "^Y" as sequence <> source
    | "^Z" as sequence <> source
    | "^@" as sequence <> source
    | "^[" as sequence <> source
    | "^\\" as sequence <> source
    | "^]" as sequence <> source
    | "^^" as sequence <> source
    | "^_" as sequence <> source
    | "^?" as sequence <> source -> #(advance(lexer, source), sequence)

    "x{" <> _source -> lex_brace_escape_sequence(lexer)
    "x" <> source -> lex_hex_escape_sequence(advance(lexer, source))

    "0" as char <> source
    | "1" as char <> source
    | "2" as char <> source
    | "3" as char <> source
    | "4" as char <> source
    | "5" as char <> source
    | "6" as char <> source
    | "7" as char <> source ->
      lex_octal_escape_sequence(advance(lexer, source), char)

    _ ->
      case string.pop_grapheme(lexer.source) {
        Error(_) -> #(error(lexer, UnterminatedEscapeSequence), "")
        Ok(#(char, source)) -> #(advance(lexer, source), char)
      }
  }
}

fn lex_octal_escape_sequence(lexer: Lexer, first: String) -> #(Lexer, String) {
  case extract_octal_digit(lexer) {
    Error(_) -> #(lexer, first)
    Ok(#(lexer, second)) ->
      case extract_octal_digit(lexer) {
        Error(_) -> #(lexer, first <> second)
        Ok(#(lexer, third)) -> #(lexer, first <> second <> third)
      }
  }
}

fn extract_octal_digit(lexer: Lexer) -> Result(#(Lexer, String), Nil) {
  case lexer.source {
    "0" as char <> source
    | "1" as char <> source
    | "2" as char <> source
    | "3" as char <> source
    | "4" as char <> source
    | "5" as char <> source
    | "6" as char <> source
    | "7" as char <> source -> Ok(#(advance(lexer, source), char))
    _ -> Error(Nil)
  }
}

fn lex_hex_escape_sequence(lexer: Lexer) -> #(Lexer, String) {
  case extract_hex_digit(lexer) {
    Error(_) -> #(error(lexer, UnterminatedEscapeSequence), "x")
    Ok(#(lexer, first)) ->
      case extract_hex_digit(lexer) {
        Error(_) -> #(error(lexer, UnterminatedEscapeSequence), "x" <> first)
        Ok(#(lexer, second)) -> #(lexer, "x" <> first <> second)
      }
  }
}

fn extract_hex_digit(lexer: Lexer) -> Result(#(Lexer, String), Nil) {
  case lexer.source {
    "0" as char <> source
    | "1" as char <> source
    | "2" as char <> source
    | "3" as char <> source
    | "4" as char <> source
    | "5" as char <> source
    | "6" as char <> source
    | "7" as char <> source
    | "8" as char <> source
    | "9" as char <> source
    | "a" as char <> source
    | "b" as char <> source
    | "c" as char <> source
    | "d" as char <> source
    | "e" as char <> source
    | "f" as char <> source
    | "A" as char <> source
    | "B" as char <> source
    | "C" as char <> source
    | "D" as char <> source
    | "E" as char <> source
    | "F" as char <> source -> Ok(#(advance(lexer, source), char))
    _ -> Error(Nil)
  }
}

fn lex_brace_escape_sequence(lexer: Lexer) -> #(Lexer, String) {
  case
    splitter.split_after(lexer.splitters.brace_escape_sequence, lexer.source)
  {
    #(before, "") -> #(error(lexer, UnterminatedEscapeSequence), before)
    #(before, after) -> #(advance(lexer, after), before)
  }
}

type LexNumberMode {
  Initial
  Radix(Int)
  Decimal
  Exponent
}

type DelimitedPosition {
  AfterDecimal
  AfterNumber
  AfterSeparator
  AfterExponent
  AfterRadix
}

fn lex_number(
  lexer: Lexer,
  lexed: String,
  mode: LexNumberMode,
  position: DelimitedPosition,
) -> #(Lexer, Token) {
  let radix = case mode {
    Radix(r) -> r
    Initial | Decimal | Exponent -> 10
  }

  case lexer.source {
    "0" as char <> source | "1" as char <> source ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "2" as char <> source if radix >= 3 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "3" as char <> source if radix >= 4 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "4" as char <> source if radix >= 5 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "5" as char <> source if radix >= 6 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "6" as char <> source if radix >= 7 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "7" as char <> source if radix >= 8 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "8" as char <> source if radix >= 9 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "9" as char <> source if radix >= 10 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "a" as char <> source | "A" as char <> source if radix >= 11 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "b" as char <> source | "B" as char <> source if radix >= 12 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "c" as char <> source | "C" as char <> source if radix >= 13 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "d" as char <> source | "D" as char <> source if radix >= 14 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "e" as char <> source | "E" as char <> source if radix >= 15 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "f" as char <> source | "F" as char <> source if radix >= 16 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "g" as char <> source | "G" as char <> source if radix >= 17 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "h" as char <> source | "H" as char <> source if radix >= 18 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "i" as char <> source | "I" as char <> source if radix >= 19 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "j" as char <> source | "J" as char <> source if radix >= 20 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "k" as char <> source | "K" as char <> source if radix >= 21 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "l" as char <> source | "L" as char <> source if radix >= 22 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "m" as char <> source | "M" as char <> source if radix >= 23 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "n" as char <> source | "N" as char <> source if radix >= 24 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "o" as char <> source | "O" as char <> source if radix >= 25 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "p" as char <> source | "P" as char <> source if radix >= 26 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "q" as char <> source | "Q" as char <> source if radix >= 27 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "r" as char <> source | "R" as char <> source if radix >= 28 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "s" as char <> source | "S" as char <> source if radix >= 29 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "t" as char <> source | "T" as char <> source if radix >= 30 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "u" as char <> source | "U" as char <> source if radix >= 31 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "v" as char <> source | "V" as char <> source if radix >= 32 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "w" as char <> source | "W" as char <> source if radix >= 33 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "x" as char <> source | "X" as char <> source if radix >= 34 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "y" as char <> source | "Y" as char <> source if radix >= 35 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)
    "z" as char <> source | "Z" as char <> source if radix >= 36 ->
      lex_number(advance(lexer, source), lexed <> char, mode, AfterNumber)

    "#" <> source if mode == Initial && position == AfterNumber ->
      case int.parse(string.replace(in: lexed, each: "_", with: "")) {
        Error(_) -> #(
          error(advance(lexer, source), InvalidRadix(lexed)),
          Integer(lexed),
        )
        Ok(radix) if radix < 2 || radix > 36 -> #(
          error(advance(lexer, source), InvalidRadix(lexed)),
          Integer(lexed),
        )
        Ok(radix) ->
          lex_number(
            advance(lexer, source),
            lexed <> "#",
            Radix(radix),
            AfterRadix,
          )
      }

    "_" <> source if position == AfterNumber ->
      lex_number(advance(lexer, source), lexed <> "_", mode, AfterSeparator)

    "_" <> _ -> #(error(lexer, NumericSeparatorNotAllowed), Integer(lexed))

    "." <> source if mode == Initial && position == AfterNumber ->
      lex_number(advance(lexer, source), lexed <> ".", Decimal, AfterDecimal)

    "e-" as prefix <> source
      | "e" as prefix <> source
      | "E-" as prefix <> source
      | "E" as prefix <> source
      if mode == Decimal && position == AfterNumber
    ->
      lex_number(
        advance(lexer, source),
        lexed <> prefix,
        Exponent,
        AfterExponent,
      )

    _ -> {
      let token = case mode {
        Decimal | Exponent -> Float(lexed)
        Initial | Radix(_) -> Integer(lexed)
      }
      case position {
        // If we have some code that looks like `15.`, that is valid syntax,
        // but it's an integer followed by a dot, not a float.
        AfterDecimal -> #(
          advance(lexer, "." <> lexer.source),
          Integer(string.drop_end(lexed, 1)),
        )
        AfterExponent -> #(error(lexer, ExpectedExponent), token)
        AfterRadix -> #(error(lexer, NumberCannotEndAfterRadix), token)
        AfterNumber -> #(lexer, token)
        AfterSeparator -> #(error(lexer, NumericSeparatorNotAllowed), token)
      }
    }
  }
}

fn lex_sigil(lexer: Lexer) -> #(Lexer, Token) {
  let #(lexer, sigil, verbatim) = case lexer.source {
    "b" as sigil <> source | "s" as sigil <> source -> #(
      advance(lexer, source),
      sigil,
      False,
    )

    "B" as sigil <> source | "S" as sigil <> source -> #(
      advance(lexer, source),
      sigil,
      True,
    )
    _ -> #(lexer, "", False)
  }

  case lexer.source {
    "\"\"\"" <> source ->
      lex_triple_quoted_string(advance(lexer, source), Some(sigil))
    _ -> {
      let #(lexer, delimiter, closing_char) = case lexer.source {
        "(" <> source -> #(advance(lexer, source), SigilParen, ")")
        "[" <> source -> #(advance(lexer, source), SigilSquare, "]")
        "{" <> source -> #(advance(lexer, source), SigilBrace, "}")
        "<" <> source -> #(advance(lexer, source), SigilAngle, ">")

        "/" <> source -> #(advance(lexer, source), SigilSlash, "/")
        "|" <> source -> #(advance(lexer, source), SigilPipe, "|")
        "'" <> source -> #(advance(lexer, source), SigilSingleQuote, "'")
        "\"" <> source -> #(advance(lexer, source), SigilDoubleQuote, "\"")
        "`" <> source -> #(advance(lexer, source), SigilBacktick, "`")
        "#" <> source -> #(advance(lexer, source), SigilHash, "#")

        _ -> #(error(lexer, ExpectedSigilDelimiter), SigilNone, "")
      }

      case delimiter {
        SigilNone -> #(
          lexer,
          UnterminatedSigil(sigil:, delimiter:, contents: ""),
        )
        _ -> {
          let splitter = case verbatim {
            False -> lexer.splitters.sigil
            True -> lexer.splitters.sigil_verbatim
          }

          do_lex_sigil(lexer, sigil, delimiter, closing_char, splitter, "")
        }
      }
    }
  }
}

fn do_lex_sigil(
  lexer: Lexer,
  sigil: String,
  delimiter: SigilDelimiter,
  closing_char: String,
  splitter: Splitter,
  contents: String,
) -> #(Lexer, Token) {
  let #(before, split, after) = splitter.split(splitter, lexer.source)
  case split {
    "" -> #(
      error(advance(lexer, after), UnterminatedStringLiteral),
      UnterminatedSigil(sigil:, delimiter:, contents: contents <> before),
    )

    "\\" ->
      case string.pop_grapheme(after) {
        Error(_) -> #(
          error(advance(lexer, after), UnterminatedStringLiteral),
          UnterminatedSigil(
            sigil:,
            delimiter:,
            contents: contents <> before <> "\\",
          ),
        )
        Ok(#(character, source)) ->
          do_lex_sigil(
            advance(lexer, source),
            sigil,
            delimiter,
            closing_char,
            splitter,
            contents <> before <> "\\" <> character,
          )
      }

    _ if split == closing_char -> #(
      advance(lexer, after),
      Sigil(sigil:, delimiter:, contents: contents <> before),
    )

    // Here, we've split on a delimiter which doesn't match the current sigil.
    // In this case, we must continue lexing until we find a delimiter of the
    // correct kind.
    _ ->
      do_lex_sigil(
        advance(lexer, after),
        sigil,
        delimiter,
        closing_char,
        splitter,
        contents <> before <> split,
      )
  }
}

fn lex_string(lexer: Lexer, contents: String) -> #(Lexer, Token) {
  let #(before, split, after) =
    splitter.split(lexer.splitters.string, lexer.source)
  case split {
    "" -> #(
      error(advance(lexer, after), UnterminatedStringLiteral),
      UnterminatedString(contents <> before),
    )

    "\\" -> {
      let #(lexer, escape) = lex_escape_sequence(advance(lexer, after))
      lex_string(lexer, contents <> before <> "\\" <> escape)
    }

    _ -> #(advance(lexer, after), String(contents <> before))
  }
}

fn lex_triple_quoted_string(
  lexer: Lexer,
  sigil: Option(String),
) -> #(Lexer, Token) {
  let #(lexer, extra_quotes) = count_extra_quotes(lexer, 0)

  let #(lexer, beginning_whitespace) = case
    splitter.split(lexer.splitters.until_end_of_line, lexer.source)
  {
    #(_, "", _) -> #(error(lexer, ExpectedWhitespaceAfterTripleQuote), "")
    #(before, newline, after) ->
      case is_whitespace(before) {
        True -> #(advance(lexer, after), before <> newline)
        False -> #(error(lexer, ExpectedWhitespaceAfterTripleQuote), "")
      }
  }

  let #(lexer, lines, end_indentation) =
    lex_triple_quoted_string_contents(lexer, [], "", extra_quotes)

  case strip_line_prefixes(lines, end_indentation, []) {
    Error(line) -> {
      let contents =
        beginning_whitespace
        <> string.join(list.reverse(lines), "\n")
        <> "\n"
        <> end_indentation
      #(
        error(
          lexer,
          InvalidTripleQuotedStringIndentation(
            expected_indentation: end_indentation,
            line:,
          ),
        ),
        InvalidTripleQuotedString(contents),
      )
    }
    Ok(lines) -> #(
      lexer,
      TripleQuotedString(
        sigil:,
        number_of_quotes: extra_quotes + 3,
        beginning_whitespace:,
        lines:,
        end_indentation:,
      ),
    )
  }
}

fn count_extra_quotes(lexer: Lexer, extra: Int) -> #(Lexer, Int) {
  case lexer.source {
    "\"" <> source -> count_extra_quotes(advance(lexer, source), extra + 1)
    _ -> #(lexer, extra)
  }
}

fn is_whitespace(string: String) -> Bool {
  case string {
    "" -> True
    " " <> string
    | "\n" <> string
    | "\r" <> string
    | "\t" <> string
    | "\f" <> string -> is_whitespace(string)
    _ -> False
  }
}

fn strip_line_prefixes(
  lines: List(String),
  end_indentation: String,
  acc: List(String),
) -> Result(List(String), String) {
  case lines {
    [] -> Ok(acc)
    [line, ..lines] ->
      case strip_prefix(line, end_indentation) {
        Ok(line) -> strip_line_prefixes(lines, end_indentation, [line, ..acc])
        Error(_) -> Error(line)
      }
  }
}

@external(erlang, "pearl_ffi", "strip_prefix")
@external(javascript, "./pearl_ffi.mjs", "strip_prefix")
fn strip_prefix(string: String, prefix: String) -> Result(String, Nil)

fn lex_triple_quoted_string_contents(
  lexer: Lexer,
  lines: List(String),
  current_line: String,
  extra_quotes: Int,
) -> #(Lexer, List(String), String) {
  let #(before, split, after) =
    splitter.split(lexer.splitters.triple_quoted_string, lexer.source)

  let before = current_line <> before

  case split {
    "\"\"\"" -> {
      let lexer = advance(lexer, after)
      case is_whitespace(before) {
        False ->
          lex_triple_quoted_string_contents(
            lexer,
            lines,
            before <> "\"\"\"",
            extra_quotes,
          )
        True if extra_quotes == 0 -> #(lexer, lines, before)
        True ->
          case consume_extra_quotes(lexer, extra_quotes) {
            Ok(lexer) -> #(lexer, lines, before)
            Error(Nil) ->
              lex_triple_quoted_string_contents(
                lexer,
                lines,
                before <> "\"\"\"",
                extra_quotes,
              )
          }
      }
    }

    "\n" | "\r\n" ->
      lex_triple_quoted_string_contents(
        advance(lexer, after),
        [before, ..lines],
        "",
        extra_quotes,
      )

    _ -> #(error(lexer, UnterminatedStringLiteral), [before, ..lines], "")
  }
}

fn consume_extra_quotes(lexer: Lexer, extra_quotes: Int) -> Result(Lexer, Nil) {
  case extra_quotes, lexer.source {
    0, _ -> Ok(lexer)
    _, "\"" <> source ->
      consume_extra_quotes(advance(lexer, source), extra_quotes - 1)
    _, _ -> Error(Nil)
  }
}

fn lex_quoted_atom(lexer: Lexer, contents: String) -> #(Lexer, Token) {
  let #(before, split, after) =
    splitter.split(lexer.splitters.quoted_atom, lexer.source)
  case split {
    "" -> #(
      error(advance(lexer, after), UnterminatedQuotedAtom),
      UnterminatedAtom(contents <> before),
    )

    "\\" ->
      case string.pop_grapheme(after) {
        Error(_) -> #(
          error(advance(lexer, after), UnterminatedStringLiteral),
          UnterminatedString(contents),
        )
        Ok(#(character, source)) ->
          lex_string(
            advance(lexer, source),
            contents <> before <> "\\" <> character,
          )
      }

    _ -> #(advance(lexer, after), Atom(contents <> before, True))
  }
}

fn lex_variable_or_atom(lexer: Lexer, lexed: String) -> #(Lexer, String) {
  case lexer.source {
    "a" as char <> source
    | "b" as char <> source
    | "c" as char <> source
    | "d" as char <> source
    | "e" as char <> source
    | "f" as char <> source
    | "g" as char <> source
    | "h" as char <> source
    | "i" as char <> source
    | "j" as char <> source
    | "k" as char <> source
    | "l" as char <> source
    | "m" as char <> source
    | "n" as char <> source
    | "o" as char <> source
    | "p" as char <> source
    | "q" as char <> source
    | "r" as char <> source
    | "s" as char <> source
    | "t" as char <> source
    | "u" as char <> source
    | "v" as char <> source
    | "w" as char <> source
    | "x" as char <> source
    | "y" as char <> source
    | "z" as char <> source
    | "A" as char <> source
    | "B" as char <> source
    | "C" as char <> source
    | "D" as char <> source
    | "E" as char <> source
    | "F" as char <> source
    | "G" as char <> source
    | "H" as char <> source
    | "I" as char <> source
    | "J" as char <> source
    | "K" as char <> source
    | "L" as char <> source
    | "M" as char <> source
    | "N" as char <> source
    | "O" as char <> source
    | "P" as char <> source
    | "Q" as char <> source
    | "R" as char <> source
    | "S" as char <> source
    | "T" as char <> source
    | "U" as char <> source
    | "V" as char <> source
    | "W" as char <> source
    | "X" as char <> source
    | "Y" as char <> source
    | "Z" as char <> source
    | "0" as char <> source
    | "1" as char <> source
    | "2" as char <> source
    | "3" as char <> source
    | "4" as char <> source
    | "5" as char <> source
    | "6" as char <> source
    | "7" as char <> source
    | "8" as char <> source
    | "9" as char <> source
    | "_" as char <> source
    | "@" as char <> source ->
      lex_variable_or_atom(advance(lexer, source), lexed <> char)

    _ -> #(lexer, lexed)
  }
}

fn lex_variable(lexer: Lexer, char: String) -> #(Lexer, Token) {
  let #(lexer, name) = lex_variable_or_atom(lexer, char)
  #(lexer, Variable(name))
}

fn lex_atom(lexer: Lexer, char: String) -> #(Lexer, Token) {
  let #(lexer, name) = lex_variable_or_atom(lexer, char)

  let token = case name {
    "after" -> After
    "begin" -> Begin
    "case" -> Case
    "catch" -> Catch
    "cond" -> Cond
    "else" -> Else
    "end" -> End
    "fun" -> Fun
    "if" -> If
    "let" -> Let
    "maybe" -> Maybe
    "of" -> Of
    "receive" -> Receive
    "try" -> Try
    "when" -> When
    "bnot" -> Bnot
    "div" -> Div
    "rem" -> Rem
    "band" -> Band
    "bor" -> Bor
    "bxor" -> Bxor
    "bsl" -> Bsl
    "bsr" -> Bsr
    "not" -> Not
    "and" -> And
    "or" -> Or
    "xor" -> Xor
    "andalso" -> Andalso
    "orelse" -> Orelse

    _ -> Atom(name, False)
  }
  #(lexer, token)
}

fn lex_until_end_of_line(lexer: Lexer) -> #(Lexer, String) {
  let #(before, after) =
    splitter.split_after(lexer.splitters.until_end_of_line, lexer.source)
  #(advance(lexer, after), before)
}

fn lex_whitespace(lexer: Lexer, lexed: String) -> #(Lexer, Token) {
  case lexer.source {
    " " as space <> source
    | "\n" as space <> source
    | "\r" as space <> source
    | "\t" as space <> source
    | "\f" as space <> source ->
      lex_whitespace(advance(lexer, source), lexed <> space)
    _ -> maybe_token(lexer, Whitespace(lexed), !lexer.ignore_whitespace)
  }
}

fn maybe_token(lexer: Lexer, token: Token, condition: Bool) -> #(Lexer, Token) {
  case condition {
    True -> #(lexer, token)
    False -> next(lexer)
  }
}

fn advance(lexer: Lexer, source: String) -> Lexer {
  Lexer(..lexer, source:)
}

fn error(lexer: Lexer, error: Error) -> Lexer {
  Lexer(..lexer, errors: [error, ..lexer.errors])
}

/// A highlighting token, containing information about the kind of syntax
/// being used. Many similar tokens (e.g. all keywords) are grouped together 
/// to simplify them.
/// 
/// For syntax tokens, see [`Token`](#Token).
/// 
pub type HighlightToken {
  HighlightWhitespace(String)
  HighlightKeyword(String)
  HighlightVariable(String)
  HighlightString(String)
  HighlightAtom(String)
  HighlightNumber(String)
  HighlightModule(String)
  HighlightFunction(String)
  HighlightOperator(String)
  HighlightComment(String)
  HighlightPunctuation(String)
  HighlightOther(String)
}

/// Convert a string of Erlang source code into ansi highlighting.
/// 
/// Colours taken from [`contour`](https://hexdocs.pm/contour):
/// | Token                  | Colour      |
/// | ---------------------- | ----------- |
/// | Keyword                | Yellow      |
/// | Module                 | Cyan        |
/// | Function               | Blue        |
/// | Operator               | Magenta     |
/// | Comment                | Italic grey |
/// | String, Number, Atom   | Green       |
/// | Whitespace, Variable   | No colour   |
///
/// If you wish to use other colours or another format, use `to_tokens`.
/// 
pub fn highlight_ansi(code: String) -> String {
  highlight_tokens(code)
  |> list.fold("", fn(code, token) {
    code
    <> case token {
      HighlightWhitespace(s) -> ansi.reset(s)
      HighlightKeyword(s) -> ansi.yellow(s)
      HighlightVariable(s) -> ansi.reset(s)
      HighlightString(s) -> ansi.green(s)
      HighlightAtom(s) -> ansi.green(s)
      HighlightNumber(s) -> ansi.green(s)
      HighlightModule(s) -> ansi.cyan(s)
      HighlightFunction(s) -> ansi.blue(s)
      HighlightOperator(s) -> ansi.magenta(s)
      HighlightComment(s) -> ansi.italic(ansi.gray(s))
      HighlightPunctuation(s) -> ansi.reset(s)
      HighlightOther(s) -> ansi.reset(s)
    }
  })
}

/// Convert a string of Erlang source code into an HTML string.
/// Each token is wrapped in a `<span>` with a class indicating the type of 
/// 
/// Class names taken from [`contour`](https://hexdocs.pm/contour):
/// | Token       | CSS class      |
/// | ----------- | -------------- |
/// | Keyword     | hl-keyword     |
/// | Variable    | hl-variable    |
/// | Module      | hl-module      |
/// | Function    | hl-function    |
/// | Operator    | hl-operator    |
/// | Punctuation | hl-punctuation |
/// | Comment     | hl-comment     |
/// | String      | hl-string      |
/// | Atom        | hl-atom        |
/// | Number      | hl-number      |
/// | Whitespace  | no class       |
///
/// Place the output within a `<pre><code>...</code></pre>` and add styling for
/// these CSS classes to get highlighting on your website. Here's some CSS you
/// could use:
///
/// ```css
/// pre code .hl-comment  { color: #d4d4d4; font-style: italic }
/// pre code .hl-function { color: #9ce7ff }
/// pre code .hl-keyword  { color: #ffd596 }
/// pre code .hl-operator { color: #ffaff3 }
/// pre code .hl-string   { color: #c8ffa7 }
/// pre code .hl-number   { color: #c8ffa7 }
/// pre code .hl-atom     { color: #c8ffa7 }
/// pre code .hl-module   { color: #ffddfa }
/// ```
///
/// If you wish to use another format see `to_ansi` or `to_tokens`.
///
pub fn highlight_html(code: String) -> String {
  highlight_tokens(code)
  |> list.fold("", fn(acc, token) {
    case token {
      HighlightWhitespace(s) -> acc <> s
      HighlightKeyword(s) ->
        acc <> "<span class=hl-keyword>" <> houdini.escape(s) <> "</span>"
      HighlightVariable(s) ->
        acc <> "<span class=hl-variable>" <> houdini.escape(s) <> "</span>"
      HighlightString(s) ->
        acc <> "<span class=hl-string>" <> houdini.escape(s) <> "</span>"
      HighlightAtom(s) ->
        acc <> "<span class=hl-atom>" <> houdini.escape(s) <> "</span>"
      HighlightNumber(s) ->
        acc <> "<span class=hl-number>" <> houdini.escape(s) <> "</span>"
      HighlightModule(s) ->
        acc <> "<span class=hl-module>" <> houdini.escape(s) <> "</span>"
      HighlightFunction(s) ->
        acc <> "<span class=hl-function>" <> houdini.escape(s) <> "</span>"
      HighlightOperator(s) ->
        acc <> "<span class=hl-operator>" <> houdini.escape(s) <> "</span>"
      HighlightComment(s) ->
        acc <> "<span class=hl-comment>" <> houdini.escape(s) <> "</span>"
      HighlightPunctuation(s) ->
        acc <> "<span class=hl-punctuation>" <> houdini.escape(s) <> "</span>"
      HighlightOther(s) -> acc <> s
    }
  })
}

/// Convert a string of Erlang source code into highlighting tokens.
/// Highlighting tokens only contain information about the kind of syntax
/// being used, grouping similar tokens (e.g. all keywords) into one category.
/// 
/// To convert code into syntax tokens, see `pearl.tokenise`.
/// 
pub fn highlight_tokens(code: String) -> List(HighlightToken) {
  let #(tokens, _errors) = tokenise(new(code))
  do_highlight_tokens(tokens, [])
}

fn do_highlight_tokens(
  in: List(Token),
  out: List(HighlightToken),
) -> List(HighlightToken) {
  case in {
    [] -> list.reverse(out)

    // Specific constructs
    [Atom(value, quoted: False), LeftParen, ..in] ->
      do_highlight_tokens(in, [
        HighlightPunctuation("("),
        HighlightFunction(value),
        ..out
      ])
    [Atom(function, quoted: False), Slash, Integer(arity), ..in] ->
      do_highlight_tokens(in, [
        HighlightNumber(arity),
        HighlightPunctuation("/"),
        HighlightFunction(function),
        ..out
      ])
    [
      Atom(module, quoted: False),
      Colon,
      Atom(function, quoted: False),
      Slash,
      Integer(arity),
      ..in
    ] ->
      do_highlight_tokens(in, [
        HighlightNumber(arity),
        HighlightPunctuation("/"),
        HighlightFunction(function),
        HighlightPunctuation(":"),
        HighlightModule(module),
        ..out
      ])
    [Atom(module, quoted: False), Colon, Atom(function, quoted: False), ..in] ->
      do_highlight_tokens(in, [
        HighlightFunction(function),
        HighlightPunctuation(":"),
        HighlightModule(module),
        ..out
      ])
    [Question, Variable(macro_name), ..in] ->
      do_highlight_tokens(in, [
        HighlightFunction(macro_name),
        HighlightPunctuation("?"),
        ..out
      ])

    // Whitespace and comments
    [Whitespace(space), ..in] ->
      do_highlight_tokens(in, [HighlightWhitespace(space), ..out])
    [Comment(contents), ..in] ->
      do_highlight_tokens(in, [HighlightComment("%" <> contents), ..out])
    [DocComment(contents), ..in] ->
      do_highlight_tokens(in, [HighlightComment("%%" <> contents), ..out])
    [ModuleComment(contents), ..in] ->
      do_highlight_tokens(in, [HighlightComment("%%%" <> contents), ..out])
    [EndOfFile, ..in] -> do_highlight_tokens(in, out)

    // Literals
    [Character(char), ..in] ->
      do_highlight_tokens(in, [HighlightString("$" <> char), ..out])
    [Integer(int), ..in] ->
      do_highlight_tokens(in, [HighlightNumber(int), ..out])
    [Float(float), ..in] ->
      do_highlight_tokens(in, [HighlightNumber(float), ..out])
    [Atom(name:, quoted: True), ..in] ->
      do_highlight_tokens(in, [HighlightAtom("'" <> name <> "'"), ..out])
    [Atom(name:, quoted: False), ..in] ->
      do_highlight_tokens(in, [HighlightAtom(name), ..out])
    [String(contents), ..in] ->
      do_highlight_tokens(in, [HighlightString("\"" <> contents <> "\""), ..out])
    [
      TripleQuotedString(
        sigil:,
        number_of_quotes:,
        beginning_whitespace:,
        lines:,
        end_indentation:,
      ),
      ..in
    ] ->
      do_highlight_tokens(in, [
        HighlightString(
          case sigil {
            option.None -> ""
            option.Some(sigil) -> "~" <> sigil
          }
          <> string.repeat("\"", number_of_quotes)
          <> beginning_whitespace
          <> string.join(
            list.map(lines, fn(line) { end_indentation <> line }),
            "\n",
          )
          <> "\n"
          <> end_indentation
          <> string.repeat("\"", number_of_quotes),
        ),
        ..out
      ])
    [Sigil(sigil:, delimiter:, contents:), ..in] ->
      do_highlight_tokens(in, [
        HighlightString({
          let #(opening, closing) = sigil_delimiters(delimiter)
          "~" <> sigil <> opening <> contents <> closing
        }),
        ..out
      ])
    [Variable(name), ..in] ->
      do_highlight_tokens(in, [HighlightVariable(name), ..out])

    // Keywords
    [After, ..in] -> do_highlight_tokens(in, [HighlightKeyword("after"), ..out])
    [Begin, ..in] -> do_highlight_tokens(in, [HighlightKeyword("begin"), ..out])
    [Case, ..in] -> do_highlight_tokens(in, [HighlightKeyword("case"), ..out])
    [Catch, ..in] -> do_highlight_tokens(in, [HighlightKeyword("catch"), ..out])
    [Cond, ..in] -> do_highlight_tokens(in, [HighlightKeyword("cond"), ..out])
    [Else, ..in] -> do_highlight_tokens(in, [HighlightKeyword("else"), ..out])
    [End, ..in] -> do_highlight_tokens(in, [HighlightKeyword("end"), ..out])
    [Fun, ..in] -> do_highlight_tokens(in, [HighlightKeyword("fun"), ..out])
    [If, ..in] -> do_highlight_tokens(in, [HighlightKeyword("if"), ..out])
    [Let, ..in] -> do_highlight_tokens(in, [HighlightKeyword("let"), ..out])
    [Maybe, ..in] -> do_highlight_tokens(in, [HighlightKeyword("maybe"), ..out])
    [Of, ..in] -> do_highlight_tokens(in, [HighlightKeyword("of"), ..out])
    [Receive, ..in] ->
      do_highlight_tokens(in, [HighlightKeyword("receive"), ..out])
    [Try, ..in] -> do_highlight_tokens(in, [HighlightKeyword("try"), ..out])
    [When, ..in] -> do_highlight_tokens(in, [HighlightKeyword("when"), ..out])

    // Punctuation
    [LeftParen, ..in] ->
      do_highlight_tokens(in, [HighlightPunctuation("("), ..out])
    [RightParen, ..in] ->
      do_highlight_tokens(in, [HighlightPunctuation(")"), ..out])
    [LeftBrace, ..in] ->
      do_highlight_tokens(in, [HighlightPunctuation("{"), ..out])
    [RightBrace, ..in] ->
      do_highlight_tokens(in, [HighlightPunctuation("}"), ..out])
    [LeftSquare, ..in] ->
      do_highlight_tokens(in, [HighlightPunctuation("["), ..out])
    [RightSquare, ..in] ->
      do_highlight_tokens(in, [HighlightPunctuation("]"), ..out])
    [Comma, ..in] -> do_highlight_tokens(in, [HighlightPunctuation(","), ..out])
    [Semicolon, ..in] ->
      do_highlight_tokens(in, [HighlightPunctuation(";"), ..out])
    [Colon, ..in] -> do_highlight_tokens(in, [HighlightPunctuation(":"), ..out])
    [Dot, ..in] -> do_highlight_tokens(in, [HighlightPunctuation("."), ..out])
    [MinusGreater, ..in] ->
      do_highlight_tokens(in, [HighlightPunctuation("->"), ..out])
    [DoubleLess, ..in] ->
      do_highlight_tokens(in, [HighlightPunctuation("<<"), ..out])
    [DoubleGreater, ..in] ->
      do_highlight_tokens(in, [HighlightPunctuation(">>"), ..out])
    [Hash, ..in] -> do_highlight_tokens(in, [HighlightPunctuation("#"), ..out])
    [DoubleColon, ..in] ->
      do_highlight_tokens(in, [HighlightPunctuation("::"), ..out])
    [DoubleDot, ..in] ->
      do_highlight_tokens(in, [HighlightPunctuation(".."), ..out])
    [TripleDot, ..in] ->
      do_highlight_tokens(in, [HighlightPunctuation("..."), ..out])
    [Question, ..in] ->
      do_highlight_tokens(in, [HighlightPunctuation("?"), ..out])

    // Operators
    [DoublePipe, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("||"), ..out])
    [EqualGreater, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("=>"), ..out])
    [ColonEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator(":="), ..out])
    [LessMinus, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("<-"), ..out])
    [LessEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("<="), ..out])
    [Pipe, ..in] -> do_highlight_tokens(in, [HighlightOperator("|"), ..out])
    [DoubleEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("=="), ..out])
    [SlashEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("/="), ..out])
    [EqualLess, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("=<"), ..out])
    [Less, ..in] -> do_highlight_tokens(in, [HighlightOperator("<"), ..out])
    [GreaterEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator(">="), ..out])
    [Greater, ..in] -> do_highlight_tokens(in, [HighlightOperator(">"), ..out])
    [EqualColonEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("=:="), ..out])
    [EqualSlashEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("=/="), ..out])
    [Plus, ..in] -> do_highlight_tokens(in, [HighlightOperator("+"), ..out])
    [Minus, ..in] -> do_highlight_tokens(in, [HighlightOperator("-"), ..out])
    [Star, ..in] -> do_highlight_tokens(in, [HighlightOperator("*"), ..out])
    [Slash, ..in] -> do_highlight_tokens(in, [HighlightOperator("/"), ..out])
    [Bnot, ..in] -> do_highlight_tokens(in, [HighlightOperator("bnot"), ..out])
    [Div, ..in] -> do_highlight_tokens(in, [HighlightOperator("div"), ..out])
    [Rem, ..in] -> do_highlight_tokens(in, [HighlightOperator("rem"), ..out])
    [Band, ..in] -> do_highlight_tokens(in, [HighlightOperator("band"), ..out])
    [Bor, ..in] -> do_highlight_tokens(in, [HighlightOperator("bor"), ..out])
    [Bxor, ..in] -> do_highlight_tokens(in, [HighlightOperator("bxor"), ..out])
    [Bsl, ..in] -> do_highlight_tokens(in, [HighlightOperator("bsl"), ..out])
    [Bsr, ..in] -> do_highlight_tokens(in, [HighlightOperator("bsr"), ..out])
    [Not, ..in] -> do_highlight_tokens(in, [HighlightOperator("not"), ..out])
    [And, ..in] -> do_highlight_tokens(in, [HighlightOperator("and"), ..out])
    [Or, ..in] -> do_highlight_tokens(in, [HighlightOperator("or"), ..out])
    [Xor, ..in] -> do_highlight_tokens(in, [HighlightOperator("xor"), ..out])
    [Andalso, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("andalso"), ..out])
    [Orelse, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("orelse"), ..out])
    [DoublePlus, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("++"), ..out])
    [DoubleMinus, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("--"), ..out])
    [QuestionEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("?="), ..out])
    [Bang, ..in] -> do_highlight_tokens(in, [HighlightOperator("!"), ..out])
    [Equal, ..in] -> do_highlight_tokens(in, [HighlightOperator("="), ..out])

    // Invalid tokens
    [Unknown(char), ..in] ->
      do_highlight_tokens(in, [HighlightOther(char), ..out])
    [UnterminatedString(contents), ..in] ->
      do_highlight_tokens(in, [HighlightString("\"" <> contents), ..out])
    [UnterminatedSigil(sigil:, contents:, delimiter:), ..in] ->
      do_highlight_tokens(in, [
        HighlightString({
          let #(opening, _closing) = sigil_delimiters(delimiter)
          "~" <> sigil <> opening <> contents
        }),
        ..out
      ])
    [UnterminatedAtom(contents), ..in] ->
      do_highlight_tokens(in, [HighlightAtom("'" <> contents), ..out])
    [InvalidTripleQuotedString(contents), ..in] ->
      do_highlight_tokens(in, [
        HighlightString("\"\"\"" <> contents <> "\"\"\""),
        ..out
      ])
  }
}

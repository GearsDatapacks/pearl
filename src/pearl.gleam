import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import pearl/token.{type Token}
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
  UnterminatedString
  UnterminatedAtom
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

pub fn to_source(tokens: List(Token)) -> String {
  list.fold(tokens, "", fn(code, token) { code <> token.to_source(token) })
}

fn do_tokenise(lexer: Lexer, tokens: List(Token)) -> #(List(Token), List(Error)) {
  case next(lexer) {
    #(lexer, token.EndOfFile) -> #(
      list.reverse([token.EndOfFile, ..tokens]),
      list.reverse(lexer.errors),
    )
    #(lexer, token) -> do_tokenise(lexer, [token, ..tokens])
  }
}

fn next(lexer: Lexer) -> #(Lexer, Token) {
  case lexer.source {
    "" -> #(lexer, token.EndOfFile)

    " " as space <> source
    | "\n" as space <> source
    | "\r" as space <> source
    | "\t" as space <> source
    | "\f" as space <> source -> lex_whitespace(advance(lexer, source), space)

    "%%%" <> source -> {
      let #(lexer, contents) = lex_until_end_of_line(advance(lexer, source))
      maybe_token(lexer, token.ModuleComment(contents), !lexer.ignore_comments)
    }
    "%%" <> source -> {
      let #(lexer, contents) = lex_until_end_of_line(advance(lexer, source))
      maybe_token(lexer, token.DocComment(contents), !lexer.ignore_comments)
    }
    "%" <> source -> {
      let #(lexer, contents) = lex_until_end_of_line(advance(lexer, source))
      maybe_token(lexer, token.Comment(contents), !lexer.ignore_comments)
    }

    "::" <> source -> #(advance(lexer, source), token.DoubleColon)
    ":=" <> source -> #(advance(lexer, source), token.ColonEqual)
    ":" <> source -> #(advance(lexer, source), token.Colon)
    "..." <> source -> #(advance(lexer, source), token.TripleDot)
    ".." <> source -> #(advance(lexer, source), token.DoubleDot)

    "(" <> source -> #(advance(lexer, source), token.LeftParen)
    ")" <> source -> #(advance(lexer, source), token.RightParen)
    "{" <> source -> #(advance(lexer, source), token.LeftBrace)
    "}" <> source -> #(advance(lexer, source), token.RightBrace)
    "[" <> source -> #(advance(lexer, source), token.LeftSquare)
    "]" <> source -> #(advance(lexer, source), token.RightSquare)

    "," <> source -> #(advance(lexer, source), token.Comma)
    ";" <> source -> #(advance(lexer, source), token.Semicolon)
    "." <> source -> #(advance(lexer, source), token.Dot)
    "->" <> source -> #(advance(lexer, source), token.MinusGreater)
    "<<" <> source -> #(advance(lexer, source), token.DoubleLess)
    ">>" <> source -> #(advance(lexer, source), token.DoubleGreater)
    "#" <> source -> #(advance(lexer, source), token.Hash)
    "||" <> source -> #(advance(lexer, source), token.DoublePipe)
    "=>" <> source -> #(advance(lexer, source), token.EqualGreater)
    "<-" <> source -> #(advance(lexer, source), token.LessMinus)
    "<=" <> source -> #(advance(lexer, source), token.LessEqual)
    "|" <> source -> #(advance(lexer, source), token.Pipe)

    "++" <> source -> #(advance(lexer, source), token.DoublePlus)
    "--" <> source -> #(advance(lexer, source), token.DoubleMinus)
    "==" <> source -> #(advance(lexer, source), token.DoubleEqual)
    "/=" <> source -> #(advance(lexer, source), token.SlashEqual)
    "=<" <> source -> #(advance(lexer, source), token.EqualLess)
    "<" <> source -> #(advance(lexer, source), token.Less)
    ">=" <> source -> #(advance(lexer, source), token.GreaterEqual)
    ">" <> source -> #(advance(lexer, source), token.Greater)
    "=:=" <> source -> #(advance(lexer, source), token.EqualColonEqual)
    "=/=" <> source -> #(advance(lexer, source), token.EqualSlashEqual)
    "+" <> source -> #(advance(lexer, source), token.Plus)
    "-" <> source -> #(advance(lexer, source), token.Minus)
    "*" <> source -> #(advance(lexer, source), token.Star)
    "/" <> source -> #(advance(lexer, source), token.Slash)
    "?=" <> source -> #(advance(lexer, source), token.QuestionEqual)
    "?" <> source -> #(advance(lexer, source), token.Question)
    "!" <> source -> #(advance(lexer, source), token.Bang)
    "=" <> source -> #(advance(lexer, source), token.Equal)

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
        Error(_) -> #(lexer, token.EndOfFile)
        Ok(#(char, source)) -> #(
          advance(error(lexer, UnknownCharacter(char)), source),
          token.Unknown(char),
        )
      }
  }
}

fn lex_character(lexer: Lexer) -> #(Lexer, Token) {
  case lexer.source {
    "\\" <> source -> {
      let #(lexer, escape_sequence) =
        lex_escape_sequence(advance(lexer, source))
      #(lexer, token.Character("\\" <> escape_sequence))
    }
    _ ->
      case string.pop_grapheme(lexer.source) {
        Ok(#(char, source)) -> #(advance(lexer, source), token.Character(char))
        Error(_) -> #(error(lexer, UnterminatedCharacter), token.Character(""))
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

    "x{" <> source -> lex_brace_escape_sequence(advance(lexer, source))
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
  case splitter.split(lexer.splitters.brace_escape_sequence, lexer.source) {
    #(before, "}", after) -> #(advance(lexer, after), "x{" <> before <> "}")
    #(before, separator, after) -> #(
      advance(error(lexer, UnterminatedEscapeSequence), separator <> after),
      "x{" <> before,
    )
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
          token.Integer(lexed),
        )
        Ok(radix) if radix < 2 || radix > 36 -> #(
          error(advance(lexer, source), InvalidRadix(lexed)),
          token.Integer(lexed),
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

    "_" <> _ -> #(
      error(lexer, NumericSeparatorNotAllowed),
      token.Integer(lexed),
    )

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
        Decimal | Exponent -> token.Float(lexed)
        Initial | Radix(_) -> token.Integer(lexed)
      }
      case position {
        // If we have some code that looks like `15.`, that is valid syntax,
        // but it's an integer followed by a dot, not a float.
        AfterDecimal -> #(
          advance(lexer, "." <> lexer.source),
          token.Integer(string.drop_end(lexed, 1)),
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
        "(" <> source -> #(advance(lexer, source), token.SigilParen, ")")
        "[" <> source -> #(advance(lexer, source), token.SigilSquare, "]")
        "{" <> source -> #(advance(lexer, source), token.SigilBrace, "}")
        "<" <> source -> #(advance(lexer, source), token.SigilAngle, ">")

        "/" <> source -> #(advance(lexer, source), token.SigilSlash, "/")
        "|" <> source -> #(advance(lexer, source), token.SigilPipe, "|")
        "'" <> source -> #(advance(lexer, source), token.SigilSingleQuote, "'")
        "\"" <> source -> #(
          advance(lexer, source),
          token.SigilDoubleQuote,
          "\"",
        )
        "`" <> source -> #(advance(lexer, source), token.SigilBacktick, "`")
        "#" <> source -> #(advance(lexer, source), token.SigilHash, "#")

        _ -> #(error(lexer, ExpectedSigilDelimiter), token.SigilNone, "")
      }

      case delimiter {
        token.SigilNone -> #(
          lexer,
          token.UnterminatedSigil(sigil:, delimiter:, contents: ""),
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
  delimiter: token.SigilDelimiter,
  closing_char: String,
  splitter: Splitter,
  contents: String,
) -> #(Lexer, Token) {
  let #(before, split, after) = splitter.split(splitter, lexer.source)
  case split {
    "" -> #(
      error(advance(lexer, after), UnterminatedString),
      token.UnterminatedSigil(sigil:, delimiter:, contents: contents <> before),
    )

    "\\" ->
      case string.pop_grapheme(after) {
        Error(_) -> #(
          error(advance(lexer, after), UnterminatedString),
          token.UnterminatedSigil(
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
      token.Sigil(sigil:, delimiter:, contents: contents <> before),
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
      error(advance(lexer, after), UnterminatedString),
      token.UnterminatedString(contents <> before),
    )

    "\\" -> {
      let #(lexer, escape) = lex_escape_sequence(advance(lexer, after))
      lex_string(lexer, contents <> before <> "\\" <> escape)
    }

    _ -> #(advance(lexer, after), token.String(contents <> before))
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
        token.InvalidTripleQuotedString(contents),
      )
    }
    Ok(lines) -> #(
      lexer,
      token.TripleQuotedString(
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

    _ -> #(error(lexer, UnterminatedString), [before, ..lines], "")
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
      error(advance(lexer, after), UnterminatedAtom),
      token.UnterminatedAtom(contents <> before),
    )

    "\\" ->
      case string.pop_grapheme(after) {
        Error(_) -> #(
          error(advance(lexer, after), UnterminatedString),
          token.UnterminatedString(contents),
        )
        Ok(#(character, source)) ->
          lex_string(
            advance(lexer, source),
            contents <> before <> "\\" <> character,
          )
      }

    _ -> #(advance(lexer, after), token.Atom(contents <> before, True))
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
  #(lexer, token.Variable(name))
}

fn lex_atom(lexer: Lexer, char: String) -> #(Lexer, Token) {
  let #(lexer, name) = lex_variable_or_atom(lexer, char)

  let token = case name {
    "after" -> token.After
    "begin" -> token.Begin
    "case" -> token.Case
    "catch" -> token.Catch
    "cond" -> token.Cond
    "else" -> token.Else
    "end" -> token.End
    "fun" -> token.Fun
    "if" -> token.If
    "let" -> token.Let
    "maybe" -> token.Maybe
    "of" -> token.Of
    "receive" -> token.Receive
    "try" -> token.Try
    "when" -> token.When
    "bnot" -> token.Bnot
    "div" -> token.Div
    "rem" -> token.Rem
    "band" -> token.Band
    "bor" -> token.Bor
    "bxor" -> token.Bxor
    "bsl" -> token.Bsl
    "bsr" -> token.Bsr
    "not" -> token.Not
    "and" -> token.And
    "or" -> token.Or
    "xor" -> token.Xor
    "andalso" -> token.Andalso
    "orelse" -> token.Orelse

    _ -> token.Atom(name, False)
  }
  #(lexer, token)
}

fn lex_until_end_of_line(lexer: Lexer) -> #(Lexer, String) {
  let #(before, split, after) =
    splitter.split(lexer.splitters.until_end_of_line, lexer.source)
  #(advance(lexer, split <> after), before)
}

fn lex_whitespace(lexer: Lexer, lexed: String) -> #(Lexer, Token) {
  case lexer.source {
    " " as space <> source
    | "\n" as space <> source
    | "\r" as space <> source
    | "\t" as space <> source
    | "\f" as space <> source ->
      lex_whitespace(advance(lexer, source), lexed <> space)
    _ -> maybe_token(lexer, token.Whitespace(lexed), !lexer.ignore_whitespace)
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

import gleam/list
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
  )
}

pub type Error {
  UnknownCharacter(character: String)
  UnterminatedString
  UnterminatedAtom
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
    until_end_of_line: splitter.new(["\n", "\r"]),
    string: splitter.new(["\"", "\\"]),
    quoted_atom: splitter.new(["'", "\\"]),
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

    "\"" <> source -> lex_string(advance(lexer, source), "")
    "'" <> source -> lex_quoted_atom(advance(lexer, source), "")

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

fn lex_string(lexer: Lexer, contents: String) -> #(Lexer, Token) {
  let #(before, split, after) =
    splitter.split(lexer.splitters.string, lexer.source)
  case split {
    "" -> #(
      error(advance(lexer, after), UnterminatedString),
      token.UnterminatedString(contents <> before),
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

    _ -> #(advance(lexer, after), token.String(contents <> before))
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

fn lex_atom(lexer: Lexer, lexed: String) -> #(Lexer, Token) {
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
    | "@" as char <> source -> lex_atom(advance(lexer, source), lexed <> char)

    _ -> {
      let token = case lexed {
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

        _ -> token.Atom(lexed, False)
      }
      #(lexer, token)
    }
  }
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

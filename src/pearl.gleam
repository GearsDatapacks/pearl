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
  Splitters(until_end_of_line: Splitter)
}

pub type Error {
  UnknownCharacter(character: String)
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
  Splitters(until_end_of_line: splitter.new(["\n", "\r"]))
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

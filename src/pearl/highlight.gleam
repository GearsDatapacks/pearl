import gleam/list
import gleam/option
import gleam/string
import gleam_community/ansi
import houdini
import pearl
import pearl/token as t

/// A highlighting token, containing information about the kind of syntax
/// being used. Many similar tokens (e.g. all keywords) are grouped together 
/// to simplify them.
/// 
/// For syntax tokens, see `pearl/token.{type Token}`.
/// 
pub type Token {
  Whitespace(String)
  Keyword(String)
  Variable(String)
  String(String)
  Atom(String)
  Number(String)
  Module(String)
  Function(String)
  Operator(String)
  Comment(String)
  Punctuation(String)
  Other(String)
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
pub fn ansi(code: String) -> String {
  tokens(code)
  |> list.fold("", fn(code, token) {
    code
    <> case token {
      Whitespace(s) -> ansi.reset(s)
      Keyword(s) -> ansi.yellow(s)
      Variable(s) -> ansi.reset(s)
      String(s) -> ansi.green(s)
      Atom(s) -> ansi.green(s)
      Number(s) -> ansi.green(s)
      Module(s) -> ansi.cyan(s)
      Function(s) -> ansi.blue(s)
      Operator(s) -> ansi.magenta(s)
      Comment(s) -> ansi.italic(ansi.gray(s))
      Punctuation(s) -> ansi.reset(s)
      Other(s) -> ansi.reset(s)
    }
  })
}

/// Convert a string of Erlang source code into an HTML string.
/// Each token is wrapped in a `<span>` with a class indicating the type of token.
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
/// pre code .hl-regexp   { color: #c8ffa7 }
/// pre code .hl-class    { color: #ffddfa }
/// ```
///
/// If you wish to use another format see `to_ansi` or `to_tokens`.
///
pub fn html(code: String) -> String {
  tokens(code)
  |> list.fold("", fn(acc, token) {
    case token {
      Whitespace(s) -> acc <> s
      Keyword(s) ->
        acc <> "<span class=hl-keyword>" <> houdini.escape(s) <> "</span>"
      Variable(s) ->
        acc <> "<span class=hl-variable>" <> houdini.escape(s) <> "</span>"
      String(s) ->
        acc <> "<span class=hl-string>" <> houdini.escape(s) <> "</span>"
      Atom(s) -> acc <> "<span class=hl-atom>" <> houdini.escape(s) <> "</span>"
      Number(s) ->
        acc <> "<span class=hl-number>" <> houdini.escape(s) <> "</span>"
      Module(s) ->
        acc <> "<span class=hl-module>" <> houdini.escape(s) <> "</span>"
      Function(s) ->
        acc <> "<span class=hl-function>" <> houdini.escape(s) <> "</span>"
      Operator(s) ->
        acc <> "<span class=hl-operator>" <> houdini.escape(s) <> "</span>"
      Comment(s) ->
        acc <> "<span class=hl-comment>" <> houdini.escape(s) <> "</span>"
      Punctuation(s) ->
        acc <> "<span class=hl-punctuation>" <> houdini.escape(s) <> "</span>"
      Other(s) -> acc <> s
    }
  })
}

/// Convert a string of Erlang source code into highlighting tokens.
/// Highlighting tokens only contain information about the kind of syntax
/// being used, grouping similar tokens (e.g. all keywords) into one category.
/// 
/// To convert code into syntax tokens, see `pearl.tokenise`.
/// 
pub fn tokens(code: String) -> List(Token) {
  let lexer = pearl.new(code)
  let #(tokens, _errors) = pearl.tokenise(lexer)
  do_to_tokens(tokens, [])
}

fn do_to_tokens(in: List(t.Token), out: List(Token)) -> List(Token) {
  case in {
    [] -> list.reverse(out)

    // Specific constructs
    [t.Atom(value, quoted: False), t.LeftParen, ..in] ->
      do_to_tokens(in, [Punctuation("("), Function(value), ..out])
    [t.Atom(function, quoted: False), t.Slash, t.Integer(arity), ..in] ->
      do_to_tokens(in, [
        Number(arity),
        Punctuation("/"),
        Function(function),
        ..out
      ])
    [
      t.Atom(module, quoted: False),
      t.Colon,
      t.Atom(function, quoted: False),
      t.Slash,
      t.Integer(arity),
      ..in
    ] ->
      do_to_tokens(in, [
        Number(arity),
        Punctuation("/"),
        Function(function),
        Punctuation(":"),
        Module(module),
        ..out
      ])
    [
      t.Atom(module, quoted: False),
      t.Colon,
      t.Atom(function, quoted: False),
      ..in
    ] ->
      do_to_tokens(in, [
        Function(function),
        Punctuation(":"),
        Module(module),
        ..out
      ])
    [t.Question, t.Variable(macro_name), ..in] ->
      do_to_tokens(in, [Function(macro_name), Punctuation("?"), ..out])

    // Whitespace and comments
    [t.Whitespace(space), ..in] -> do_to_tokens(in, [Whitespace(space), ..out])
    [t.Comment(contents), ..in] ->
      do_to_tokens(in, [Comment("%" <> contents), ..out])
    [t.DocComment(contents), ..in] ->
      do_to_tokens(in, [Comment("%%" <> contents), ..out])
    [t.ModuleComment(contents), ..in] ->
      do_to_tokens(in, [Comment("%%%" <> contents), ..out])
    [t.EndOfFile, ..in] -> do_to_tokens(in, out)

    // Literals
    [t.Character(char), ..in] -> do_to_tokens(in, [String("$" <> char), ..out])
    [t.Integer(int), ..in] -> do_to_tokens(in, [Number(int), ..out])
    [t.Float(float), ..in] -> do_to_tokens(in, [Number(float), ..out])
    [t.Atom(name:, quoted: True), ..in] ->
      do_to_tokens(in, [Atom("'" <> name <> "'"), ..out])
    [t.Atom(name:, quoted: False), ..in] ->
      do_to_tokens(in, [Atom(name), ..out])
    [t.String(contents), ..in] ->
      do_to_tokens(in, [String("\"" <> contents <> "\""), ..out])
    [
      t.TripleQuotedString(
        sigil:,
        number_of_quotes:,
        beginning_whitespace:,
        lines:,
        end_indentation:,
      ),
      ..in
    ] ->
      do_to_tokens(in, [
        String(
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
    [t.Sigil(sigil:, delimiter:, contents:), ..in] ->
      do_to_tokens(in, [
        String({
          let #(opening, closing) = t.sigil_delimiters(delimiter)
          "~" <> sigil <> opening <> contents <> closing
        }),
        ..out
      ])
    [t.Variable(name), ..in] -> do_to_tokens(in, [Variable(name), ..out])

    // Keywords
    [t.After, ..in] -> do_to_tokens(in, [Keyword("after"), ..out])
    [t.Begin, ..in] -> do_to_tokens(in, [Keyword("begin"), ..out])
    [t.Case, ..in] -> do_to_tokens(in, [Keyword("case"), ..out])
    [t.Catch, ..in] -> do_to_tokens(in, [Keyword("catch"), ..out])
    [t.Cond, ..in] -> do_to_tokens(in, [Keyword("cond"), ..out])
    [t.Else, ..in] -> do_to_tokens(in, [Keyword("else"), ..out])
    [t.End, ..in] -> do_to_tokens(in, [Keyword("end"), ..out])
    [t.Fun, ..in] -> do_to_tokens(in, [Keyword("fun"), ..out])
    [t.If, ..in] -> do_to_tokens(in, [Keyword("if"), ..out])
    [t.Let, ..in] -> do_to_tokens(in, [Keyword("let"), ..out])
    [t.Maybe, ..in] -> do_to_tokens(in, [Keyword("maybe"), ..out])
    [t.Of, ..in] -> do_to_tokens(in, [Keyword("of"), ..out])
    [t.Receive, ..in] -> do_to_tokens(in, [Keyword("receive"), ..out])
    [t.Try, ..in] -> do_to_tokens(in, [Keyword("try"), ..out])
    [t.When, ..in] -> do_to_tokens(in, [Keyword("when"), ..out])

    // Punctuation
    [t.LeftParen, ..in] -> do_to_tokens(in, [Punctuation("("), ..out])
    [t.RightParen, ..in] -> do_to_tokens(in, [Punctuation(")"), ..out])
    [t.LeftBrace, ..in] -> do_to_tokens(in, [Punctuation("{"), ..out])
    [t.RightBrace, ..in] -> do_to_tokens(in, [Punctuation("}"), ..out])
    [t.LeftSquare, ..in] -> do_to_tokens(in, [Punctuation("["), ..out])
    [t.RightSquare, ..in] -> do_to_tokens(in, [Punctuation("]"), ..out])
    [t.Comma, ..in] -> do_to_tokens(in, [Punctuation(","), ..out])
    [t.Semicolon, ..in] -> do_to_tokens(in, [Punctuation(";"), ..out])
    [t.Colon, ..in] -> do_to_tokens(in, [Punctuation(":"), ..out])
    [t.Dot, ..in] -> do_to_tokens(in, [Punctuation("."), ..out])
    [t.MinusGreater, ..in] -> do_to_tokens(in, [Punctuation("->"), ..out])
    [t.DoubleLess, ..in] -> do_to_tokens(in, [Punctuation("<<"), ..out])
    [t.DoubleGreater, ..in] -> do_to_tokens(in, [Punctuation(">>"), ..out])
    [t.Hash, ..in] -> do_to_tokens(in, [Punctuation("#"), ..out])
    [t.DoubleColon, ..in] -> do_to_tokens(in, [Punctuation("::"), ..out])
    [t.DoubleDot, ..in] -> do_to_tokens(in, [Punctuation(".."), ..out])
    [t.TripleDot, ..in] -> do_to_tokens(in, [Punctuation("..."), ..out])
    [t.Question, ..in] -> do_to_tokens(in, [Punctuation("?"), ..out])

    // Operators
    [t.DoublePipe, ..in] -> do_to_tokens(in, [Operator("||"), ..out])
    [t.EqualGreater, ..in] -> do_to_tokens(in, [Operator("=>"), ..out])
    [t.ColonEqual, ..in] -> do_to_tokens(in, [Operator(":="), ..out])
    [t.LessMinus, ..in] -> do_to_tokens(in, [Operator("<-"), ..out])
    [t.LessEqual, ..in] -> do_to_tokens(in, [Operator("<="), ..out])
    [t.Pipe, ..in] -> do_to_tokens(in, [Operator("|"), ..out])
    [t.DoubleEqual, ..in] -> do_to_tokens(in, [Operator("=="), ..out])
    [t.SlashEqual, ..in] -> do_to_tokens(in, [Operator("/="), ..out])
    [t.EqualLess, ..in] -> do_to_tokens(in, [Operator("=<"), ..out])
    [t.Less, ..in] -> do_to_tokens(in, [Operator("<"), ..out])
    [t.GreaterEqual, ..in] -> do_to_tokens(in, [Operator(">="), ..out])
    [t.Greater, ..in] -> do_to_tokens(in, [Operator(">"), ..out])
    [t.EqualColonEqual, ..in] -> do_to_tokens(in, [Operator("=:="), ..out])
    [t.EqualSlashEqual, ..in] -> do_to_tokens(in, [Operator("=/="), ..out])
    [t.Plus, ..in] -> do_to_tokens(in, [Operator("+"), ..out])
    [t.Minus, ..in] -> do_to_tokens(in, [Operator("-"), ..out])
    [t.Star, ..in] -> do_to_tokens(in, [Operator("*"), ..out])
    [t.Slash, ..in] -> do_to_tokens(in, [Operator("/"), ..out])
    [t.Bnot, ..in] -> do_to_tokens(in, [Operator("bnot"), ..out])
    [t.Div, ..in] -> do_to_tokens(in, [Operator("div"), ..out])
    [t.Rem, ..in] -> do_to_tokens(in, [Operator("rem"), ..out])
    [t.Band, ..in] -> do_to_tokens(in, [Operator("band"), ..out])
    [t.Bor, ..in] -> do_to_tokens(in, [Operator("bor"), ..out])
    [t.Bxor, ..in] -> do_to_tokens(in, [Operator("bxor"), ..out])
    [t.Bsl, ..in] -> do_to_tokens(in, [Operator("bsl"), ..out])
    [t.Bsr, ..in] -> do_to_tokens(in, [Operator("bsr"), ..out])
    [t.Not, ..in] -> do_to_tokens(in, [Operator("not"), ..out])
    [t.And, ..in] -> do_to_tokens(in, [Operator("and"), ..out])
    [t.Or, ..in] -> do_to_tokens(in, [Operator("or"), ..out])
    [t.Xor, ..in] -> do_to_tokens(in, [Operator("xor"), ..out])
    [t.Andalso, ..in] -> do_to_tokens(in, [Operator("andalso"), ..out])
    [t.Orelse, ..in] -> do_to_tokens(in, [Operator("orelse"), ..out])
    [t.DoublePlus, ..in] -> do_to_tokens(in, [Operator("++"), ..out])
    [t.DoubleMinus, ..in] -> do_to_tokens(in, [Operator("--"), ..out])
    [t.QuestionEqual, ..in] -> do_to_tokens(in, [Operator("?="), ..out])
    [t.Bang, ..in] -> do_to_tokens(in, [Operator("!"), ..out])
    [t.Equal, ..in] -> do_to_tokens(in, [Operator("="), ..out])

    // Invalid tokens
    [t.Unknown(char), ..in] -> do_to_tokens(in, [Other(char), ..out])
    [t.UnterminatedString(contents), ..in] ->
      do_to_tokens(in, [String("\"" <> contents), ..out])
    [t.UnterminatedSigil(sigil:, contents:, delimiter:), ..in] ->
      do_to_tokens(in, [
        String({
          let #(opening, _closing) = t.sigil_delimiters(delimiter)
          "~" <> sigil <> opening <> contents
        }),
        ..out
      ])
    [t.UnterminatedAtom(contents), ..in] ->
      do_to_tokens(in, [Atom("'" <> contents), ..out])
    [t.InvalidTripleQuotedString(contents), ..in] ->
      do_to_tokens(in, [String("\"\"\"" <> contents <> "\"\"\""), ..out])
  }
}

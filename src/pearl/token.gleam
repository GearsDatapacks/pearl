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
  TripleQuotedString(contents: String, end_indentation: String)
  Sigil(sigil: String, contents: String)
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
  Bang
  Equal

  // Invalid tokens
  Unknown(String)
  UnterminatedString(String)
  UnterminatedAtom(String)
}

pub fn to_source(token: Token) -> String {
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
    TripleQuotedString(contents:, end_indentation:) ->
      "\"\"\"\n" <> contents <> "\n" <> end_indentation <> "\"\"\""
    Sigil(sigil:, contents:) -> "~" <> sigil <> "\"" <> contents <> "\""
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
    Bang -> "!"
    Equal -> "="

    // Invalid tokens
    Unknown(char) -> char
    UnterminatedString(contents) -> "\"" <> contents
    UnterminatedAtom(contents) -> "'" <> contents
  }
}

import birdie
import gleam/list
import gleam/option.{None, Some}
import gleeunit
import pearl
import simplifile

pub fn main() {
  gleeunit.main()
}

fn assert_roundtrip(src: String, allow_errors: Bool) -> Nil {
  let #(tokens, errors) = src |> pearl.new |> pearl.tokenise
  case allow_errors {
    True -> Nil
    False -> {
      assert errors == []
    }
  }

  assert pearl.to_source(tokens) == src
}

fn assert_tokens(src: String, tokens: List(pearl.Token)) -> Nil {
  let #(lexed, errors) =
    src |> pearl.new |> pearl.ignore_whitespace |> pearl.tokenise
  assert errors == []
  let tokens = list.append(tokens, [pearl.EndOfFile])
  assert lexed == tokens
}

fn assert_errors(src: String, errors: List(pearl.Error)) -> Nil {
  let #(_, found_errors) = src |> pearl.new |> pearl.tokenise
  assert found_errors == errors
}

pub fn stdlib_ffi_roundtrip_test() {
  let assert Ok(stdlib_ffi_file) =
    simplifile.read("build/packages/gleam_stdlib/src/gleam_stdlib.erl")

  assert_roundtrip(stdlib_ffi_file, False)
}

pub fn tokens_roundtrip_test() {
  let assert Ok(tokens_file) = simplifile.read("test/tokens.txt")
  assert_roundtrip(tokens_file, False)
}

pub fn numbers_test() {
  let src =
    "2.3E10 5.4e-3 16#ABCDEF 16#abcdef123 8#1234 2#101101 0123 0988 1_2_3_4_5
1_2.3_4e-5_6 14#123_456_aBC"
  assert_tokens(src, [
    pearl.Float("2.3E10"),
    pearl.Float("5.4e-3"),
    pearl.Integer("16#ABCDEF"),
    pearl.Integer("16#abcdef123"),
    pearl.Integer("8#1234"),
    pearl.Integer("2#101101"),
    pearl.Integer("0123"),
    pearl.Integer("0988"),
    pearl.Integer("1_2_3_4_5"),
    pearl.Float("1_2.3_4e-5_6"),
    pearl.Integer("14#123_456_aBC"),
  ])
}

pub fn atom_variable_test() {
  let src = "this_is_an_atom This_is_a_variable 'This is an atom'"
  assert_tokens(src, [
    pearl.Atom("this_is_an_atom", False),
    pearl.Variable("This_is_a_variable"),
    pearl.Atom("This is an atom", True),
  ])
}

pub fn sigil_test() {
  let src =
    "~B/This is a raw sigil\\/ ~<Bracketed, with escapes <\\>>
~s|Sigil which allows \"quotes\"|"
  assert_tokens(src, [
    pearl.Sigil("B", pearl.SigilSlash, "This is a raw sigil\\"),
    pearl.Sigil("", pearl.SigilAngle, "Bracketed, with escapes <\\>"),
    pearl.Sigil("s", pearl.SigilPipe, "Sigil which allows \"quotes\""),
  ])
}

pub fn character_literal_test() {
  let src = "$\" $\\^_ $\\xAb $\\x{abcDe1} $\\41"
  assert_tokens(src, [
    pearl.Character("\""),
    pearl.Character("\\^_"),
    pearl.Character("\\xAb"),
    pearl.Character("\\x{abcDe1}"),
    pearl.Character("\\41"),
  ])
}

pub fn triple_quoted_string_test() {
  let src =
    "
\"\"\"  
\t Hello, this is triple-quoted!
\t  This line starts with a space
\t Quotes are allowed: \"\", even three: \"\"\"
\t \"\"\"
"

  assert_tokens(src, [
    pearl.TripleQuotedString(
      sigil: None,
      number_of_quotes: 3,
      beginning_whitespace: "  \n",
      lines: [
        "Hello, this is triple-quoted!",
        " This line starts with a space",
        "Quotes are allowed: \"\", even three: \"\"\"",
      ],
      end_indentation: "\t ",
    ),
  ])
}

pub fn triple_quoted_string_sigil_test() {
  let src =
    "
~b\"\"\"
  Hello
  This is a triple-quoted sigil
  \"\"\"
"

  assert_tokens(src, [
    pearl.TripleQuotedString(
      sigil: Some("b"),
      number_of_quotes: 3,
      beginning_whitespace: "\n",
      lines: ["Hello", "This is a triple-quoted sigil"],
      end_indentation: "  ",
    ),
  ])
}

pub fn triple_quoted_string_more_quotes_test() {
  let src =
    "
\"\"\"\"\"
   This string has five quotes
   so four are allowed:
   \"\"\"\"
   \"\"\"\"\"
"

  assert_tokens(src, [
    pearl.TripleQuotedString(
      sigil: None,
      number_of_quotes: 5,
      beginning_whitespace: "\n",
      lines: ["This string has five quotes", "so four are allowed:", "\"\"\"\""],
      end_indentation: "   ",
    ),
  ])
}

pub fn unknown_character_test() {
  let src = "a&b"
  assert_errors(src, [pearl.UnknownCharacter("&")])
}

pub fn unterminated_string_eof_test() {
  let src = "\"Some string that doesn't end"
  assert_errors(src, [pearl.UnterminatedStringLiteral])
}

pub fn unterminated_string_newline_test() {
  let src =
    "\"Some string that tries
to continue on the next line"
  assert_errors(src, [pearl.UnterminatedStringLiteral])
}

pub fn unterminated_atom_test() {
  let src = "'This is an atom!"
  assert_errors(src, [pearl.UnterminatedQuotedAtom])
}

pub fn invalid_radix_test() {
  let src = "94#123"
  assert_errors(src, [pearl.InvalidRadix("94")])
}

pub fn invalid_radix_low_test() {
  let src = "1#123"
  assert_errors(src, [pearl.InvalidRadix("1")])
}

pub fn number_cannot_end_after_radix_test() {
  let src = "16#"
  assert_errors(src, [pearl.NumberCannotEndAfterRadix])
}

pub fn unterminated_character_test() {
  let src = "$"
  assert_errors(src, [pearl.UnterminatedCharacter])
}

pub fn expected_sigil_delimiter_test() {
  let src = "~abc"
  assert_errors(src, [pearl.ExpectedSigilDelimiter])
}

pub fn unterminated_escape_sequence_test() {
  let src = "\"\\x\""
  assert_errors(src, [pearl.UnterminatedEscapeSequence])
}

pub fn unterminated_escape_sequence2_test() {
  let src = "\"\\x1\""
  assert_errors(src, [pearl.UnterminatedEscapeSequence])
}

pub fn unterminated_escape_sequence3_test() {
  let src = "\"\\x{123\""
  assert_errors(src, [
    pearl.UnterminatedEscapeSequence,
    pearl.UnterminatedStringLiteral,
  ])
}

pub fn unterminated_escape_sequence4_test() {
  let src = "\"\\"
  assert_errors(src, [
    pearl.UnterminatedEscapeSequence,
    pearl.UnterminatedStringLiteral,
  ])
}

pub fn double_numeric_separator_test() {
  let src = "1__2"
  assert_errors(src, [pearl.NumericSeparatorNotAllowed])
}

pub fn trailing_numeric_separator_test() {
  let src = "1_2_"
  assert_errors(src, [pearl.NumericSeparatorNotAllowed])
}

pub fn trailing_decimal_numeric_separator_test() {
  let src = "1_2_.3"
  assert_errors(src, [pearl.NumericSeparatorNotAllowed])
}

pub fn leading_decimal_numeric_separator_test() {
  let src = "1_2._3"
  assert_errors(src, [pearl.NumericSeparatorNotAllowed])
}

pub fn trailing_exponent_numeric_separator_test() {
  let src = "1_2.3_e4"
  assert_errors(src, [pearl.NumericSeparatorNotAllowed])
}

pub fn leading_exponent_numeric_separator_test() {
  let src = "1_2.3e_4"
  assert_errors(src, [pearl.NumericSeparatorNotAllowed])
}

pub fn leading_negative_exponent_numeric_separator_test() {
  let src = "1_2.3e-_4"
  assert_errors(src, [pearl.NumericSeparatorNotAllowed])
}

pub fn missing_exponent_test() {
  let src = "1.2e"
  assert_errors(src, [pearl.ExpectedExponent])
}

pub fn missing_negative_exponent_test() {
  let src = "1.2e-"
  assert_errors(src, [pearl.ExpectedExponent])
}

pub fn missing_whitespace_after_triple_quote() {
  let src =
    "\"\"\"Hello
\"\"\""
  assert_errors(src, [pearl.ExpectedWhitespaceAfterTripleQuote])
}

pub fn invalid_triple_quoted_string_indentation() {
  let src =
    "\"\"\"
 Hello
    world
   \"\"\""
  assert_errors(src, [
    pearl.InvalidTripleQuotedStringIndentation(
      expected_indentation: "   ",
      line: " Hello",
    ),
  ])
}

fn assert_ansi_highlight(title: String, src: String) -> Nil {
  birdie.snap(
    src <> "\n\n---\n\n" <> pearl.highlight_ansi(src),
    "highlight_" <> title <> "_ansi",
  )
}

fn assert_html_highlight(title: String, src: String) -> Nil {
  birdie.snap(
    src <> "\n\n---\n\n" <> pearl.highlight_html(src),
    "highlight_" <> title <> "_html",
  )
}

pub fn basic_program_test() {
  assert_ansi_highlight(
    "basic_program",
    "
-module(hello_world).

-export([main/0]).

main() ->
  Message = \"Hello, world!\",
  print(Message),
  nil.
",
  )
}

pub fn basic_program_html_test() {
  assert_html_highlight(
    "basic_program",
    "
-module(hello_world).

-export([main/0]).

main() ->
  Message = \"Hello, world!\",
  print(Message),
  nil.
",
  )
}

pub fn literals_test() {
  assert_ansi_highlight(
    "literals",
    "
$\\^?
\"Hello, world!\"
atom
'Complex atom!'
~/I'm a sigil/
\"\"\"\"
  Triple quoted
  string with four quotes
  \"\"\"\"
Some_variable
16#C0de
123_456
12_3.6e-20
",
  )
}

pub fn literals_html_test() {
  assert_html_highlight(
    "literals",
    "
$\\^?
\"Hello, world!\"
atom
'Complex atom!'
~/I'm a sigil/
\"\"\"\"
  Triple quoted
  string with four quotes
  \"\"\"\"
Some_variable
16#C0de
123_456
12_3.6e-20
",
  )
}

pub fn comments_test() {
  assert_ansi_highlight(
    "comments",
    "
%%% This a comment for the entire module

%% This is a doc comment for the main function
main() ->
  % A comment within a function
  nil. % This one is after the line
",
  )
}

pub fn functions_test() {
  assert_ansi_highlight(
    "functions",
    "
some_function/2
module:function/1
call_me_a_module:function()
just_a_function()
?MACRO(10)
",
  )
}

pub fn errors_test() {
  assert_ansi_highlight(
    "errors",
    "
Unknown = &,
Sigil = ~b/This sigil never
terminates.
",
  )
}

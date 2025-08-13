import gleam/list
import gleeunit
import pearl
import pearl/token
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

fn assert_tokens(src: String, tokens: List(token.Token)) -> Nil {
  let #(lexed, errors) =
    src |> pearl.new |> pearl.ignore_whitespace |> pearl.tokenise
  assert errors == []
  let tokens = list.append(tokens, [token.EndOfFile])
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
    token.Float("2.3E10"),
    token.Float("5.4e-3"),
    token.Integer("16#ABCDEF"),
    token.Integer("16#abcdef123"),
    token.Integer("8#1234"),
    token.Integer("2#101101"),
    token.Integer("0123"),
    token.Integer("0988"),
    token.Integer("1_2_3_4_5"),
    token.Float("1_2.3_4e-5_6"),
    token.Integer("14#123_456_aBC"),
  ])
}

pub fn atom_variable_test() {
  let src = "this_is_an_atom This_is_a_variable 'This is an atom'"
  assert_tokens(src, [
    token.Atom("this_is_an_atom", False),
    token.Variable("This_is_a_variable"),
    token.Atom("This is an atom", True),
  ])
}

pub fn sigil_test() {
  let src =
    "~B/This is a raw sigil\\/ ~<Bracketed, with escapes <\\>>
~s|Sigil which allows \"quotes\"|"
  assert_tokens(src, [
    token.Sigil("B", token.SigilSlash, "This is a raw sigil\\"),
    token.Sigil("", token.SigilAngle, "Bracketed, with escapes <\\>"),
    token.Sigil("s", token.SigilPipe, "Sigil which allows \"quotes\""),
  ])
}

pub fn character_literal_test() {
  let src = "$\" $\\^_ $\\xAb $\\x{abcDe1} $\\41"
  assert_tokens(src, [
    token.Character("\""),
    token.Character("\\^_"),
    token.Character("\\xAb"),
    token.Character("\\x{abcDe1}"),
    token.Character("\\41"),
  ])
}

pub fn unknown_character_test() {
  let src = "a&b"
  assert_errors(src, [pearl.UnknownCharacter("&")])
}

pub fn unterminated_string_eof_test() {
  let src = "\"Some string that doesn't end"
  assert_errors(src, [pearl.UnterminatedString])
}

pub fn unterminated_string_newline_test() {
  let src =
    "\"Some string that tries
to continue on the next line"
  assert_errors(src, [pearl.UnterminatedString])
}

pub fn unterminated_atom_test() {
  let src = "'This is an atom!"
  assert_errors(src, [pearl.UnterminatedAtom])
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
    pearl.UnterminatedString,
  ])
}

pub fn unterminated_escape_sequence4_test() {
  let src = "\"\\"
  assert_errors(src, [
    pearl.UnterminatedEscapeSequence,
    pearl.UnterminatedString,
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

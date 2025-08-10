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

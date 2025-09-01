# Changelog

## v2.1.0 - 2025-09-01

- Improved the performance of lexing comments and escape sequences.

## v2.0.0 - 2025-08-22

- Merged the `pearl/token` and `pearl/highlight` modules into one `pearl` module.

- Renamed several variants and functions to prevent name clashes.
  - `pearl/token.to_source` -> `pearl.token_to_source`
  - `pearl.UnterminatedAtom` -> `pearl.UnterminatedQuotedAtom`
  - `pearl.UnterminatedString` -> `pearl.UnterminatedStringLiteral`
  - `pearl/highlight.tokens` -> `pearl.highlight_tokens`
  - `pearl/highlight.ansi` -> `pearl.highlight_ansi`
  - `pearl/highlight.html` -> `pearl.highlight_html`
  - `pearl/highlight.{type Token}` -> `pearl.HighlightToken`

- Added the `stringify_error` function.

## v1.0.0 - 2025-08-21

- Initial release.

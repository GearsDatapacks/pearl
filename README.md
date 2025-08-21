# Pearl

An Erlang lexer and syntax highlighter for Gleam!

[![Package Version](https://img.shields.io/hexpm/v/pearl)](https://hex.pm/packages/pearl)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/pearl/)

Pearl is a lexer and syntax highlighter for Erlang, written in Gleam.  
The `pearl` module, based on [`glexer`](https://hexdocs.mp/glexer) and
[`just`](https://hexdocs.mp/just), exposes a
standard lexer API, allowing you to convert Erlang source code into tokens.  
The `pearl/highlight` module allows you to highlight Erlang code using ansi
colours, html or a custom format. Heavily inspired by [`contour`](https://hexdocs.pm/contour).

```sh
gleam add pearl@1
```

```gleam
import pearl
import pearl/highlight

pub fn main() {
  let code = "console.log('Hello, world!');"

  let lexer = pearl.new(code)
  // Lex syntax tokens for parsing or other uses
  let #(tokens, errors) = pearl.tokenise(lexer)
  let assert [] = errors
  parse_erlang(tokens)

  // Highlight with ansi codes to print in the terminal
  let highlighted = highlight.ansi(code)
  io.println(highlighted)

  // Render to html to show in the browser
  let html = highlight.html(code)
  io.println("<pre><code>" <> html <> "</code></pre>")

  // Convert to "highlighting tokens" to highlight in some other way
  let highlight_tokens = highlight.tokens(code)
  highlight_tokens_some_other_way(highlight_tokens)
}
```

Further documentation can be found at <https://hexdocs.pm/pearl>.

### Feature completeness

As far as I can tell, `pearl` can lex all valid Erlang programs. However, I
couldn't find a technical specification for Erlang syntax so what I've implemented
is based on the Erlang documentation and other Erlang parsers. If there is
something missing, please open an issue and I'll implement it as soon as possible.

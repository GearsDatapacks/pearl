import birdie
import pearl/highlight

fn assert_ansi_highlight(title: String, src: String) -> Nil {
  birdie.snap(
    src <> "\n\n---\n\n" <> highlight.ansi(src),
    "highlight_" <> title <> "_ansi",
  )
}

fn assert_html_highlight(title: String, src: String) -> Nil {
  birdie.snap(
    src <> "\n\n---\n\n" <> highlight.html(src),
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

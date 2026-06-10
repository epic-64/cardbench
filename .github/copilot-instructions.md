# Instructions
This is Gigadev: a Scala 3 fullstack project using Cask (JVM server) and
ScalaJS + Laminar (client), as a cross-project with shared code.

Preferred code style:
- always prefer Scala 3 syntax over Scala 2 syntax
- never use _ as a wildcard import, always use *
- use braceless syntax unless there is a good reason not to
- instead of try/catch, we use Try()/match.
- in general, we like to use match, .pipe, .tap, .map and so on

Test style:
- we use ScalaTest for testing
- we prefer BDD style high-level tests that focus on behavior and outcomes rather than implementation details
- the tests should call one and only one high-level function and treat it as a black box
- tests should avoid shared state and invisible dependencies
- tests must not rely on if-statements and should avoid loops

Compiling:
- bloop compile jvm
- bloop compile js

Warnings:
- *Always* treat *all* warnings as errors. This is non-negotiable. If you see a warning, fix it immediately.
- It does not matter if the warnings are from the current session, or earlier. We have 100% control and
  responsibility over the *entire* codebase, and every warning must always be addressed.

Context:
- Never inspect generated code.
- Never read, open, grep, search, or semantically search these files:
  - `jvm/src/main/resources/static/dist/main.js`
  - `jvm/src/main/resources/static/dist/main.js.map`
  - `jvm/src/main/resources/static/dist/app-bundle.css`

running tests:
- to run jvm tests, use bloop: `bloop test jvm`
- to run js tests, use sbt: `sbt js/test` (unfortunately bloop cannot run client tests)
- to run all tests, with fullOptJs, use `scripts/run-all-tests.sh`

ui library:
- we use a custom CSS lib, defined in `jvm/src/main/resources/static/css/base.css`

Style considerations:
- never use a rounded border-left, it looks horrendous.
- Whenever you write or encounter repetitive CSS styles, please extract them into CSS variables.

File size:
- most files should stay under 500 lines
- if a file grows beyond 300 lines, consider refactoring it into multiple files

Important Laminar Concepts:
- when using signals that update frequently, always use `.distinct` on the signal to prevent unnecessary DOM updates
  - without `.distinct`, every signal emission triggers a re-render even if the value hasn't changed
  - example: `child.text <-- someSignal.map(s => s.someValue).distinct`
- combineWith automatically flattens tuples: Signal[A].combineWith[B].combineWith[C] => Signal[(A, B, C)]

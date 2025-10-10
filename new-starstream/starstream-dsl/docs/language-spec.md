# Language Specification

## Grammar

This document provides a complete EBNF grammar specification for the IMP
(Imperative) language used in the Starstream DSL project. The grammar is written
using W3C notation and follows literate programming principles (code embedded as
code blocks inside this markdown), where the documentation explains the language
structure alongside the formal grammar rules.

This document uses the following concepts for ensuring readability for both
humans and AI:

1. The grammar uses catalog rules (we do NOT cascade rules. Although this is
   easier to read for tools, it's harder to read for humans)
2. To handle ambiguity, we provide a separate, normative
   precedence/associativity table

This document assumes you took at least a university course on programming
languages, design and compilers. Therefore, we try and keep prose to a minimum
and prefer to bundle concepts in codeblocks where it makes sense.

## Grammar Rules

```ebnf
program ::= statement*

statement ::= variable_declaration
            | assignment
            | if_statement
            | while_statement
            | block
            | expression_statement

variable_declaration ::= "let" identifier "=" expression ";"

assignment ::= identifier "=" expression ";"

if_statement ::= "if" "(" expression ")" block [ "else" block ]

while_statement ::= "while" "(" expression ")" block

block ::= "{" statement* "}"

expression_statement ::= expression ";"

expression ::= binary_or_expression
            | binary_and_expression
            | equality_expression
            | comparison_expression
            | additive_expression
            | multiplicative_expression
            | unary_expression
            | primary_expression

binary_or_expression ::= expression "||" expression

binary_and_expression ::= expression "&&" expression

equality_expression ::= expression ( "==" | "!=" ) expression

comparison_expression ::= expression ( "<" | "<=" | ">" | ">=" ) expression

additive_expression ::= expression ( "+" | "-" ) expression

multiplicative_expression ::= expression ( "*" | "/" | "%" ) expression

unary_expression ::= "-" expression
                  | "!" expression

primary_expression ::= integer_literal
                    | boolean_literal
                    | identifier
                    | "(" expression ")"

integer_literal ::= [0-9]+

boolean_literal ::= "true" | "false"

identifier ::= [a-zA-Z_][a-zA-Z0-9_]*
```

## Precedence and Associativity

| Precedence  | Operator             | Associativity | Description     |
| ----------- | -------------------- | ------------- | --------------- |
| 1 (highest) | `!`, `-`             | Right         | Unary operators |
| 2           | `*`, `/`, `%`        | Left          | Multiplicative  |
| 3           | `+`, `-`             | Left          | Additive        |
| 4           | `<`, `<=`, `>`, `>=` | Left          | Relational      |
| 5           | `==`, `!=`           | Left          | Equality        |
| 6           | `&&`                 | Left          | Logical AND     |
| 7 (lowest)  | `                    |               | `               |

---

> everything below is not yet implemented

## Semantics

### Common concepts

Scopes:

- Every expression exists within a stack of scopes, in the traditional static scoping sense.
- Each scope has a table of variables, identified by name and having a current integer value.
- Syntactic blocks (curly braces) introduce new scopes.

### Environment

The Env of the semantics is defined by the following contexts:

- The instruction context: the program and the instructions left to process
  - A suspended UTXO must serialize itself; the VM expects to be able to simply run it from its entry point with its existing memory
- The local memory context: any variables local to the function (ex: "the stack")
  - objects can be removed from this context either by going out of scope, or by being used (linear types)
- The persistent memory context: any shared variables that are globally referable (ex: static variables, "the heap")
  - Including some notion of when a piece of persistent memory is "freed" and can be safely zeroed (immediately and deterministically)
- The type context: which types exist and their definitions
  - There are no pointer types to either functions or resources
  - Types have identities (hashes) and are structural (names are omitted when computing the ID)
- The resource context: which references exist to externally-managed resources (tokens)
  - UTXO external resources and token intermediates are passed around explicitly, not part of the context

### Type identities

- Algebraic Data Types (ADTs) are supported
  - Struct identities are based on their field types, in order
    - So `(i32, i32)` == `struct Foo { a: i32, b: i32 }` == `struct Bar { b: i32, c: i32 }`
  - Enum identities are based on their variant discriminators (ordinals), and the field types in order of each variant
    - So `i32 | (i32, i32)` == `enum Foo { A { b: i32, }, C { d: i32, e: i32 } }`

- Function identities are based on their name and their type.
  - Function types are based on their parameter types in order, return type, and possible effect set

### Evaluation

Expressions:

- Integer literals work in the obvious way.
- Boolean literals work in the obvious way.
- Variable names refer to a `let` declaration earlier in the current scope or
  one of its parents, but not child scopes.
- Arithmetic operators: `+`, `-`, `*`, `/`, `%` work over integers in the usual
  way.
  - We assume wrapping signed 32-bit two's complement integers.
  - `%` is the Euclid remainder (least non-negative).
- Unary `-` applies to integers. Unary `!` applies to booleans.
- Comparison operators: `==`, `!=`, `<`, `>`, `<=`, `>=` accept (integer, integer) or (boolean, boolean) and
  produce booleans.
- The boolean operators `!`, `&&`, `||` accept booleans and produce
  booleans.
  - `&&` and `||` are short-circuiting.

Statements:

- `if` statements evaluate their condition, require it to be a boolean, and branch in the obvious way.
- `while` expressions loop in the obvious way.
- Blocks introduce a new child scope for `let` statements.
- `let` statements add a new variable binding to the current scope and give it
  an initial value based on its expression.
  - Variables are integers.
- Assignment statements look up a variable in the stack of scopes and change its current value to the result of evaluating the right-hand side.

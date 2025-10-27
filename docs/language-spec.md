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

> [!NOTE]
> When updating this grammar, also update:
> - the [canonical parser](../starstream-compiler/src/parser/).
> - the [Tree-sitter grammar](../tree-sitter-starstream/grammar.js).
> - if changing comments or bracket/quote pairs, the [VSC language configuration](../vscode-starstream/language-configuration.json).

## Precedence and Associativity

| Precedence  | Operator             | Associativity | Description     |
| ----------- | -------------------- | ------------- | --------------- |
| 1 (highest) | `!`, `-`             | Right         | Unary operators |
| 2           | `*`, `/`, `%`        | Left          | Multiplicative  |
| 3           | `+`, `-`             | Left          | Additive        |
| 4           | `<`, `<=`, `>`, `>=` | Left          | Relational      |
| 5           | `==`, `!=`           | Left          | Equality        |
| 6           | `&&`                 | Left          | Logical AND     |
| 7 (lowest)  | `\|\|`               | Left          | Logical OR      |

## Scopes

- Every expression exists within a stack of scopes, in the traditional static scoping sense.
- Each scope has a table of variables, identified by name and having a static type and a current value.
- Syntactic blocks (curly braces) introduce new scopes.

## Expression semantics

- Integer literals work in the obvious way.
- Boolean literals work in the obvious way.
- Variable names refer to a `let` declaration earlier in the current scope or
  one of its parents, but not child scopes.
- Arithmetic operators: `+`, `-`, `*`, `/`, `%` work over integers in the usual
  way.
  - We assume wrapping signed 64-bit two's complement integers.
  - `/` and `%` are floored. `%` has the same sign as the divisor.
- Unary `-` applies to integers. Unary `!` applies to booleans.
- Comparison operators: `==`, `!=`, `<`, `>`, `<=`, `>=` accept (integer, integer) or (boolean, boolean) and
  produce booleans.
- The boolean operators `!`, `&&`, `||` accept booleans and produce
  booleans.
  - `&&` and `||` are short-circuiting.

| Syntax rule                | Type rule                                                                 | Value rule                |
| -------------------------- | ------------------------------------------------------------------------- | ------------------------- |
| integer_literal            | $\dfrac{}{Γ ⊢ integer\ literal : i64}$                                    | Integer literal           |
| boolean_literal            | $\dfrac{}{Γ ⊢ boolean\ literal : bool}$                                   | Boolean literal           |
| identifier                 | $\dfrac{ident : T ∈ Γ}{Γ ⊢ ident : T}$                                    | Refers to `let` in scope  |
| (expression)               | $\dfrac{Γ ⊢ e : T}{Γ ⊢ (e) : T}$                                          | Identity                  |
| !expression                | $\dfrac{Γ ⊢ e : bool}{Γ ⊢\ !e : bool}$                                    | Boolean inverse           |
| -expression                | $\dfrac{Γ ⊢ e : i64}{Γ ⊢ -e : i64}$                                       | Integer negation          |
| expression \* expression   | $\dfrac{Γ ⊢ lhs : i64 ∧ Γ ⊢ rhs : i64}{Γ ⊢ lhs * rhs : i64}$              | Integer multiplication    |
| expression / expression    | $\dfrac{Γ ⊢ lhs : i64 ∧ Γ ⊢ rhs : i64}{Γ ⊢ lhs / rhs : i64}$              | Integer floored division  |
| expression % expression    | $\dfrac{Γ ⊢ lhs : i64 ∧ Γ ⊢ rhs : i64}{Γ ⊢ lhs\ \%\ rhs : i64}$           | Integer floored remainder |
| expression + expression    | $\dfrac{Γ ⊢ lhs : i64 ∧ Γ ⊢ rhs : i64}{Γ ⊢ lhs + rhs : i64}$              | Integer addition          |
| expression - expression    | $\dfrac{Γ ⊢ lhs : i64 ∧ Γ ⊢ rhs : i64}{Γ ⊢ lhs - rhs : i64}$              | Integer subtraction       |
| expression < expression    | $\dfrac{Γ ⊢ lhs : i64 ∧ Γ ⊢ rhs : i64}{Γ ⊢ lhs < rhs : bool}$             | Integer less-than         |
|                            | $\dfrac{Γ ⊢ lhs : bool ∧ Γ ⊢ rhs : bool}{Γ ⊢ lhs < rhs : bool}$           | See [truth tables]        |
| expression <= expression   | $\dfrac{Γ ⊢ lhs : i64 ∧ Γ ⊢ rhs : i64}{Γ ⊢ lhs <= rhs : bool}$            | Integer less-or-equal     |
|                            | $\dfrac{Γ ⊢ lhs : bool ∧ Γ ⊢ rhs : bool}{Γ ⊢ lhs <= rhs : bool}$          | See [truth tables]        |
| expression > expression    | $\dfrac{Γ ⊢ lhs : i64 ∧ Γ ⊢ rhs : i64}{Γ ⊢ lhs > rhs : bool}$             | Integer greater-than      |
|                            | $\dfrac{Γ ⊢ lhs : bool ∧ Γ ⊢ rhs : bool}{Γ ⊢ lhs > rhs : bool}$           | See [truth tables]        |
| expression >= expression   | $\dfrac{Γ ⊢ lhs : i64 ∧ Γ ⊢ rhs : i64}{Γ ⊢ lhs >= rhs : bool}$            | Integer greater-or-equal  |
|                            | $\dfrac{Γ ⊢ lhs : bool ∧ Γ ⊢ rhs : bool}{Γ ⊢ lhs >= rhs : bool}$          | See [truth tables]        |
| expression == expression   | $\dfrac{Γ ⊢ lhs : i64 ∧ Γ ⊢ rhs : i64}{Γ ⊢ lhs == rhs : bool}$            | Integer equality          |
|                            | $\dfrac{Γ ⊢ lhs : bool ∧ Γ ⊢ rhs : bool}{Γ ⊢ lhs == rhs : bool}$          | See [truth tables]        |
| expression != expression   | $\dfrac{Γ ⊢ lhs : i64 ∧ Γ ⊢ rhs : i64}{Γ ⊢ lhs \text{ != } rhs : bool}$   | Integer nonequality       |
|                            | $\dfrac{Γ ⊢ lhs : bool ∧ Γ ⊢ rhs : bool}{Γ ⊢ lhs \text{ != } rhs : bool}$ | See [truth tables]        |
| expression && expression   | $\dfrac{Γ ⊢ lhs : bool ∧ Γ ⊢ rhs : bool}{Γ ⊢ lhs\ \&\&\ rhs : bool}$      | Short-circuiting AND      |
| expression \|\| expression | $\dfrac{Γ ⊢ lhs : bool ∧ Γ ⊢ rhs : bool}{Γ ⊢ lhs\ \|\|\ rhs : bool}$      | Short-circuiting OR       |

### Overflow and underflow

Integer overflow and underflow wraps.

### Floored division and remainder

The remainder always has the sign of the right-hand side.

| a   | b   | a / b | a % b |
| --- | --- | ----- | ----- |
| 3   | 16  | 0     | 3     |
| -3  | 16  | -1    | 13    |
| 3   | -16 | -1    | -13   |
| -3  | -16 | 0     | -3    |

### Truth tables

[truth tables]: #truth-tables

| a         | !a    |
| --------- | ----- |
| **false** | TRUE  |
| **TRUE**  | false |

| a         | b         | a && b | a \|\| b | a == b | a != b | a < b | a <= b | a > b | a >= b |
| --------- | --------- | ------ | -------- | ------ | ------ | ----- | ------ | ----- | ------ |
| **false** | **false** | false  | false    | TRUE   | false  | false | TRUE   | false | TRUE   |
| **false** | **TRUE**  | false  | TRUE     | false  | TRUE   | TRUE  | TRUE   | false | false  |
| **TRUE**  | **false** | false  | TRUE     | false  | TRUE   | false | false  | TRUE  | TRUE   |
| **TRUE**  | **TRUE**  | TRUE   | TRUE     | TRUE   | false  | false | TRUE   | false | TRUE   |

## Statement semantics

- `if` statements evaluate their condition, require it to be a boolean, and branch in the obvious way.
- `while` expressions loop in the obvious way.
- Blocks introduce a new child scope for `let` statements.
- `let` statements add a new variable binding to the current scope and give it
  an initial value based on its expression.
  - Variables are integers.
- Assignment statements look up a variable in the stack of scopes and change its current value to the result of evaluating the right-hand side.

# Not Yet Implemented

## Type system

- Structural.
- Product `struct` and sum `enum` types.
  - Tuples = anonymous structs.
  - Unions = anonymous enums.
- Should compile to WIT.
- Has support for linear types.
- Pseudo-generics for built-in constructs like `Utxo<AnotherContractName>`.
  - Because we eventually need functions that can accept "any UTXO satisfying X condition".
- Effects and resumable errors are typed as part of the signature of a function.
- Fatal errors (fail the transaction) are not typed.

## Semantics

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

### Type system

Type conveniences:

- Pattern matching.
  - In `match`:
    - Enums should have exhaustiveness checking.
    - Motive: match on enum messages between contracts.
  - In `let`:
    - When one pattern can cover the whole value space (namely structs).
  - Spread operator `..` to ignore remainder of fields.

### Type identities

Structure type definitions can be hashed for comparison. Names do not matter (structural typing).

- Algebraic Data Types (ADTs) are supported

  - Struct identities are based on their field types, in order
    - So `(i32, i32)` == `struct Foo { a: i32, b: i32 }` == `struct Bar { b: i32, c: i32 }`
    - No such thing as anonymous `{ a: i32, b: i32 }`.
  - Enum identities are based on their variant discriminators (ordinals), and the field types in order of each variant
    - So `i32 | (i32, i32)` == `enum Foo { A { b: i32, }, C { d: i32, e: i32 } }`

- Function identities are based on their name and their type.
  - Function types are based on their parameter types in order, return type, and possible effect set

# Potential Future Ideas

- Struct updates: `Foo { a: 1, ..old_foo }`
- In-script unit and property tests.

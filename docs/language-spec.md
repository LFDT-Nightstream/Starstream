---
sidebar_position: 2
---

# Starstream language specification

## Grammar

This document provides a complete EBNF grammar specification for the IMP
(Imperative) language used in the Starstream DSL project. The grammar is written
using ISO EBNF notation—specifically parentheses with a trailing `?`
for optional groups—and follows literate programming principles (code embedded as
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
program ::= definition*

(* Definitions *)

definition ::=
  | import_definition
  | function_definition
  | struct_definition
  | enum_definition
  | utxo_definition
  | abi_definition

import_definition ::=
  | "import" "{" import_named_item ( "," import_named_item )* "}" "from" import_source ";"
  | "import" identifier "from" import_source ";"

import_named_item ::= identifier ( "as" identifier )?

import_source ::= identifier ":" identifier "/" identifier

function_definition ::= ( function_export )? function

function_export ::=
  | "script"

function ::=
  "fn" identifier
  "(" ( parameter ( "," parameter )* )? ")"
  ( "->" type_annotation )?
  block

parameter ::= identifier ":" type_annotation

struct_definition ::=
  "struct" identifier "{" ( struct_field ( "," struct_field )* )? "}"

struct_field ::= identifier ":" type_annotation

enum_definition ::=
  "enum" identifier "{" ( enum_variant ( "," enum_variant )* )? "}"

enum_variant ::= identifier ( enum_variant_tuple_payload | enum_variant_struct_payload )?

enum_variant_tuple_payload ::= "(" ( type_annotation ( "," type_annotation )* )? ")"

enum_variant_struct_payload ::= "{" ( struct_field ( "," struct_field )* )? "}"

utxo_definition ::=
  "utxo" identifier "{" utxo_part* "}"

utxo_part ::=
  | storage_utxo_part
  | main_fn_utxo_part

storage_utxo_part ::= "storage" "{" utxo_global* "}"

utxo_global ::= "let" "mut" identifier ":" type_annotation ";"

main_fn_utxo_part ::= "main" function

abi_definition ::=
  "abi" identifier "{" abi_part* "}"

abi_part ::=
  | event_definition

event_definition ::=
  "event" identifier "(" ( parameter ( "," parameter )* )? ")" ";"

(* Type syntax *)

type_annotation ::= identifier ( "<" type_annotation ( "," type_annotation )* ">" )?

(* Blocks and statements *)

block ::= "{" statement* ( expression )? "}"

statement ::=
  | variable_declaration
  | assignment
  | while_statement
  | return_statement
  | expression_statement

variable_declaration ::= "let" ("mut")? identifier (":" type_annotation)? "=" expression ";"

assignment ::= identifier "=" expression ";"

while_statement ::= "while" "(" expression ")" block

return_statement ::= "return" ( expression )? ";"

expression_statement ::= expression ";"

(* Expressions *)

expression ::=
  | postfix_expression
  (* High to low precedence *)
  | unary_expression
  | multiplicative_expression
  | additive_expression
  | comparison_expression
  | equality_expression
  | logical_and_expression
  | logical_or_expression

(* Postfix expressions: function calls and field access *)
postfix_expression ::= primary_expression ( call_suffix | field_suffix )*
call_suffix ::= "(" ( expression ( "," expression )* )? ")"
field_suffix ::= "." identifier

(* Primary expressions are those outside the precedence table *)
primary_expression ::=
  | "(" expression ")"
  | identifier
  | integer_literal
  | boolean_literal
  | unit_literal
  | struct_literal
  | enum_construction
  | namespace_call
  | emit_expression
  | raise_expression
  | runtime_expression
  | block
  | if_expression
  | match_expression

emit_expression ::= "emit" identifier "(" ( expression ( "," expression )* )? ")"

raise_expression ::= "raise" expression

runtime_expression ::= "runtime" expression

namespace_call ::= identifier "::" identifier "(" ( expression ( "," expression )* )? ")"

struct_literal ::= identifier "{" ( struct_field_initializer ( "," struct_field_initializer )* )? "}"

struct_field_initializer ::= identifier ":" expression

enum_construction ::=
  identifier "::" identifier ( enum_constructor_tuple_payload | enum_constructor_struct_payload )?

enum_constructor_tuple_payload ::= "(" ( expression ( "," expression )* )? ")"

enum_constructor_struct_payload ::= "{" ( struct_field_initializer ( "," struct_field_initializer )* )? "}"

match_expression ::= "match" expression "{" ( match_arm ( "," match_arm )* )? "}"

match_arm ::= pattern "=>" block

pattern ::=
  | "_"
  | integer_literal
  | boolean_literal
  | unit_literal
  | identifier
  | struct_pattern
  | enum_variant_pattern

struct_pattern ::= identifier "{" ( struct_field_pattern ( "," struct_field_pattern )* )? "}"

struct_field_pattern ::=
  | identifier ":" pattern
  | identifier

enum_variant_pattern ::=
  identifier "::" identifier ( enum_pattern_tuple_payload | enum_pattern_struct_payload )?

enum_pattern_tuple_payload ::= "(" ( pattern ( "," pattern )* )? ")"

enum_pattern_struct_payload ::= "{" ( struct_field_pattern ( "," struct_field_pattern )* )? "}"

if_expression ::= "if" "(" expression ")" block ( "else" "if" "(" expression ")" block )* ( "else" block )?

unary_expression ::= ("-" | "!") expression

multiplicative_expression ::= expression ( "*" | "/" | "%" ) expression

additive_expression ::= expression ( "+" | "-" ) expression

comparison_expression ::= expression ( "<" | "<=" | ">" | ">=" ) expression

equality_expression ::= expression ( "==" | "!=" ) expression

logical_and_expression ::= expression "&&" expression

logical_or_expression ::= expression "||" expression

(* Literals and other terminals *)

identifier ::= [a-zA-Z_][a-zA-Z0-9_]*

integer_literal ::= [0-9]+

boolean_literal ::= "true" | "false"

unit_literal ::= "(" ")"
```

Definitions live exclusively at the program (module) scope. Statements appear inside blocks (function bodies, control-flow branches, etc.) and cannot occur at the top level.

`type_annotation` names reuse the type declarations defined elsewhere in this
spec (e.g., `i64`, `bool`, `CustomType`). Structured annotations such as tuples
or generic parameters extend this rule by nesting additional `type_annotation`
instances between `<…>` as described in the [Type System](#type-system) section.
Record and enum shapes must first be declared via `struct`/`enum` definitions
before they can be referenced. The name `_` means "unspecified", a free type
variable subject to inference.

The following reserved words may not be used as identifiers:

- `let`
- `mut`
- `if`
- `else`
- `while`
- `true`
- `false`
- `fn`
- `return`
- `struct`
- `enum`
- `match`
- `abi`
- `event`
- `emit`
- `import`
- `from`
- `as`
- `raise`
- `runtime`

<!--
  NOTE: When updating this grammar, also update:
  - the [canonical parser](../starstream-compiler/src/parser/).
  - the [Tree-sitter grammar](../tree-sitter-starstream/grammar.js).
  - the [VSC language configuration](../vscode-starstream/language-configuration.json).
-->

## Comments and whitespace

Comments and whitespace may appear between terminal tokens.

- `/*` starts a comment that ends at the first `*/` (no nesting).
- `//` starts a comment that ends at a new line.
- `///` starts a doc comment that ends at a new line. Multiple consecutive `///`
  lines form a single doc block. Doc comments attach to the definition
  immediately following them and are displayed in IDE hover information.
- `#!` at the start of a program starts a comment that ends at a new line.

### Doc comments

Doc comments use the `///` prefix and document the definition that follows:

```starstream
/// Adds two integers and returns their sum.
fn add(a: i64, b: i64) -> i64 {
    a + b
}

/// A point in 2D space.
struct Point {
    x: i64,
    y: i64,
}
```

The content after `/// ` (including the optional space) is extracted and
displayed in IDE hover tooltips above the type information.

## Precedence and Associativity

| Precedence  | Operator             | Associativity | Description           |
| ----------- | -------------------- | ------------- | --------------------- |
| 8 (highest) | `.`, `()`            | Left          | Field access, Call    |
| 7           | `!`, `-`             | Right         | Unary operators       |
| 6           | `*`, `/`, `%`        | Left          | Multiplicative     |
| 5           | `+`, `-`             | Left          | Additive           |
| 4           | `<`, `<=`, `>`, `>=` | Left          | Comparison         |
| 3           | `==`, `!=`           | Left          | Equality           |
| 2           | `&&`                 | Left          | Logical AND        |
| 1 (lowest)  | `\|\|`               | Left          | Logical OR         |

## Function visibility

All functions are visible within the module they are defined. By default,
functions are private to the module. The `fn` keyword can be preceded by a
visibility modifier:

- `script fn` exports a coordination script. It can be the root of a transaction
  or called by another coordination script.

## Imports

Imports bring external functions into scope from WIT-style interface paths.

### Named imports

Named imports bring specific functions into the local scope:

```starstream
import { blockHeight } from starstream:std/cardano;
import { foo, bar as baz } from starstream:std/utils;
```

The imported functions can be called directly by name. Functions can be renamed using `as`.

### Namespace imports

Namespace imports bring all functions from an interface under a namespace alias:

```starstream
import cardano from starstream:std/cardano;
```

Functions are accessed using namespace-qualified syntax: `cardano::blockHeight()`.

### Effect annotations

Some imported functions have effect annotations that require special call syntax:

- **Runtime functions** are implemented by the language runtime as host functions (FFI calls) and must be called with the `runtime` keyword:
  ```starstream
  let height = runtime blockHeight();
  let height = runtime cardano::blockHeight();
  ```

- **Effectful functions** raise effects that can be caught, handled, and resumed by effect handlers. They must be called with the `raise` keyword:
  ```starstream
  let result = raise someEffectfulFunction();
  ```

Calling an effectful or runtime function without the appropriate keyword is a type error.

## Scopes

- Every expression exists within a stack of scopes, in the traditional static scoping sense.
- Each scope has a table of variables, identified by name and having a static type and a current value.
- Syntactic blocks (curly braces) introduce new scopes.

## Expression semantics

- Integer literals work in the obvious way.
- Boolean literals work in the obvious way.
- Struct literals `TypeName { field: expr, ... }` evaluate each field expression once and produce a record value. Field names must be unique; order is irrelevant.
- Enum constructors use `TypeName::Variant` with a previously declared enum name. Tuple-style payloads evaluate left-to-right and are stored without reordering.
- Field accesses evaluate the receiver, ensure it is a struct value, then project the requested field. Accessing a missing field is a type error.
- `match` expressions evaluate the scrutinee first, then test arms sequentially. The first pattern whose shape matches the scrutinee executes. Pattern matching is exhaustive: all possible cases must be covered, and unreachable patterns are reported as errors. The wildcard pattern `_` matches any value without introducing a binding.
- Function calls `f(arg1, arg2, ...)` evaluate the callee (which must be a function name), then evaluate arguments left-to-right, then execute the function body with parameters bound to argument values. The call expression evaluates to the function's return value.
- Emit expressions `emit EventName(arg1, arg2, ...)` emit an event declared in an `abi` block. The event name must refer to an event definition in scope. Arguments are evaluated left-to-right and typechecked against the event's parameter types. The expression's type is always `()` (Unit). Unknown event names are type errors.
- Namespace-qualified calls `namespace::function(args...)` call a function from an imported namespace. The namespace must have been imported via `import namespace from ...;`.
- `raise expr` wraps an effectful function call. The inner expression must be a call to an effectful function. Using `raise` on a non-effectful call is a type error.
- `runtime expr` wraps a runtime function call. Runtime functions access runtime-only information (e.g., block height) and must be explicitly marked at the call site. Using `runtime` on a non-runtime call is a type error.
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
- Structural records/enums are compared by shape, not name. Two structs with identical field sets and types are interchangeable; enum variants must likewise line up by name and payload shape.

| Syntax rule                 | Type rule                                                                 | Value rule                |
| --------------------------- | ------------------------------------------------------------------------- | ------------------------- |
| integer_literal             | $\dfrac{}{Γ ⊢ integer\ literal : i64}$                                    | Integer literal           |
| boolean_literal             | $\dfrac{}{Γ ⊢ boolean\ literal : bool}$                                   | Boolean literal           |
| identifier                  | $\dfrac{ident : T ∈ Γ}{Γ ⊢ ident : T}$                                    | Refers to `let` in scope  |
| (expression)                | $\dfrac{Γ ⊢ e : T}{Γ ⊢ (e) : T}$                                          | Identity                  |
| !expression                 | $\dfrac{Γ ⊢ e : bool}{Γ ⊢\ !e : bool}$                                    | Boolean inverse           |
| -expression                 | $\dfrac{Γ ⊢ e : i64}{Γ ⊢ -e : i64}$                                       | Integer negation          |
| expression \* expression    | $\dfrac{Γ ⊢ lhs : i64 ∧ Γ ⊢ rhs : i64}{Γ ⊢ lhs * rhs : i64}$              | Integer multiplication    |
| expression / expression     | $\dfrac{Γ ⊢ lhs : i64 ∧ Γ ⊢ rhs : i64}{Γ ⊢ lhs / rhs : i64}$              | Integer floored division  |
| expression % expression     | $\dfrac{Γ ⊢ lhs : i64 ∧ Γ ⊢ rhs : i64}{Γ ⊢ lhs\ \%\ rhs : i64}$           | Integer floored remainder |
| expression + expression     | $\dfrac{Γ ⊢ lhs : i64 ∧ Γ ⊢ rhs : i64}{Γ ⊢ lhs + rhs : i64}$              | Integer addition          |
| expression - expression     | $\dfrac{Γ ⊢ lhs : i64 ∧ Γ ⊢ rhs : i64}{Γ ⊢ lhs - rhs : i64}$              | Integer subtraction       |
| expression < expression     | $\dfrac{Γ ⊢ lhs : i64 ∧ Γ ⊢ rhs : i64}{Γ ⊢ lhs < rhs : bool}$             | Integer less-than         |
|                             | $\dfrac{Γ ⊢ lhs : bool ∧ Γ ⊢ rhs : bool}{Γ ⊢ lhs < rhs : bool}$           | See [truth tables]        |
| expression &lt;= expression | $\dfrac{Γ ⊢ lhs : i64 ∧ Γ ⊢ rhs : i64}{Γ ⊢ lhs <= rhs : bool}$            | Integer less-or-equal     |
|                             | $\dfrac{Γ ⊢ lhs : bool ∧ Γ ⊢ rhs : bool}{Γ ⊢ lhs <= rhs : bool}$          | See [truth tables]        |
| expression > expression     | $\dfrac{Γ ⊢ lhs : i64 ∧ Γ ⊢ rhs : i64}{Γ ⊢ lhs > rhs : bool}$             | Integer greater-than      |
|                             | $\dfrac{Γ ⊢ lhs : bool ∧ Γ ⊢ rhs : bool}{Γ ⊢ lhs > rhs : bool}$           | See [truth tables]        |
| expression >= expression    | $\dfrac{Γ ⊢ lhs : i64 ∧ Γ ⊢ rhs : i64}{Γ ⊢ lhs >= rhs : bool}$            | Integer greater-or-equal  |
|                             | $\dfrac{Γ ⊢ lhs : bool ∧ Γ ⊢ rhs : bool}{Γ ⊢ lhs >= rhs : bool}$          | See [truth tables]        |
| expression == expression    | $\dfrac{Γ ⊢ lhs : i64 ∧ Γ ⊢ rhs : i64}{Γ ⊢ lhs == rhs : bool}$            | Integer equality          |
|                             | $\dfrac{Γ ⊢ lhs : bool ∧ Γ ⊢ rhs : bool}{Γ ⊢ lhs == rhs : bool}$          | See [truth tables]        |
| expression != expression    | $\dfrac{Γ ⊢ lhs : i64 ∧ Γ ⊢ rhs : i64}{Γ ⊢ lhs \text{ != } rhs : bool}$   | Integer nonequality       |
|                             | $\dfrac{Γ ⊢ lhs : bool ∧ Γ ⊢ rhs : bool}{Γ ⊢ lhs \text{ != } rhs : bool}$ | See [truth tables]        |
| expression && expression    | $\dfrac{Γ ⊢ lhs : bool ∧ Γ ⊢ rhs : bool}{Γ ⊢ lhs\ \&\&\ rhs : bool}$      | Short-circuiting AND      |
| expression \|\| expression  | $\dfrac{Γ ⊢ lhs : bool ∧ Γ ⊢ rhs : bool}{Γ ⊢ lhs\ \|\|\ rhs : bool}$      | Short-circuiting OR       |
| f(e₁, ..., eₙ)              | $\dfrac{f : (T_1, ..., T_n) → R ∧ Γ ⊢ e_i : T_i}{Γ ⊢ f(e_1, ..., e_n) : R}$ | Function call             |
| emit E(e₁, ..., eₙ)         | $\dfrac{E : event(T_1, ..., T_n) ∧ Γ ⊢ e_i : T_i}{Γ ⊢ emit\ E(e_1, ..., e_n) : ()}$ | Event emission            |

### Structural typing rules

- Struct and enum definitions introduce canonical shapes, but names are merely aliases; two independently-declared structs with the same field names/types are interchangeable.
- Type annotations refer to those named definitions. During type checking the compiler canonicalizes field/variant order before comparing shapes so structurally identical names unify.
- Unification succeeds for records when both sides have the same field names (order-insensitive) and each corresponding field type unifies. A similar rule holds for enums, matching variant names and payload arity/type.
- Pattern matching and field access operate on these shapes; renaming a type but keeping its layout requires no code changes.

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

| a         | b         | a && b | a \|\| b | a == b | a != b | a < b | a &lt;= b | a > b | a >= b |
| --------- | --------- | ------ | -------- | ------ | ------ | ----- | --------- | ----- | ------ |
| **false** | **false** | false  | false    | TRUE   | false  | false | TRUE      | false | TRUE   |
| **false** | **TRUE**  | false  | TRUE     | false  | TRUE   | TRUE  | TRUE      | false | false  |
| **TRUE**  | **false** | false  | TRUE     | false  | TRUE   | false | false     | TRUE  | TRUE   |
| **TRUE**  | **TRUE**  | TRUE   | TRUE     | TRUE   | false  | false | TRUE      | false | TRUE   |

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

### Functions

- Functions are first-class **definitions** that bind a name to a parameterized block at module scope.
- All parameters **must** carry an explicit type annotation (`identifier: Type`).
- The declared return type is optional; when omitted the function returns the `Unit` type (`()`).
- The body block may terminate with a tail expression (no trailing semicolon). That expression becomes the implicit return value when no explicit `return` is executed.
- `return` statements exit the current function early. `return;` returns the unit value, while `return <expr>;` yields the expression's value.
- Parameter and return annotations participate in the Hindley–Milner inference engine; they constrain the inferred types of the body expressions.
- Blocks that end without an explicit `return` or tail expression evaluate to unit.

```star
fn some_function(a: i64, b: i64) -> i64 {
  if (a > b) {
    return a;
  }

  a + b
}
```

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

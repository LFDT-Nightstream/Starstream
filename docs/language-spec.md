---
sidebar_position: 2
---

# Starstream language specification

This document describes the specification of the Starstream language (syntax
and abstract semantics), as currently implemented. For planned future features,
see [Future specifications][future].

[future]: ./future-spec.md

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

### Grammar rules

```ebnf
program ::= definition*

(* Definitions *)

definition ::=
  | contract_definition
  | import_definition
  | function_definition
  | struct_definition
  | enum_definition
  | utxo_definition
  | abi_definition

contract_definition ::= "contract" ";"

import_definition ::=
  | "import" "{" import_named_item ( "," import_named_item )* "}" "from" import_source ";"
  | "import" identifier "from" import_source ";"

import_named_item ::= identifier ( "as" identifier )?

import_source ::=
  | identifier ":" identifier ( "/" identifier )?
  | string_literal

function_definition ::= ( function_export )? function

function_export ::=
  | "script"

function ::=
  "fn" identifier
  "(" ( function_parameter ( "," function_parameter )* )? ")"
  ( "->" type_annotation )?
  block

function_parameter ::= ("pub")? identifier ":" type_annotation

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
  | fn_utxo_part
  | abi_impl_utxo_part

storage_utxo_part ::= "storage" "{" utxo_global* "}"

utxo_global ::= "let" "mut" identifier ":" type_annotation ";"

fn_utxo_part ::= ("main")? function

abi_impl_utxo_part ::= "impl" identifier "{" impl_part* "}"

impl_part ::=
  | fn_impl_part

fn_impl_part ::= function

abi_definition ::=
  "abi" identifier "{" abi_part* "}"

abi_part ::=
  | event_definition
  | abi_fn_declaration

event_definition ::=
  "event" identifier "(" ( parameter ( "," parameter )* )? ")" ";"

abi_fn_declaration ::=
  "fn" identifier "(" ( parameter ( "," parameter )* )? ")" ( "->" type_annotation )? ";"

(* Type syntax *)

type_annotation ::= identifier ( "<" type_annotation ( "," type_annotation )* ">" )?

(* Blocks and statements *)

block ::= "{" statement* ( expression )? "}"

statement ::=
  | variable_declaration
  | assignment
  | while_statement
  | return_statement
  | resume_statement
  | expression_statement

variable_declaration ::= "let" ("pub")? ("mut")? identifier (":" type_annotation)? "=" expression ";"

assignment ::= identifier "=" expression ";"

while_statement ::= "while" "(" expression ")" block

return_statement ::= "return" ( expression )? ";"

resume_statement ::= "resume" ";"

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
  | disclose_expression
  | emit_expression
  | raise_expression
  | runtime_expression
  | block
  | if_expression
  | match_expression
  | yield_expression

disclose_expression ::= "disclose" "(" expression ")"

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

if_condition ::=
  | "(" expression ")"
  | identifier "is" identifier

if_expression ::= "if" if_condition block ( "else" "if" if_condition block )* ( "else" block )?

yield_expression ::= "yield" "(" ( identifier ( "," identifier )* )? ")"

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

(* Quoted string used only as an `import_source`. Backslash-escapes are
   limited to `\\`, `\"`, `\n`, `\r`, `\t`. *)
string_literal ::= '"' ( [^"\\] | "\\" ["\\nrt] )* '"'
```

Definitions live exclusively at the program (module) scope. Statements appear
inside blocks (function bodies, control-flow branches, etc.) and cannot occurat the top level.

`type_annotation` names reuse the type declarations defined elsewhere in this
spec (e.g., `i64`, `bool`, `CustomType`). Structured annotations such as tuples
or generic parameters extend this rule by nesting additional `type_annotation`
instances between `<…>` as described in the [Types](#types) section.
Record and enum shapes must first be declared via `struct`/`enum` definitions
before they can be referenced. The name `_` means "unspecified", a free type
variable subject to inference.

The following reserved words may not be used as identifiers:

- `let`
- `pub`
- `mut`
- `if`
- `else`
- `while`
- `true`
- `false`
- `fn`
- `return`
- `resume`
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
- `disclose`
- `is`
- `yield`
- `contract`

<!--
  NOTE: When updating this grammar, also update:
  - the [canonical parser](../starstream-compiler/src/parser/).
  - the [Tree-sitter grammar](../tree-sitter-starstream/grammar.js).
  - the [VSC language configuration](../vscode-starstream/language-configuration.json).
-->

### Comments and whitespace

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

### Precedence and associativity

| Precedence  | Operator             | Associativity | Description        |
| ----------- | -------------------- | ------------- | ------------------ |
| 8 (highest) | `.`, `()`            | Left          | Field access, Call |
| 7           | `!`, `-`             | Right         | Unary operators    |
| 6           | `*`, `/`, `%`        | Left          | Multiplicative     |
| 5           | `+`, `-`             | Left          | Additive           |
| 4           | `<`, `<=`, `>`, `>=` | Left          | Comparison         |
| 3           | `==`, `!=`           | Left          | Equality           |
| 2           | `&&`                 | Left          | Logical AND        |
| 1 (lowest)  | `\|\|`               | Left          | Logical OR         |

## Types

### Built-in integer types

| Type  | Signed | Bits | Min                  | Max                  |
| ----- | ------ | ---- | -------------------- | -------------------- |
| `i8`  | yes    | 8    | -128                 | 127                  |
| `i16` | yes    | 16   | -32768               | 32767                |
| `i32` | yes    | 32   | -2147483648          | 2147483647           |
| `i64` | yes    | 64   | -9223372036854775808 | 9223372036854775807  |
| `u8`  | no     | 8    | 0                    | 255                  |
| `u16` | no     | 16   | 0                    | 65535                |
| `u32` | no     | 32   | 0                    | 4294967295           |
| `u64` | no     | 64   | 0                    | 18446744073709551615 |

### Other built-in types

- `()` - unit type with one value, `()`
- `bool` - boolean type with two values, `true` and `false`
- `Option<T>` - generic type with `None` and `Some(T)` variants
- `Result<T, E>` - generic type with `Ok(T)` and `Err(E)` variants
- `Utxo` - handle to a Utxo of unknown contract and ABI

### User-defined types

- `struct Foo` syntax declares record types
- `enum Foo` syntax declares variant types
- `utxo Foo` syntax declares Utxo handle types of known contract but unknown ABI
  - `main fn` items within the `utxo` block act as this type's constructors
  - Can be unconditionally upcast to the root `Utxo` handle
  - Can attempt to downcast from the root `Utxo` handle (returns `Result`)

No user-defined generics at this time.

### Structural typing rules

- Struct and enum definitions introduce canonical shapes, but names are merely aliases; two independently-declared structs with the same field names/types are interchangeable.
- Type annotations refer to those named definitions. During type checking the compiler canonicalizes field/variant order before comparing shapes so structurally identical names unify.
- Unification succeeds for records when both sides have the same field names (order-insensitive) and each corresponding field type unifies. A similar rule holds for enums, matching variant names and payload arity/type.
- Pattern matching and field access operate on these shapes; renaming a type but keeping its layout requires no code changes.

## Contracts

Any `.star` file that contains a bare `contract;` declaration is a **codegen
entry point**. The marker may appear anywhere at the top level of the file;
the formatter sorts it to the top.

```starstream
contract;

script fn run() -> i64 {
    42
}
```

Files without `contract;` are *helpers* — reusable functions and types that
contracts pull in via path imports.

**One workspace, one module graph.** The scan-based CLI commands
(`check`, `docs`, `build`) and the language server build a **single
workspace-wide module graph** over every `.star` file they find (including
files transitively pulled in by path imports). Type-checking runs **once**
across that whole graph, so a helper shared by two contracts is parsed and
checked exactly once. Code generation then runs once per `contract;` node,
walking only the subgraph reachable from that contract.

The `starstream wasm` command is the exception: it always builds a
single-file mini-graph rooted at the file you point it at, treating that
file as the contract regardless of whether it declares `contract;`.

**Cross-contract imports are not supported yet.** Any edge in the graph
whose *target* declares `contract;` is rejected, with an error pointing at
the import statement that created the edge — whether the importer is itself
a contract or a helper doesn't matter.

**Orphan helpers** (files with no `contract;` that aren't reached by any
contract's import chain) still participate in the workspace graph as loose
nodes. They're type-checked along with everything else, so editors get
real-time diagnostics on them even before they're imported anywhere. They
just produce no wasm output.

> For the user-facing commands that drive this model
> (`starstream wasm`/`check`/`docs`/`build`/`lsp`), see [CLI](./cli.md).

## Imports

Imports bring external functions into scope from WIT-style interface paths or
from another `.star` file in the project via path imports.

The available import sources are:

- `starstream:std` - Starstream builtins known to the compiler.
  - `/cardano` - functions expected to be available when hosted on Cardano.
- `"./relative/path.star"` - another `.star` file in the project (see
  [Path imports](#path-imports)).

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

### Path imports

A path import references another `.star` file by a quoted relative path,
resolved against the *importing* file's directory. The `.star` extension is
required.

```starstream
import { add, Point } from "./helpers/math.star";
import math from "./helpers/math.star";
import { foo } from "../shared/util.star";
```

**Resolution rules:**

- Paths must be relative — they must begin with `./` or `../`. Absolute paths
  and bare module names are rejected.
- Paths must end with the `.star` extension.
- Paths are resolved relative to the directory of the importing file.
- The path is canonicalized (`./` and `../` are normalized; symlinks are
  resolved) so a file that is reached via two different relative paths is
  loaded only once within a single contract graph.

**Visibility:**

- Named imports are strict: only the names listed in `{ ... }` are visible to
  the importing module. Other top-level definitions in the target file are
  not implicitly accessible.
- Namespace imports expose all top-level functions of the target file under
  the alias (e.g., `math::add(...)`). They do not expose types.
- Top-level types (`struct`, `enum`, `abi`, `utxo`) are referenced by name —
  importing one with `import { Point } from "./shapes.star";` makes `Point`
  usable in the importing file's type annotations.

**Cross-contract guard:**

- A contract may not import from another file that also declares `contract;`.
  The compiler raises a hard error. Cross-contract calls will land in a
  future release.

**Module graph and cycles:**

- For each contract entry, the compiler builds a directed graph of the
  `.star` files reachable through its path imports. The graph is
  topologically sorted before type-checking, so dependencies are processed
  before their dependents.
- Cycles in the path-import graph are a hard compile error.

**`script fn` semantics:**

- Every `script fn` reachable from a contract's entry is exported as a
  transaction root in that contract's wasm — whether it's declared in the
  entry file itself or in one of its helpers. If you wrote `script fn`,
  you meant to expose it; the compiler honors that.

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

## Functions

- Functions bind a name to a parameterized block at module scope.
- All parameters must carry an explicit type annotation.
- Function parameters may optionally be marked `pub` (`pub name: Type`) to indicate non-secrecy (witness protection program).
- The declared return type is optional; when omitted the function returns the `Unit` type (`()`).
  - Utxo `main fn`s may not manually specify a return type. Their body must `yield` to yield or return to consume the Utxo. Callers see their return type as being a handle to the created Utxo.
- The body block may terminate with a tail expression (no trailing semicolon). That expression becomes the implicit return value when no explicit `return` is executed.
- `return` statements exit the current function early. `return;` returns the unit value, while `return <expr>;` returns the expression's value.
- `resume` statements (valid only inside a `utxo`) transfer control flow back to the `yield` point the Utxo was previously suspended at.
- Parameter and return annotations participate in the Hindley–Milner inference engine; they constrain the inferred types of the body expressions.
- Blocks that end without an explicit `return` or tail expression evaluate to unit.

Example:

```star
fn some_function(a: i64, b: i64) -> i64 {
  if (a > b) {
    return a;
  }

  a + b
}
```

### Function visibility

All functions are visible within the module they are defined. By default,
functions are private to the module. The `fn` keyword can be preceded by a
visibility modifier:

- `script fn` exports a coordination script. It can be the root of a transaction
  or called by another coordination script.

## Scopes

- Every expression exists within a stack of scopes, in the traditional static scoping sense.
- Each scope has a table of variables, identified by name and having a static type and a current value.
- Syntactic blocks (curly braces) introduce new scopes.

## Statements

- `if` statements evaluate their condition, require it to be a boolean, and branch in the obvious way.
- `while` expressions loop in the obvious way.
- Blocks introduce a new child scope for `let` statements.
- `let` statements add a new variable binding to the current scope and give it
  an initial value based on its expression.
  - Variables may be integers (`i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`), booleans, structs, or enums.
  - `let pub name = expr;` and `let pub mut name = expr;` require `expr` to already be public, or explicitly disclosed via `disclose(expr)`.
- Assignment statements look up a variable in the stack of scopes and change its current value to the result of evaluating the right-hand side.
  - Assigning to any public target (including `storage`) requires a public RHS; use `disclose(...)` when assigning private values into a public binding.
  - Reads from `storage` bindings are already on the public side.

## Expressions

- Integer literals are polymorphic: an unadorned numeric literal like `42` adopts the integer type determined by context (e.g. a type annotation or function parameter type). When no context constrains the type, the literal defaults to `i64`. A compile-time error is emitted if the literal value does not fit in the resolved type (e.g. `let x: i8 = 300` is an error).
- Boolean literals work in the obvious way.
- Struct literals `TypeName { field: expr, ... }` evaluate each field expression once and produce a record value. Field names must be unique; order is irrelevant.
- Enum constructors use `TypeName::Variant` with a previously declared enum name. Tuple-style payloads evaluate left-to-right and are stored without reordering.
- Field accesses evaluate the receiver, ensure it is a struct value, then project the requested field. Accessing a missing field is a type error.
- `match` expressions evaluate the scrutinee first, then test arms sequentially. The first pattern whose shape matches the scrutinee executes. Pattern matching is exhaustive: all possible cases must be covered, and unreachable patterns are reported as errors. The wildcard pattern `_` matches any value without introducing a binding.
- Function calls `f(arg1, arg2, ...)` evaluate the callee (which must be a function name), then evaluate arguments left-to-right, then execute the function body with parameters bound to argument values. The call expression evaluates to the function's return value.
- Emit expressions `emit EventName(arg1, arg2, ...)` emit an event declared in an `abi` block. The event name must refer to an event definition in scope. Arguments are evaluated left-to-right and typechecked against the event's parameter types. The expression's type is always `()` (Unit). Unknown event names are type errors.
- Namespace-qualified calls `namespace::function(args...)` call a function from an imported namespace. The namespace must have been imported via `import namespace from ...;`.
- `disclose(expr)` converts `expr` to the public visibility side without changing its static type. If `expr` is already public, the wrapper is redundant and should be removed.
- `raise expr` wraps an effectful function call. The inner expression must be a call to an effectful function. Using `raise` on a non-effectful call is a type error.
- `runtime expr` wraps a runtime function call. Runtime functions access runtime-only information (e.g., block height) and must be explicitly marked at the call site. Using `runtime` on a non-runtime call is a type error.
- Variable names refer to a `let` declaration earlier in the current scope or
  one of its parents, but not child scopes.
- Arithmetic operators: `+`, `-`, `*`, `/`, `%` work over integers of the same type.
  - Both operands must have the same integer type; cross-type arithmetic (e.g. `i32 + i64`) is a type error.
  - The supported integer types and their ranges are:
  - Integer overflow and underflow is checked at runtime and traps.
  - `/` and `%` are floored for signed types. `%` has the same sign as the divisor.
  - For unsigned types, `/` and `%` are standard unsigned division and remainder.
- Unary `-` applies to signed integers only. Negating an unsigned integer is a type error. Unary `!` applies to booleans.
- Comparison operators: `==`, `!=`, `<`, `>`, `<=`, `>=` accept (integer, integer) of the same type or (boolean, boolean) and
  produce booleans.
- The boolean operators `!`, `&&`, `||` accept booleans and produce
  booleans.
  - `&&` and `||` are short-circuiting.
- Structural records/enums are compared by shape, not name. Two structs with identical field sets and types are interchangeable; enum variants must likewise line up by name and payload shape.
- `yield` expressions mark points where a Utxo's execution can be suspended to the ledger.
  They are only valid inside Utxo `main fn`s.

| Syntax rule                 | Type rule                                                                                        | Value rule                        |
| --------------------------- | ------------------------------------------------------------------------------------------------ | --------------------------------- |
| integer_literal             | $\dfrac{}{Γ ⊢ integer\ literal : Int}$ where $Int$ is inferred from context, defaulting to $i64$ | Polymorphic integer literal       |
| boolean_literal             | $\dfrac{}{Γ ⊢ boolean\ literal : bool}$                                                          | Boolean literal                   |
| identifier                  | $\dfrac{ident : T ∈ Γ}{Γ ⊢ ident : T}$                                                           | Refers to `let` in scope          |
| (expression)                | $\dfrac{Γ ⊢ e : T}{Γ ⊢ (e) : T}$                                                                 | Identity                          |
| !expression                 | $\dfrac{Γ ⊢ e : bool}{Γ ⊢\ !e : bool}$                                                           | Boolean inverse                   |
| -expression                 | $\dfrac{Γ ⊢ e : Int,\ Int\ signed}{Γ ⊢ -e : Int}$                                                | Signed integer negation           |
| expression \* expression    | $\dfrac{Γ ⊢ lhs : Int ∧ Γ ⊢ rhs : Int}{Γ ⊢ lhs * rhs : Int}$                                     | Integer multiplication            |
| expression / expression     | $\dfrac{Γ ⊢ lhs : Int ∧ Γ ⊢ rhs : Int}{Γ ⊢ lhs / rhs : Int}$                                     | Integer division                  |
| expression % expression     | $\dfrac{Γ ⊢ lhs : Int ∧ Γ ⊢ rhs : Int}{Γ ⊢ lhs\ \%\ rhs : Int}$                                  | Integer remainder                 |
| expression + expression     | $\dfrac{Γ ⊢ lhs : Int ∧ Γ ⊢ rhs : Int}{Γ ⊢ lhs + rhs : Int}$                                     | Integer addition                  |
| expression - expression     | $\dfrac{Γ ⊢ lhs : Int ∧ Γ ⊢ rhs : Int}{Γ ⊢ lhs - rhs : Int}$                                     | Integer subtraction               |
| expression < expression     | $\dfrac{Γ ⊢ lhs : Int ∧ Γ ⊢ rhs : Int}{Γ ⊢ lhs < rhs : bool}$                                    | Integer less-than                 |
|                             | $\dfrac{Γ ⊢ lhs : bool ∧ Γ ⊢ rhs : bool}{Γ ⊢ lhs < rhs : bool}$                                  | See [truth tables]                |
| expression &lt;= expression | $\dfrac{Γ ⊢ lhs : Int ∧ Γ ⊢ rhs : Int}{Γ ⊢ lhs <= rhs : bool}$                                   | Integer less-or-equal             |
|                             | $\dfrac{Γ ⊢ lhs : bool ∧ Γ ⊢ rhs : bool}{Γ ⊢ lhs <= rhs : bool}$                                 | See [truth tables]                |
| expression > expression     | $\dfrac{Γ ⊢ lhs : Int ∧ Γ ⊢ rhs : Int}{Γ ⊢ lhs > rhs : bool}$                                    | Integer greater-than              |
|                             | $\dfrac{Γ ⊢ lhs : bool ∧ Γ ⊢ rhs : bool}{Γ ⊢ lhs > rhs : bool}$                                  | See [truth tables]                |
| expression >= expression    | $\dfrac{Γ ⊢ lhs : Int ∧ Γ ⊢ rhs : Int}{Γ ⊢ lhs >= rhs : bool}$                                   | Integer greater-or-equal          |
|                             | $\dfrac{Γ ⊢ lhs : bool ∧ Γ ⊢ rhs : bool}{Γ ⊢ lhs >= rhs : bool}$                                 | See [truth tables]                |
| expression == expression    | $\dfrac{Γ ⊢ lhs : Int ∧ Γ ⊢ rhs : Int}{Γ ⊢ lhs == rhs : bool}$                                   | Integer equality                  |
|                             | $\dfrac{Γ ⊢ lhs : bool ∧ Γ ⊢ rhs : bool}{Γ ⊢ lhs == rhs : bool}$                                 | See [truth tables]                |
| expression != expression    | $\dfrac{Γ ⊢ lhs : Int ∧ Γ ⊢ rhs : Int}{Γ ⊢ lhs \text{ != } rhs : bool}$                          | Integer nonequality               |
|                             | $\dfrac{Γ ⊢ lhs : bool ∧ Γ ⊢ rhs : bool}{Γ ⊢ lhs \text{ != } rhs : bool}$                        | See [truth tables]                |
| expression && expression    | $\dfrac{Γ ⊢ lhs : bool ∧ Γ ⊢ rhs : bool}{Γ ⊢ lhs\ \&\&\ rhs : bool}$                             | Short-circuiting AND              |
| expression \|\| expression  | $\dfrac{Γ ⊢ lhs : bool ∧ Γ ⊢ rhs : bool}{Γ ⊢ lhs\ \|\|\ rhs : bool}$                             | Short-circuiting OR               |
| f(e₁, ..., eₙ)              | $\dfrac{f : (T_1, ..., T_n) → R ∧ Γ ⊢ e_i : T_i}{Γ ⊢ f(e_1, ..., e_n) : R}$                      | Function call                     |
| disclose(e)                 | $\dfrac{Γ ⊢ e : T}{Γ ⊢ disclose(e) : T}$                                                         | Converts `e` to public visibility |
| emit E(e₁, ..., eₙ)         | $\dfrac{E : event(T_1, ..., T_n) ∧ Γ ⊢ e_i : T_i}{Γ ⊢ emit\ E(e_1, ..., e_n) : ()}$              | Event emission                    |

In the rules above, $Int$ stands for any single integer type from `{i8, i16, i32, i64, u8, u16, u32, u64}`. Both operands of a binary operator must have the **same** integer type.

### Overflow and underflow

Integer overflow and underflow is checked at runtime and causes a trap (runtime error).

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

## Utxo methods and coroutine lifetimes

The basic flow for a coordination script interacting with a Utxo resembles:

1. Coordination script starts.
2. It calls a Utxo's `main fn`, which spawns the Utxo and starts its execution.
3. The `main fn` runs until it ends or hits a `yield`.
4. The Utxo makes itself suspendable by storing locals and program counter to globals / linear memory ("stackless").
5. Control flow then returns back to the coordination script caller.
6. The main-fn call expression evaluates to a handle to the new Utxo.
7. The contents of the `yield` determine what methods/ABIs are available:
    - Methods which contain no `resume` statements are "normal"; they can read (and write) storage, but do not affect the Utxo's lifetime.
      - Writing storage mutates the Utxo, which is logically similar to consuming it and producing another Utxo, but it cannot affect the Utxo's liveness or exposed ABIs, so we do not say that its lifetime has changed.
      - In Rust terms, `fn(&self)` and `fn(&mut self)`
    - Methods containing `resume` statements must return unit. The `resume` statement transfers control flow to the current `yield` point and runs until another `yield` point is hit, at which point the caller sees the method as having returned. Such methods therefore affect the Utxo's lifetime.
      - Flow typing by the caller is responsible for tracking the handle used to call the method.
      - In Rust terms, `fn(self) -> Self`, `-> Utxo`, `-> Option<Utxo>`, etc.

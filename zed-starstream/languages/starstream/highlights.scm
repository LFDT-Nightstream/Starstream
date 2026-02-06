; VSCode scopes: https://code.visualstudio.com/api/language-extensions/semantic-highlight-guide#standard-token-types-and-modifiers
; Zed scopes: https://zed.dev/docs/extensions/languages#syntax-highlighting

; Fallback identifier - FIRST so more specific rules below override it
(identifier) @variable

; Strict keywords that cannot be identifiers (see parser/primitives.rs)
[
  "let"
  "mut"
  "if"
  "else"
  "while"
  "true"
  "false"
  "fn"
  "return"
  "struct"
  "enum"
  "match"
  "abi"
  "event"
  "emit"
  "import"
  "from"
  "as"
  "raise"
  "runtime"
] @keyword

; Literals
; Zed recognizes @boolean, but VSC doesn't, so set @keyword as well.
(boolean_literal) @boolean
(integer_literal) @number

; Type annotations (override the fallback for types)
(type_annotation (identifier) @type)
(type_annotation "<" @operator)
(type_annotation "," @operator)
(type_annotation ">" @operator)

; Common operators
[
  "+"
  "-"
  "*"
  "/"
  "%"
  "=="
  "!="
  "<"
  "<="
  ">"
  ">="
  "&&"
  "||"
  "="
  "->"
] @operator

; Actual syntax forms
(function (identifier) @function.definition)
(function_export "script" @keyword)
(parameter (identifier) @parameter)
(parameter ":" @operator)
(variable_declaration (identifier) @variable.declaration)
(assignment (identifier) @variable.modification)
(struct_definition (identifier) @type.definition)
(struct_field (identifier) @variable.member)
(struct_field ":" @operator)
(enum_definition (identifier) @type.definition)
(enum_variant (identifier) @constant)
(block "{" @punctuation.bracket)
(block "}" @punctuation.bracket)
(struct_literal (identifier) @type)
(struct_field_initializer (identifier) @variable.member)
(struct_field_initializer ":" @operator)
(field_expression "." @operator)
(enum_constructor (identifier) @type)
(enum_constructor "::" @operator)
; Function calls
(call_expression
  function: (identifier) @function.call)
; Method calls
(call_expression
  function: (field_expression
    field: (identifier) @function.call))
(arguments "(" @punctuation.bracket)
(arguments ")" @punctuation.bracket)
(arguments "," @operator)
(match_expression "match" @keyword)
(match_arm "=>" @operator)
(utxo_definition "utxo" @keyword)
(storage_utxo_part "storage" @keyword)
(main_fn_utxo_part "main" @keyword)
; ABI and events
(abi_definition (identifier) @type.definition)
(event_definition (identifier) @function.definition)
(emit_expression
  event: (identifier) @function.call)

; Import definitions
(import_source
  namespace: (identifier) @namespace)
(import_source
  package: (identifier) @namespace)
(import_source
  interface: (identifier) @namespace)
(import_source ":" @operator)
(import_source "/" @operator)
(import_named_item
  imported: (identifier) @function)
(import_named_item
  local: (identifier) @function)
(import_named_items "{" @punctuation.bracket)
(import_named_items "}" @punctuation.bracket)

; Raise expression
(raise_expression "raise" @keyword)

; Runtime expression
(runtime_expression "runtime" @keyword)

(comment) @comment
(doc_comment) @comment.documentation
(shebang) @comment

; VSCode scopes: https://code.visualstudio.com/api/language-extensions/semantic-highlight-guide#standard-token-types-and-modifiers
; Zed scopes: https://zed.dev/docs/extensions/languages#syntax-highlighting

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
] @keyword

; Literals
; Zed recognizes @boolean, but VSC doesn't, so set @keyword as well.
(boolean_literal) @boolean
(integer_literal) @number

; Fallback identifier
(identifier) @variable

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
(function_definition (identifier) @function.definition)
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
("." @operator)
(enum_constructor (identifier) @type)
(enum_constructor "::" @operator)
(call_expression "(" @punctuation.bracket)
(call_expression ")" @punctuation.bracket)
(call_expression "," @operator)
(match_expression "match" @keyword)
(match_arm "=>" @operator)
(utxo_definition "utxo" @keyword)
(storage_utxo_part "storage" @keyword)

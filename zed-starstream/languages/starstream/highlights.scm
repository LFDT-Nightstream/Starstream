; VSCode scopes: https://code.visualstudio.com/api/language-extensions/semantic-highlight-guide#standard-token-types-and-modifiers
; Zed scopes: https://zed.dev/docs/extensions/languages#syntax-highlighting

; Fallback identifier - FIRST so more specific rules below override it
(identifier) @variable

; Strict keywords that cannot be identifiers (see parser/primitives.rs)
[
  "let"
  "pub"
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
  "disclose"
  "is"
  "yield"
  "resume"
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
(function_definition (identifier) @function.definition)
(fn_utxo_part (identifier) @function.definition)
(function_export "script" @keyword)
(function_parameter (identifier) @parameter)
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
(struct_constructor (scoped_name) @type)
(struct_field_initializer (identifier) @variable.member)
(struct_field_initializer ":" @operator)
(field_access "." @operator)
(scoped_name "::" @operator)
(arguments "(" @punctuation.bracket)
(arguments ")" @punctuation.bracket)
(arguments "," @operator)
(match_expression "match" @keyword)
(match_arm "=>" @operator)
(utxo_definition "utxo" @keyword)
(abi_impl_utxo_part "impl" @keyword)
(storage_utxo_part "storage" @keyword)
(fn_utxo_part "main" @keyword)
; ABI and events
(abi_definition (identifier) @type.definition)
(event_definition (identifier) @function.definition)
(event_definition "event" @keyword)
(effect_definition (identifier) @function.definition)
(effect_definition "effect" @keyword)

; Import definitions
(import_wit_source
  namespace: (identifier) @namespace)
(import_wit_source
  package: (identifier) @namespace)
(import_wit_source
  interface: (identifier) @namespace)
(import_wit_source ":" @operator)
(import_wit_source "/" @operator)
(import_path_source (string_literal) @string)
(import_named_item
  imported: (identifier) @function)
(import_named_item
  local: (identifier) @function)
(import_named_items "{" @punctuation.bracket)
(import_named_items "}" @punctuation.bracket)

; Contract marker
(contract_definition "contract" @keyword)

; Raise expression
(raise_expression "raise" @keyword)

; Runtime expression
(runtime_expression "runtime" @keyword)

; Disclose expression
(disclose_expression "disclose" @keyword)

; Is condition
(is_condition "is" @keyword)

; ABI method declaration
(abi_fn_declaration
  "fn" @keyword
  (identifier) @function.method)

(comment) @comment
(doc_comment) @comment.documentation
(shebang) @comment

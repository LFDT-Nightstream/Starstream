; VSCode scopes: https://code.visualstudio.com/api/language-extensions/semantic-highlight-guide#standard-token-types-and-modifiers
; Zed scopes: https://zed.dev/docs/extensions/languages#syntax-highlighting

; Keywords
"let" @keyword
"mut" @keyword
"if" @keyword
"else" @keyword
"while" @keyword
"fn" @keyword
"return" @keyword

; Literals
; Zed recognizes @boolean, but VSC doesn't, so set @keyword as well.
(boolean_literal) @keyword @boolean
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
(parameter (identifier) @parameter)
(parameter ":" @operator)
(variable_declaration (identifier) @variable.declaration)
(assignment (identifier) @variable.modification)

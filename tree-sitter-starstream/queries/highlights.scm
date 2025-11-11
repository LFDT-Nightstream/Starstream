; For scope names to use, see https://code.visualstudio.com/api/language-extensions/semantic-highlight-guide#standard-token-types-and-modifiers

; Keywords
"let" @keyword
"mut" @keyword
"if" @keyword
"else" @keyword
"while" @keyword
"fn" @keyword
"return" @keyword

; Literals
(boolean_literal) @keyword
(integer_literal) @number

; Actual syntax forms
(function_definition (identifier) @function)
(parameter (identifier) @parameter)
(parameter ":" @operator)
(type_annotation
  (identifier) @type
  "<" @operator
  ">" @operator
  "," @operator)
(variable_declaration (identifier) @variable.declaration)
(assignment (identifier) @variable.modification)
(identifier) @variable

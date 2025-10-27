; For scope names to use, see https://code.visualstudio.com/api/language-extensions/semantic-highlight-guide#standard-token-types-and-modifiers

; Keywords
"let" @keyword
"if" @keyword
"else" @keyword
"while" @keyword

; Literals
(boolean_literal) @keyword
(integer_literal) @number

; Actual syntax forms
(variable_declaration (identifier) @variable.declaration)
(assignment (identifier) @variable.modification)
(identifier) @variable

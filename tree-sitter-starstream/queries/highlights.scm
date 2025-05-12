(Type) @type

(ident) @variable

(FnDef (ident) @function)
("main") @function
(comment) @comment
(commentLine) @comment
((ident) (Arguments)) @function
(BindVar (ident) @variable)
(Type) @type
(Type (ident) @type)
(stringLiteral) @string
(bool) @constant.builtin.boolean
(number) @constant.numeric.integer

"fn" @keyword
"return" @keyword
"yield" @keyword.control
"resume" @keyword.control
"return" @keyword
"loop" @keyword
"while" @keyword
"if" @keyword
"else" @keyword
"let" @keyword
"try" @keyword
"impl" @keyword
"raise" @keyword.control
"with" @keyword
"error" @keyword
"utxo" @keyword
"script" @keyword
"storage" @keyword
"abi" @keyword
"bind" @keyword
"mint" @keyword
"unbind" @keyword
"effect" @keyword
"token" @keyword

"{" @punctuation.bracket
"}" @punctuation.bracket
"::" @punctuation.delimiter
"<" @punctuation.bracket
">" @punctuation.bracket

(InfixOp) @operator

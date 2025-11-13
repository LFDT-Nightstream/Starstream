; Outline entries for Starstream.

((function_definition
    "fn" @context
    (identifier) @name
    (_)*  ; parameters / return type
    (block
        "{" @open
        (_)*    ; statements / tail expr
        "}" @close)
) @item)

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

((struct_definition
    "struct" @context
    (identifier) @name
    (_)*  ; fields
    "{" @open
    (_)*  ; struct field entries
    "}" @close
) @item)

((enum_definition
    "enum" @context
    (identifier) @name
    (_)*  ; variants
    "{" @open
    (_)*  ; enum variant entries
    "}" @close
) @item)

((abi_definition
    "abi" @context
    (identifier) @name
    "{" @open
    (_)*  ; abi parts (events, functions)
    "}" @close
) @item)

((event_definition
    "event" @context
    (identifier) @name
) @item)

((import_definition
    "import" @context
    (_)*  ; import items
    "from" @context
    (import_source) @name
) @item)

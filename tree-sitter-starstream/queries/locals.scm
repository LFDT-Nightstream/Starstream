; locals.scm

[
(FnDef)
(Block)
(EffectBlock)
] @local.scope

(BindVar (ident) @local.definition)

(FnDef (ident) (TypedBindings (TypedBinding (ident) @local.definition)))

(Effect (Type (ident)) (TypedBindings (TypedBinding (ident) @local.definition)))

(ident) @local.reference

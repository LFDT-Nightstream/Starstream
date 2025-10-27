; locals.scm

[
(FnDef)
(Block)
(EffectBlock)
] @local.scope

((Var) @local.definition)

(FnDef (ident) (TypedBindings (TypedBinding (ident) @local.definition)))

(Effect (ident) (TypedBindings (TypedBinding (ident) @local.definition)))

(ident) @local.reference

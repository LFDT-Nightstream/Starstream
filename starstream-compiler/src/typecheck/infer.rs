#![allow(clippy::result_large_err)]

use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use starstream_types::{
    Scheme, Span, Spanned, Type, TypeVarId, TypedUtxoDef, TypedUtxoGlobal, TypedUtxoPart, UtxoDef,
    UtxoGlobal, UtxoPart,
    ast::{
        BinaryOp, Block, Definition, EnumConstructorPayload, EnumDef, EnumPatternPayload,
        EnumVariantPayload, Expr, FunctionDef, Identifier, Literal, Pattern, Program, Statement,
        StructDef, TypeAnnotation, UnaryOp,
    },
    typed_ast::{
        TypedBlock, TypedDefinition, TypedEnumConstructorPayload, TypedEnumDef,
        TypedEnumPatternPayload, TypedEnumVariant, TypedEnumVariantPayload, TypedExpr,
        TypedExprKind, TypedFunctionDef, TypedFunctionParam, TypedMatchArm, TypedPattern,
        TypedProgram, TypedStatement, TypedStructDef, TypedStructField, TypedStructLiteralField,
        TypedStructPatternField,
    },
    types::{
        EnumType, EnumVariantKind as TypeEnumVariantKind, EnumVariantType as TypeEnumVariant,
        RecordFieldType as TypeRecordField, RecordType,
    },
};

use super::{
    env::{Binding, TypeEnv},
    errors::{ConditionContext, EnumPayloadKind, TypeError, TypeErrorKind},
    tree::InferenceTree,
};
use crate::formatter;

/// Optional settings that control type-checker behavior.
#[derive(Clone, Debug, Default)]
pub struct TypecheckOptions {
    pub capture_traces: bool,
}

/// Successful type-checking result holding the typed AST and any inference traces.
#[derive(Clone, Debug)]
pub struct TypecheckSuccess {
    pub program: TypedProgram,
    pub traces: Vec<InferenceTree>,
}

impl TypecheckSuccess {
    pub fn display_traces(&self) -> impl Display {
        DisplayTraces(&self.traces)
    }
}

struct DisplayTraces<'a>(&'a [InferenceTree]);

impl<'a> Display for DisplayTraces<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (index, tree) in self.0.iter().enumerate() {
            if index > 0 {
                f.write_str("\n")?;
            }
            tree.fmt(f)?;
        }
        Ok(())
    }
}

struct TypeRegistry {
    entries: HashMap<String, TypeEntry>,
}

impl TypeRegistry {
    fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    fn insert(&mut self, name: String, entry: TypeEntry) {
        self.entries.insert(name, entry);
    }

    fn get(&self, name: &str) -> Option<&TypeEntry> {
        self.entries.get(name)
    }

    fn struct_info(&self, name: &str) -> Option<StructInfo> {
        self.entries.get(name).and_then(|entry| match &entry.kind {
            TypeEntryKind::Struct { fields } => Some(StructInfo {
                ty: entry.ty.clone(),
                fields: fields.clone(),
            }),
            _ => None,
        })
    }

    fn enum_info(&self, name: &str) -> Option<EnumInfo> {
        self.entries.get(name).and_then(|entry| match &entry.kind {
            TypeEntryKind::Enum { variants } => Some(EnumInfo {
                ty: entry.ty.clone(),
                variants: variants.clone(),
            }),
            _ => None,
        })
    }
}

struct TypeEntry {
    ty: Type,
    kind: TypeEntryKind,
    span: Span,
}

enum TypeEntryKind {
    Struct { fields: Vec<StructFieldInfo> },
    Enum { variants: Vec<EnumVariantInfo> },
}

#[derive(Clone)]
struct StructInfo {
    ty: Type,
    fields: Vec<StructFieldInfo>,
}

#[derive(Clone)]
struct StructFieldInfo {
    name: Identifier,
    ty: Type,
    span: Span,
}

#[derive(Clone)]
struct EnumInfo {
    ty: Type,
    variants: Vec<EnumVariantInfo>,
}

#[derive(Clone)]
struct EnumVariantInfo {
    name: Identifier,
    kind: EnumVariantInfoKind,
}

#[derive(Clone)]
enum EnumVariantInfoKind {
    Unit,
    Tuple(Vec<Type>),
    Struct(Vec<StructFieldInfo>),
}

/// Run Hindley–Milner style inference over the parsed program and return the
/// typed AST along with optional tracing information.
pub fn typecheck_program(
    program: &Program,
    options: TypecheckOptions,
) -> Result<TypecheckSuccess, Vec<TypeError>> {
    let mut inferencer = Inferencer::new(options.capture_traces);

    if let Err(error) = inferencer.register_type_definitions(&program.definitions) {
        return Err(vec![error]);
    }

    let mut typed_definitions = Vec::with_capacity(program.definitions.len());
    let mut definition_traces = Vec::with_capacity(program.definitions.len());
    let mut errors = Vec::new();

    let mut env = TypeEnv::new();
    for definition in &program.definitions {
        match inferencer.infer_definition(&mut env, definition) {
            Ok((typed_definition, trace)) => {
                typed_definitions.push(typed_definition);
                definition_traces.push(trace);
            }
            Err(error) => {
                errors.push(error);
                break;
            }
        }
    }

    if !errors.is_empty() {
        return Err(errors);
    }

    let mut typed_program = TypedProgram::new(typed_definitions);
    inferencer.apply_substitutions_program(&mut typed_program);

    let traces = if options.capture_traces {
        definition_traces
    } else {
        Vec::new()
    };

    Ok(TypecheckSuccess {
        program: typed_program,
        traces,
    })
}

/// Internal stateful helper that owns the substitution map and generates fresh
/// type variables while walking the AST.
struct Inferencer {
    capture_traces: bool,
    next_type_var: u32,
    subst: HashMap<TypeVarId, Type>,
    types: TypeRegistry,
    functions: FunctionRegistry,
}

struct FunctionRegistry {
    entries: HashMap<String, FunctionInfo>,
}

impl FunctionRegistry {
    fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    fn insert(&mut self, name: String, info: FunctionInfo) {
        self.entries.insert(name, info);
    }

    fn get(&self, name: &str) -> Option<&FunctionInfo> {
        self.entries.get(name)
    }
}

#[derive(Clone)]
struct FunctionInfo {
    param_types: Vec<Type>,
    param_spans: Vec<Span>,
    return_type: Type,
    name_span: Span,
}

struct FunctionCtx {
    expected_return: Type,
    return_span: Span,
    saw_return: bool,
}

impl Inferencer {
    /// Construct a fresh inferencer with an empty substitution environment.
    fn new(capture_traces: bool) -> Self {
        Self {
            capture_traces,
            next_type_var: 0,
            subst: HashMap::new(),
            types: TypeRegistry::new(),
            functions: FunctionRegistry::new(),
        }
    }

    fn register_type_definitions(&mut self, definitions: &[Definition]) -> Result<(), TypeError> {
        for definition in definitions {
            match definition {
                Definition::Struct(def) => self.register_struct(def)?,
                Definition::Enum(def) => self.register_enum(def)?,
                Definition::Function(_) => {}
                Definition::Utxo(_) => {}
            }
        }
        for definition in definitions {
            if let Definition::Function(def) = definition {
                self.register_function(def)?;
            }
        }
        Ok(())
    }

    fn register_function(&mut self, def: &FunctionDef) -> Result<(), TypeError> {
        let name = def.name.name.clone();
        let name_span = def.name.span.unwrap_or_else(dummy_span);

        if let Some(existing) = self.functions.get(&name) {
            return Err(TypeError::new(
                TypeErrorKind::FunctionAlreadyDefined { name },
                name_span,
            )
            .with_secondary(existing.name_span, "previously defined here"));
        }

        let mut param_types = Vec::with_capacity(def.params.len());
        let mut param_spans = Vec::with_capacity(def.params.len());
        for param in &def.params {
            let ty = self.type_from_annotation(&param.ty)?;
            param_types.push(ty);
            param_spans.push(param.ty.name.span.unwrap_or_else(dummy_span));
        }
        let return_type = match &def.return_type {
            Some(annotation) => self.type_from_annotation(annotation)?,
            None => Type::unit(),
        };
        self.functions.insert(
            name,
            FunctionInfo {
                param_types,
                param_spans,
                return_type,
                name_span,
            },
        );
        Ok(())
    }

    fn register_struct(&mut self, def: &StructDef) -> Result<(), TypeError> {
        let name = def.name.name.clone();
        if let Some(existing) = self.types.get(&name) {
            return Err(TypeError::new(
                TypeErrorKind::TypeAlreadyDefined { name },
                def.name.span.unwrap_or_else(dummy_span),
            )
            .with_secondary(existing.span, "previously defined here"));
        }

        let mut seen = HashMap::new();
        let mut fields = Vec::with_capacity(def.fields.len());
        for field in &def.fields {
            if let Some(previous_span) = seen.get(&field.name.name) {
                return Err(TypeError::new(
                    TypeErrorKind::DuplicateStructField {
                        struct_name: def.name.name.clone(),
                        field_name: field.name.name.clone(),
                    },
                    field.name.span.unwrap_or_else(dummy_span),
                )
                .with_primary_message("duplicate")
                .with_secondary(*previous_span, "first defined here"));
            }
            seen.insert(
                field.name.name.clone(),
                field.name.span.unwrap_or_else(dummy_span),
            );
            let ty = self.type_from_annotation(&field.ty)?;
            fields.push(StructFieldInfo {
                name: field.name.clone(),
                ty,
                span: field.name.span.unwrap_or_else(dummy_span),
            });
        }

        let type_fields = fields
            .iter()
            .map(|field| TypeRecordField::new(field.name.name.clone(), field.ty.clone()))
            .collect();
        let ty = Type::record(def.name.name.clone(), type_fields);
        self.types.insert(
            def.name.name.clone(),
            TypeEntry {
                ty,
                kind: TypeEntryKind::Struct { fields },
                span: def.name.span.unwrap_or_else(dummy_span),
            },
        );
        Ok(())
    }

    fn register_enum(&mut self, def: &EnumDef) -> Result<(), TypeError> {
        let name = def.name.name.clone();
        if let Some(existing) = self.types.get(&name) {
            return Err(TypeError::new(
                TypeErrorKind::TypeAlreadyDefined { name },
                def.name.span.unwrap_or_else(dummy_span),
            )
            .with_secondary(existing.span, "previously defined here"));
        }

        let mut seen = HashMap::new();
        let mut variants = Vec::with_capacity(def.variants.len());
        for variant in &def.variants {
            if let Some(previous_span) = seen.get(&variant.name.name) {
                return Err(TypeError::new(
                    TypeErrorKind::DuplicateEnumVariant {
                        enum_name: def.name.name.clone(),
                        variant_name: variant.name.name.clone(),
                    },
                    variant.name.span.unwrap_or_else(dummy_span),
                )
                .with_primary_message("duplicate")
                .with_secondary(*previous_span, "first defined here"));
            }
            seen.insert(
                variant.name.name.clone(),
                variant.name.span.unwrap_or_else(dummy_span),
            );
            let kind = match &variant.payload {
                EnumVariantPayload::Unit => EnumVariantInfoKind::Unit,
                EnumVariantPayload::Tuple(items) => {
                    let mut payload = Vec::with_capacity(items.len());
                    for ty in items {
                        payload.push(self.type_from_annotation(ty)?);
                    }
                    EnumVariantInfoKind::Tuple(payload)
                }
                EnumVariantPayload::Struct(fields) => {
                    let mut seen_fields = HashMap::new();
                    let mut payload = Vec::with_capacity(fields.len());
                    for field in fields {
                        if let Some(previous_span) = seen_fields.get(&field.name.name) {
                            return Err(TypeError::new(
                                TypeErrorKind::DuplicateStructField {
                                    struct_name: format!(
                                        "{}::{}",
                                        def.name.name, variant.name.name
                                    ),
                                    field_name: field.name.name.clone(),
                                },
                                field.name.span.unwrap_or_else(dummy_span),
                            )
                            .with_primary_message("duplicate")
                            .with_secondary(*previous_span, "first defined here"));
                        }
                        seen_fields.insert(
                            field.name.name.clone(),
                            field.name.span.unwrap_or_else(dummy_span),
                        );
                        let ty = self.type_from_annotation(&field.ty)?;
                        payload.push(StructFieldInfo {
                            name: field.name.clone(),
                            ty,
                            span: field.name.span.unwrap_or_else(dummy_span),
                        });
                    }
                    EnumVariantInfoKind::Struct(payload)
                }
            };

            variants.push(EnumVariantInfo {
                name: variant.name.clone(),
                kind,
            });
        }

        let type_variants = variants
            .iter()
            .map(|variant| match &variant.kind {
                EnumVariantInfoKind::Unit => TypeEnumVariant::unit(variant.name.name.clone()),
                EnumVariantInfoKind::Tuple(payload) => {
                    TypeEnumVariant::tuple(variant.name.name.clone(), payload.clone())
                }
                EnumVariantInfoKind::Struct(fields) => TypeEnumVariant::struct_variant(
                    variant.name.name.clone(),
                    fields
                        .iter()
                        .map(|field| {
                            TypeRecordField::new(field.name.name.clone(), field.ty.clone())
                        })
                        .collect(),
                ),
            })
            .collect();
        let ty = Type::enum_type(def.name.name.clone(), type_variants);
        self.types.insert(
            def.name.name.clone(),
            TypeEntry {
                ty,
                kind: TypeEntryKind::Enum { variants },
                span: def.name.span.unwrap_or_else(dummy_span),
            },
        );
        Ok(())
    }

    fn build_typed_struct(&self, def: &StructDef) -> Result<TypedStructDef, TypeError> {
        let info = self.types.struct_info(&def.name.name).ok_or_else(|| {
            TypeError::new(
                TypeErrorKind::UnknownStruct {
                    name: def.name.name.clone(),
                },
                def.name.span.unwrap_or_else(dummy_span),
            )
        })?;

        let fields = info
            .fields
            .iter()
            .map(|field| TypedStructField {
                name: field.name.clone(),
                ty: field.ty.clone(),
            })
            .collect();

        Ok(TypedStructDef {
            name: def.name.clone(),
            fields,
            ty: info.ty,
        })
    }

    fn build_typed_enum(&self, def: &EnumDef) -> Result<TypedEnumDef, TypeError> {
        let info = self.types.enum_info(&def.name.name).ok_or_else(|| {
            TypeError::new(
                TypeErrorKind::UnknownEnum {
                    name: def.name.name.clone(),
                },
                def.name.span.unwrap_or_else(dummy_span),
            )
        })?;

        let variants = info
            .variants
            .iter()
            .map(|variant| TypedEnumVariant {
                name: variant.name.clone(),
                payload: match &variant.kind {
                    EnumVariantInfoKind::Unit => TypedEnumVariantPayload::Unit,
                    EnumVariantInfoKind::Tuple(payload) => {
                        TypedEnumVariantPayload::Tuple(payload.clone())
                    }
                    EnumVariantInfoKind::Struct(fields) => TypedEnumVariantPayload::Struct(
                        fields
                            .iter()
                            .map(|field| TypedStructField {
                                name: field.name.clone(),
                                ty: field.ty.clone(),
                            })
                            .collect(),
                    ),
                },
            })
            .collect();

        Ok(TypedEnumDef {
            name: def.name.clone(),
            variants,
            ty: info.ty,
        })
    }

    fn infer_utxo(
        &mut self,
        env: &mut TypeEnv,
        def: &UtxoDef,
    ) -> Result<(TypedUtxoDef, InferenceTree), TypeError> {
        let parts = def
            .parts
            .iter()
            .map(|part| -> Result<TypedUtxoPart, TypeError> {
                Ok(match part {
                    UtxoPart::Storage(vars) => TypedUtxoPart::Storage(
                        vars.iter()
                            .map(|var| self.infer_utxo_global(env, var))
                            .collect::<Result<Vec<_>, _>>()?,
                    ),
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok((
            TypedUtxoDef {
                name: def.name.clone(),
                parts,
            },
            // ... is this needed?
            InferenceTree::default(),
        ))
    }

    fn infer_utxo_global(
        &mut self,
        env: &mut TypeEnv,
        var: &UtxoGlobal,
    ) -> Result<TypedUtxoGlobal, TypeError> {
        let ty = self.type_from_annotation(&var.ty)?;
        env.insert(
            var.name.name.clone(),
            Binding {
                decl_span: var.name.span.unwrap_or_else(dummy_span),
                mutable: true,
                scheme: Scheme::monomorphic(ty.clone()),
            },
        );
        Ok(TypedUtxoGlobal {
            name: var.name.clone(),
            ty,
        })
    }

    fn lookup_struct_info(&self, name: &Identifier) -> Result<StructInfo, TypeError> {
        self.types.struct_info(&name.name).ok_or_else(|| {
            TypeError::new(
                TypeErrorKind::UnknownStruct {
                    name: name.name.clone(),
                },
                name.span.unwrap_or_else(dummy_span),
            )
        })
    }

    fn lookup_enum_info(&self, name: &Identifier) -> Result<EnumInfo, TypeError> {
        self.types.enum_info(&name.name).ok_or_else(|| {
            TypeError::new(
                TypeErrorKind::UnknownEnum {
                    name: name.name.clone(),
                },
                name.span.unwrap_or_else(dummy_span),
            )
        })
    }

    fn bind_pattern_identifier(
        &mut self,
        env: &mut TypeEnv,
        ident: &Identifier,
        ty: Type,
    ) -> Result<(), TypeError> {
        if env.get_in_current_scope(&ident.name).is_some() {
            return Err(TypeError::new(
                TypeErrorKind::Redeclaration {
                    name: ident.name.clone(),
                },
                ident.span.unwrap_or_else(dummy_span),
            ));
        }

        env.insert(
            ident.name.clone(),
            Binding {
                decl_span: ident.span.unwrap_or_else(dummy_span),
                mutable: false,
                scheme: Scheme::monomorphic(ty),
            },
        );
        Ok(())
    }

    fn infer_pattern(
        &mut self,
        env: &mut TypeEnv,
        pattern: &Pattern,
        expected_ty: Type,
        value_span: Span,
    ) -> Result<(TypedPattern, Vec<InferenceTree>), TypeError> {
        match pattern {
            Pattern::Binding(ident) => {
                self.bind_pattern_identifier(env, ident, expected_ty.clone())?;
                Ok((TypedPattern::Binding(ident.clone()), Vec::new()))
            }
            Pattern::Wildcard { .. } => {
                // Wildcard matches anything but doesn't introduce a binding
                Ok((TypedPattern::Wildcard, Vec::new()))
            }
            Pattern::Literal { value, span } => {
                // Literal patterns must match the expected type
                let literal_ty = match value {
                    Literal::Integer(_) => Type::Int,
                    Literal::Boolean(_) => Type::Bool,
                    Literal::Unit => Type::Unit,
                };
                let (.., unify_trace) = self.unify(
                    expected_ty.clone(),
                    literal_ty.clone(),
                    value_span,
                    *span,
                    TypeErrorKind::GeneralMismatch {
                        expected: self.apply(&expected_ty),
                        found: literal_ty.clone(),
                    },
                )?;
                Ok((TypedPattern::Literal(value.clone()), vec![unify_trace]))
            }
            Pattern::EnumVariant {
                enum_name,
                variant,
                payload,
            } => {
                let info = self.lookup_enum_info(enum_name)?;
                let (.., unify_trace) = self.unify(
                    expected_ty.clone(),
                    info.ty.clone(),
                    value_span,
                    enum_name.span.unwrap_or_else(dummy_span),
                    TypeErrorKind::PatternEnumMismatch {
                        enum_name: enum_name.name.clone(),
                        found: self.apply(&expected_ty),
                    },
                )?;
                let mut traces = vec![unify_trace];

                let variant_info = info
                    .variants
                    .iter()
                    .find(|v| v.name.name == variant.name)
                    .ok_or_else(|| {
                        TypeError::new(
                            TypeErrorKind::UnknownEnumVariant {
                                enum_name: enum_name.name.clone(),
                                variant_name: variant.name.clone(),
                            },
                            variant.span.unwrap_or_else(dummy_span),
                        )
                    })?;

                let typed_payload = match (payload, &variant_info.kind) {
                    (EnumPatternPayload::Unit, EnumVariantInfoKind::Unit) => {
                        TypedEnumPatternPayload::Unit
                    }
                    (EnumPatternPayload::Tuple(patterns), EnumVariantInfoKind::Tuple(expected)) => {
                        if patterns.len() != expected.len() {
                            return Err(TypeError::new(
                                TypeErrorKind::EnumPayloadMismatch {
                                    enum_name: enum_name.name.clone(),
                                    variant_name: variant.name.clone(),
                                    expected: EnumPayloadKind::tuple(expected.len()),
                                    found: EnumPayloadKind::tuple(patterns.len()),
                                },
                                variant.span.unwrap_or_else(dummy_span),
                            ));
                        }

                        let mut typed = Vec::with_capacity(patterns.len());
                        for (pattern, ty) in patterns.iter().zip(expected.iter()) {
                            let (typed_pattern, mut pattern_traces) =
                                self.infer_pattern(env, pattern, ty.clone(), value_span)?;
                            traces.append(&mut pattern_traces);
                            typed.push(typed_pattern);
                        }
                        TypedEnumPatternPayload::Tuple(typed)
                    }
                    (EnumPatternPayload::Struct(fields), EnumVariantInfoKind::Struct(expected)) => {
                        let mut expected_fields = expected
                            .iter()
                            .map(|field| (field.name.name.clone(), field.clone()))
                            .collect::<HashMap<_, _>>();
                        let mut seen = HashMap::new();
                        let mut typed_fields = Vec::with_capacity(fields.len());
                        let struct_name =
                            format!("{}::{}", enum_name.name.clone(), variant.name.clone());

                        for field in fields {
                            if let Some(previous_span) = seen.get(&field.name.name) {
                                return Err(TypeError::new(
                                    TypeErrorKind::DuplicateStructLiteralField {
                                        field_name: field.name.name.clone(),
                                    },
                                    field.name.span.unwrap_or_else(dummy_span),
                                )
                                .with_primary_message("duplicate")
                                .with_secondary(*previous_span, "first used here"));
                            }
                            seen.insert(
                                field.name.name.clone(),
                                field.name.span.unwrap_or_else(dummy_span),
                            );

                            let expected_field =
                                expected_fields.remove(&field.name.name).ok_or_else(|| {
                                    TypeError::new(
                                        TypeErrorKind::UnknownStructField {
                                            struct_name: struct_name.clone(),
                                            field_name: field.name.name.clone(),
                                        },
                                        field.name.span.unwrap_or_else(dummy_span),
                                    )
                                })?;

                            let (typed_pattern, mut pattern_traces) = self.infer_pattern(
                                env,
                                &field.pattern,
                                expected_field.ty.clone(),
                                value_span,
                            )?;
                            traces.append(&mut pattern_traces);
                            typed_fields.push(TypedStructPatternField {
                                name: field.name.clone(),
                                pattern: Box::new(typed_pattern),
                            });
                        }

                        if let Some((missing_field, _)) = expected_fields.into_iter().next() {
                            return Err(TypeError::new(
                                TypeErrorKind::MissingStructField {
                                    struct_name,
                                    field_name: missing_field,
                                },
                                variant.span.unwrap_or_else(dummy_span),
                            ));
                        }

                        TypedEnumPatternPayload::Struct(typed_fields)
                    }
                    (found_payload, expected_kind) => {
                        return Err(TypeError::new(
                            TypeErrorKind::EnumPayloadMismatch {
                                enum_name: enum_name.name.clone(),
                                variant_name: variant.name.clone(),
                                expected: enum_payload_kind_from_variant(expected_kind),
                                found: enum_payload_kind_from_pattern(found_payload),
                            },
                            variant.span.unwrap_or_else(dummy_span),
                        ));
                    }
                };

                Ok((
                    TypedPattern::EnumVariant {
                        enum_name: enum_name.clone(),
                        variant: variant.clone(),
                        payload: typed_payload,
                    },
                    traces,
                ))
            }
            Pattern::Struct { name, fields } => {
                let info = self.lookup_struct_info(name)?;
                let (.., unify_trace) = self.unify(
                    expected_ty.clone(),
                    info.ty.clone(),
                    value_span,
                    name.span.unwrap_or_else(dummy_span),
                    TypeErrorKind::GeneralMismatch {
                        expected: info.ty.clone(),
                        found: expected_ty.clone(),
                    },
                )?;
                let mut traces = vec![unify_trace];

                let mut expected_fields = info
                    .fields
                    .iter()
                    .map(|field| (field.name.name.clone(), field.clone()))
                    .collect::<HashMap<_, _>>();

                let mut typed_fields = Vec::with_capacity(fields.len());
                let mut seen = HashMap::new();
                for field in fields {
                    if let Some(previous_span) = seen.get(&field.name.name) {
                        return Err(TypeError::new(
                            TypeErrorKind::DuplicateStructLiteralField {
                                field_name: field.name.name.clone(),
                            },
                            field.name.span.unwrap_or_else(dummy_span),
                        )
                        .with_primary_message("duplicate")
                        .with_secondary(*previous_span, "first used here"));
                    }
                    seen.insert(
                        field.name.name.clone(),
                        field.name.span.unwrap_or_else(dummy_span),
                    );

                    let expected_field =
                        expected_fields.remove(&field.name.name).ok_or_else(|| {
                            TypeError::new(
                                TypeErrorKind::UnknownStructField {
                                    struct_name: name.name.clone(),
                                    field_name: field.name.name.clone(),
                                },
                                field.name.span.unwrap_or_else(dummy_span),
                            )
                        })?;

                    let (typed_pattern, mut pattern_traces) = self.infer_pattern(
                        env,
                        &field.pattern,
                        expected_field.ty.clone(),
                        value_span,
                    )?;
                    traces.append(&mut pattern_traces);
                    typed_fields.push(TypedStructPatternField {
                        name: field.name.clone(),
                        pattern: Box::new(typed_pattern),
                    });
                }

                if let Some((field_name, _)) = expected_fields.into_iter().next() {
                    return Err(TypeError::new(
                        TypeErrorKind::MissingStructField {
                            struct_name: name.name.clone(),
                            field_name,
                        },
                        name.span.unwrap_or_else(dummy_span),
                    ));
                }

                Ok((
                    TypedPattern::Struct {
                        name: name.clone(),
                        fields: typed_fields,
                    },
                    traces,
                ))
            }
        }
    }

    /// Type-check a top-level definition.
    fn infer_definition(
        &mut self,
        env: &mut TypeEnv,
        definition: &Definition,
    ) -> Result<(TypedDefinition, InferenceTree), TypeError> {
        match definition {
            Definition::Function(function) => {
                let (typed_function, trace) = self.infer_function(env, function)?;
                Ok((TypedDefinition::Function(typed_function), trace))
            }
            Definition::Struct(def) => {
                let typed = self.build_typed_struct(def)?;
                Ok((TypedDefinition::Struct(typed), InferenceTree::default()))
            }
            Definition::Enum(def) => {
                let typed = self.build_typed_enum(def)?;
                Ok((TypedDefinition::Enum(typed), InferenceTree::default()))
            }
            Definition::Utxo(def) => {
                let (utxo, trace) = self.infer_utxo(env, def)?;
                Ok((TypedDefinition::Utxo(utxo), trace))
            }
        }
    }

    fn infer_function(
        &mut self,
        env: &mut TypeEnv,
        function: &FunctionDef,
    ) -> Result<(TypedFunctionDef, InferenceTree), TypeError> {
        env.push_scope();
        let mut typed_params = Vec::with_capacity(function.params.len());
        for param in &function.params {
            let ty = self.type_from_annotation(&param.ty)?;
            env.insert(
                param.name.name.clone(),
                Binding {
                    decl_span: param
                        .name
                        .span
                        .or(function.name.span)
                        .unwrap_or_else(dummy_span),
                    mutable: false,
                    scheme: Scheme::monomorphic(ty.clone()),
                },
            );
            typed_params.push(TypedFunctionParam {
                name: param.name.clone(),
                ty,
            });
        }

        let (expected_return, return_span) = match &function.return_type {
            Some(annotation) => (
                self.type_from_annotation(annotation)?,
                annotation
                    .name
                    .span
                    .or(function.name.span)
                    .unwrap_or_else(dummy_span),
            ),
            None => (Type::unit(), function.name.span.unwrap_or_else(dummy_span)),
        };

        let mut ctx = FunctionCtx {
            expected_return: expected_return.clone(),
            return_span,
            saw_return: false,
        };

        let (typed_body, body_traces) = self.infer_block(env, &function.body, &mut ctx, true)?;

        env.pop_scope();

        if expected_return != Type::unit()
            && !ctx.saw_return
            && typed_body.tail_expression.is_none()
        {
            return Err(TypeError::new(
                TypeErrorKind::MissingReturn {
                    expected: expected_return,
                },
                function.name.span.unwrap_or(return_span),
            )
            .with_help("add a `return` or tail expression to satisfy the signature"));
        }

        let subject = self.maybe_string(|| function.name.name.clone());
        let result = self.maybe_string(|| self.format_type(&ctx.expected_return));
        let trace = self.make_trace("T-Fn", None, subject, result, || body_traces);

        Ok((
            TypedFunctionDef {
                export: function.export.clone(),
                name: function.name.clone(),
                params: typed_params,
                return_type: ctx.expected_return,
                body: typed_body,
            },
            trace,
        ))
    }

    /// Type-check a single statement, yielding its typed form and an inference trace.
    fn infer_statement(
        &mut self,
        env: &mut TypeEnv,
        statement: &Statement,
        ctx: &mut FunctionCtx,
    ) -> Result<(TypedStatement, InferenceTree), TypeError> {
        let env_context = self.maybe_string(|| self.format_env(env));
        let stmt_repr = self.maybe_string(|| self.format_statement_src(statement));
        match statement {
            Statement::VariableDeclaration {
                mutable,
                name,
                ty,
                value,
            } => {
                if let Some(previous_decl) = env.get_in_current_scope(&name.name) {
                    return Err(TypeError::new(
                        TypeErrorKind::Redeclaration {
                            name: name.name.clone(),
                        },
                        name.span.unwrap_or(value.span),
                    )
                    .with_secondary(previous_decl.decl_span, "previously defined here"));
                }

                let (typed_value, value_trace) = self.infer_expr(env, value, ctx)?;
                let mut children = vec![value_trace];
                let mut value_type = self.apply(&typed_value.node.ty);

                if let Some(ty) = ty {
                    // Binding has a type annotation, so unify it with the initial value.
                    let expected_type = self.type_from_annotation(ty)?;
                    let (new_value_type, unify_trace) = self.unify(
                        expected_type.clone(),
                        value_type.clone(),
                        name.span.unwrap_or(value.span),
                        value.span,
                        TypeErrorKind::AssignmentMismatch {
                            name: name.name.clone(),
                            expected: self.apply(&expected_type),
                            found: self.apply(&value_type),
                        },
                    )?;
                    value_type = new_value_type;
                    children.push(unify_trace);
                }

                let scheme = self.generalize(env, &value_type);
                env.insert(
                    name.name.clone(),
                    Binding {
                        decl_span: name.span.unwrap_or(value.span),
                        mutable: *mutable,
                        scheme,
                    },
                );

                let value_type_repr = self.maybe_string(|| self.format_type(&value_type));
                let tree = self.make_trace(
                    "T-Let",
                    env_context.clone(),
                    stmt_repr.clone(),
                    value_type_repr,
                    || children,
                );

                Ok((
                    TypedStatement::VariableDeclaration {
                        mutable: *mutable,
                        name: name.clone(),
                        value: typed_value,
                    },
                    tree,
                ))
            }
            Statement::Assignment { target, value } => {
                let binding = env.get(&target.name).cloned().ok_or_else(|| {
                    TypeError::new(
                        TypeErrorKind::UnknownVariable {
                            name: target.name.clone(),
                        },
                        target.span.unwrap_or(value.span),
                    )
                })?;

                if !binding.mutable {
                    return Err(TypeError::new(
                        TypeErrorKind::AssignmentToImmutable {
                            name: target.name.clone(),
                        },
                        target.span.unwrap_or(value.span),
                    )
                    .with_primary_message("assigned here")
                    .with_secondary(binding.decl_span, "declared without `mut` here")
                    .with_help("consider changing `let` to `let mut`"));
                }

                let expected_type = self.instantiate(&binding.scheme);
                let (typed_value, value_trace) = self.infer_expr(env, value, ctx)?;
                let actual_type = typed_value.node.ty.clone();

                let (_, unify_trace) = self.unify(
                    actual_type.clone(),
                    expected_type.clone(),
                    value.span,
                    target.span.unwrap_or(value.span),
                    TypeErrorKind::AssignmentMismatch {
                        name: target.name.clone(),
                        expected: self.apply(&expected_type),
                        found: self.apply(&actual_type),
                    },
                )?;

                let expected_repr = self.maybe_string(|| self.format_type(&expected_type));
                let tree = self.make_trace(
                    "T-Assign",
                    env_context.clone(),
                    stmt_repr.clone(),
                    expected_repr,
                    || vec![value_trace, unify_trace],
                );

                Ok((
                    TypedStatement::Assignment {
                        target: target.clone(),
                        value: typed_value,
                    },
                    tree,
                ))
            }
            Statement::Return(value) => {
                let result_repr = self.maybe_string(|| self.format_type(&ctx.expected_return));
                match value {
                    Some(expr) => {
                        let (typed_expr, expr_trace) = self.infer_expr(env, expr, ctx)?;
                        let actual_type = typed_expr.node.ty.clone();
                        let (_, unify_trace) = self.unify(
                            actual_type.clone(),
                            ctx.expected_return.clone(),
                            expr.span,
                            ctx.return_span,
                            TypeErrorKind::ReturnMismatch {
                                expected: self.apply(&ctx.expected_return),
                                found: self.apply(&actual_type),
                            },
                        )?;
                        ctx.saw_return = true;
                        let tree = self.make_trace(
                            "T-Return",
                            env_context,
                            stmt_repr,
                            result_repr,
                            || vec![expr_trace, unify_trace],
                        );
                        Ok((TypedStatement::Return(Some(typed_expr)), tree))
                    }
                    None => {
                        let unit = Type::unit();
                        let (_, unify_trace) = self.unify(
                            unit.clone(),
                            ctx.expected_return.clone(),
                            ctx.return_span,
                            ctx.return_span,
                            TypeErrorKind::ReturnMismatch {
                                expected: self.apply(&ctx.expected_return),
                                found: self.apply(&unit),
                            },
                        )?;
                        ctx.saw_return = true;
                        let tree = self.make_trace(
                            "T-ReturnUnit",
                            env_context,
                            stmt_repr,
                            result_repr,
                            || vec![unify_trace],
                        );
                        Ok((TypedStatement::Return(None), tree))
                    }
                }
            }
            Statement::While { condition, body } => {
                let (typed_condition, cond_trace) = self.infer_expr(env, condition, ctx)?;
                let bool_check = self.require_bool(
                    &typed_condition.node.ty,
                    condition.span,
                    ConditionContext::While,
                )?;

                let (typed_body, body_traces) = self.infer_block(env, body, ctx, false)?;

                let mut children = vec![cond_trace, bool_check];
                children.extend(body_traces);

                let unit_result = self.maybe_string(|| "()".to_string());
                let tree = self.make_trace(
                    "T-While",
                    env_context.clone(),
                    stmt_repr.clone(),
                    unit_result,
                    || children,
                );

                Ok((
                    TypedStatement::While {
                        condition: typed_condition,
                        body: typed_body,
                    },
                    tree,
                ))
            }
            Statement::Expression(expr) => {
                let (typed_expr, expr_trace) = self.infer_expr(env, expr, ctx)?;
                let unit_result = self.maybe_string(|| "()".to_string());
                let tree = self.make_trace("T-Expr", env_context, stmt_repr, unit_result, || {
                    vec![expr_trace]
                });
                Ok((TypedStatement::Expression(typed_expr), tree))
            }
        }
    }

    /// Type-check a block, returning typed statements plus per-statement traces.
    fn infer_block(
        &mut self,
        env: &mut TypeEnv,
        block: &Block,
        ctx: &mut FunctionCtx,
        treat_tail_as_return: bool,
    ) -> Result<(TypedBlock, Vec<InferenceTree>), TypeError> {
        env.push_scope();
        let mut typed_statements = Vec::with_capacity(block.statements.len());
        let mut traces = Vec::with_capacity(block.statements.len() + 1);
        for statement in &block.statements {
            let (typed, trace) = self.infer_statement(env, statement, ctx)?;
            typed_statements.push(typed);
            traces.push(trace);
        }

        let mut tail_expression = None;
        if let Some(expr) = &block.tail_expression {
            let (typed_expr, expr_trace) = self.infer_expr(env, expr, ctx)?;
            let mut children = vec![expr_trace];
            if treat_tail_as_return {
                ctx.saw_return = true;
                let actual = typed_expr.node.ty.clone();
                let (_, unify_trace) = self.unify(
                    actual.clone(),
                    ctx.expected_return.clone(),
                    expr.span,
                    ctx.return_span,
                    TypeErrorKind::ReturnMismatch {
                        expected: self.apply(&ctx.expected_return),
                        found: self.apply(&actual),
                    },
                )?;
                children.push(unify_trace);
            }

            let label = if treat_tail_as_return {
                "T-ReturnTail"
            } else {
                "T-Tail"
            };
            let subject = self.maybe_string(|| self.format_expr_src(expr));
            let result = self.maybe_string(|| self.format_type(&typed_expr.node.ty));
            let tail_trace = self.make_trace(label, None, subject, result, || children);
            traces.push(tail_trace);
            tail_expression = Some(typed_expr);
        }

        env.pop_scope();
        Ok((TypedBlock::new(typed_statements, tail_expression), traces))
    }

    /// Type-check an expression, returning the typed node and corresponding trace tree.
    fn infer_expr(
        &mut self,
        env: &mut TypeEnv,
        expr: &Spanned<Expr>,
        ctx: &mut FunctionCtx,
    ) -> Result<(Spanned<TypedExpr>, InferenceTree), TypeError> {
        let env_context = self.maybe_string(|| self.format_env(env));
        let subject_repr = self.maybe_string(|| self.format_expr_src(expr));
        match &expr.node {
            Expr::Literal(lit) => {
                let (ty, kind, rule) = match lit {
                    Literal::Integer(value) => (
                        Type::int(),
                        TypedExprKind::Literal(Literal::Integer(*value)),
                        "T-Int",
                    ),
                    Literal::Boolean(value) => (
                        Type::bool(),
                        TypedExprKind::Literal(Literal::Boolean(*value)),
                        "T-Bool",
                    ),
                    Literal::Unit => (
                        Type::unit(),
                        TypedExprKind::Literal(Literal::Unit),
                        "T-Unit",
                    ),
                };
                let typed = Spanned::new(TypedExpr::new(ty.clone(), kind), expr.span);
                let result_repr = self.maybe_string(|| self.format_type(&ty));
                let tree = self.make_trace(
                    rule,
                    env_context.clone(),
                    subject_repr.clone(),
                    result_repr,
                    Vec::new,
                );
                Ok((typed, tree))
            }
            Expr::Identifier(Identifier { name, span }) => {
                let ty = if let Some(binding) = env.get(name).cloned() {
                    self.instantiate(&binding.scheme)
                } else if let Some(func_info) = self.functions.get(name) {
                    Type::Function(
                        func_info.param_types.clone(),
                        Box::new(func_info.return_type.clone()),
                    )
                } else {
                    let span = span.unwrap_or(expr.span);
                    return Err(TypeError::new(
                        TypeErrorKind::UnknownVariable { name: name.clone() },
                        span,
                    ));
                };
                let typed = Spanned::new(
                    TypedExpr::new(
                        ty.clone(),
                        TypedExprKind::Identifier(Identifier {
                            name: name.clone(),
                            span: *span,
                        }),
                    ),
                    expr.span,
                );
                let result_repr = self.maybe_string(|| self.format_type(&ty));
                let tree = self.make_trace(
                    "T-Var",
                    env_context.clone(),
                    subject_repr.clone(),
                    result_repr,
                    Vec::new,
                );
                Ok((typed, tree))
            }
            Expr::Unary { op, expr: inner } => {
                let (typed_inner, inner_trace) = self.infer_expr(env, inner, ctx)?;
                let check = match op {
                    UnaryOp::Negate => self.require_is(
                        &typed_inner.node.ty,
                        Type::int(),
                        inner.span,
                        inner.span,
                        TypeErrorKind::UnaryMismatch {
                            op: *op,
                            expected: Type::int(),
                            found: self.apply(&typed_inner.node.ty),
                        },
                    )?,
                    UnaryOp::Not => self.require_is(
                        &typed_inner.node.ty,
                        Type::bool(),
                        inner.span,
                        inner.span,
                        TypeErrorKind::UnaryMismatch {
                            op: *op,
                            expected: Type::bool(),
                            found: self.apply(&typed_inner.node.ty),
                        },
                    )?,
                };

                let typed = Spanned::new(
                    TypedExpr::new(
                        typed_inner.node.ty.clone(),
                        TypedExprKind::Unary {
                            op: *op,
                            expr: Box::new(typed_inner.clone()),
                        },
                    ),
                    expr.span,
                );

                let rule = match op {
                    UnaryOp::Negate => "T-Unary-Neg",
                    UnaryOp::Not => "T-Unary-Not",
                };

                let result_repr = self.maybe_string(|| self.format_type(&typed.node.ty));
                let tree = self.make_trace(
                    rule,
                    env_context.clone(),
                    subject_repr.clone(),
                    result_repr,
                    || vec![inner_trace, check],
                );

                Ok((typed, tree))
            }
            Expr::Binary { op, left, right } => {
                let (typed_left, left_trace) = self.infer_expr(env, left, ctx)?;
                let (typed_right, right_trace) = self.infer_expr(env, right, ctx)?;
                let left_ty = self.apply(&typed_left.node.ty);
                let right_ty = self.apply(&typed_right.node.ty);
                let left_label_span = self.label_span_for_expr(left);
                let right_label_span = self.label_span_for_expr(right);

                let mut children = vec![left_trace, right_trace];
                let expr_type = match op {
                    BinaryOp::Add
                    | BinaryOp::Subtract
                    | BinaryOp::Multiply
                    | BinaryOp::Divide
                    | BinaryOp::Remainder => {
                        let both_int =
                            matches!(&left_ty, Type::Int) && matches!(&right_ty, Type::Int);
                        if !both_int {
                            let left_repr = self.format_type(&left_ty);
                            let right_repr = self.format_type(&right_ty);
                            return Err(TypeError::new(
                                TypeErrorKind::BinaryOperandMismatch {
                                    op: *op,
                                    left: left_ty.clone(),
                                    right: right_ty.clone(),
                                },
                                left_label_span,
                            )
                            .with_primary_message(format!("has type `{left_repr}`"))
                            .with_secondary(right_label_span, format!("has type `{right_repr}`")));
                        }

                        children.push(self.require_is(
                            &typed_left.node.ty,
                            Type::int(),
                            left_label_span,
                            left_label_span,
                            TypeErrorKind::BinaryOperandMismatch {
                                op: *op,
                                left: self.apply(&typed_left.node.ty),
                                right: self.apply(&typed_right.node.ty),
                            },
                        )?);
                        children.push(self.require_is(
                            &typed_right.node.ty,
                            Type::int(),
                            right_label_span,
                            right_label_span,
                            TypeErrorKind::BinaryOperandMismatch {
                                op: *op,
                                left: self.apply(&typed_left.node.ty),
                                right: self.apply(&typed_right.node.ty),
                            },
                        )?);
                        Type::int()
                    }
                    BinaryOp::Less
                    | BinaryOp::LessEqual
                    | BinaryOp::Greater
                    | BinaryOp::GreaterEqual => {
                        let comparison = self.require_numeric_or_bool_pair(
                            op,
                            &typed_left,
                            left_label_span,
                            &typed_right,
                            right_label_span,
                        )?;
                        children.push(comparison);
                        Type::bool()
                    }
                    BinaryOp::Equal | BinaryOp::NotEqual => {
                        let check = self.require_same_primitive(
                            op,
                            &typed_left,
                            left_label_span,
                            &typed_right,
                            right_label_span,
                        )?;
                        children.push(check);
                        Type::bool()
                    }
                    BinaryOp::And | BinaryOp::Or => {
                        let both_bool =
                            matches!(&left_ty, Type::Bool) && matches!(&right_ty, Type::Bool);
                        if !both_bool {
                            let left_repr = self.format_type(&left_ty);
                            let right_repr = self.format_type(&right_ty);
                            return Err(TypeError::new(
                                TypeErrorKind::BinaryOperandMismatch {
                                    op: *op,
                                    left: left_ty.clone(),
                                    right: right_ty.clone(),
                                },
                                left_label_span,
                            )
                            .with_primary_message(format!("has type `{left_repr}`"))
                            .with_secondary(right_label_span, format!("has type `{right_repr}`")));
                        }

                        children.push(self.require_is(
                            &typed_left.node.ty,
                            Type::bool(),
                            left_label_span,
                            left_label_span,
                            TypeErrorKind::BinaryOperandMismatch {
                                op: *op,
                                left: self.apply(&typed_left.node.ty),
                                right: self.apply(&typed_right.node.ty),
                            },
                        )?);
                        children.push(self.require_is(
                            &typed_right.node.ty,
                            Type::bool(),
                            right_label_span,
                            right_label_span,
                            TypeErrorKind::BinaryOperandMismatch {
                                op: *op,
                                left: self.apply(&typed_left.node.ty),
                                right: self.apply(&typed_right.node.ty),
                            },
                        )?);
                        Type::bool()
                    }
                };

                let typed = Spanned::new(
                    TypedExpr::new(
                        expr_type.clone(),
                        TypedExprKind::Binary {
                            op: *op,
                            left: Box::new(typed_left),
                            right: Box::new(typed_right),
                        },
                    ),
                    expr.span,
                );

                let rule = match op {
                    BinaryOp::Add => "T-Bin-Add",
                    BinaryOp::Subtract => "T-Bin-Sub",
                    BinaryOp::Multiply => "T-Bin-Mul",
                    BinaryOp::Divide => "T-Bin-Div",
                    BinaryOp::Remainder => "T-Bin-Rem",
                    BinaryOp::Less => "T-Bin-Lt",
                    BinaryOp::LessEqual => "T-Bin-Le",
                    BinaryOp::Greater => "T-Bin-Gt",
                    BinaryOp::GreaterEqual => "T-Bin-Ge",
                    BinaryOp::Equal => "T-Bin-Eq",
                    BinaryOp::NotEqual => "T-Bin-Neq",
                    BinaryOp::And => "T-Bin-And",
                    BinaryOp::Or => "T-Bin-Or",
                };

                let result_repr = self.maybe_string(|| self.format_type(&typed.node.ty));
                let tree = self.make_trace(
                    rule,
                    env_context.clone(),
                    subject_repr.clone(),
                    result_repr,
                    || children,
                );

                Ok((typed, tree))
            }
            Expr::Grouping(inner) => {
                let (typed_inner, inner_trace) = self.infer_expr(env, inner, ctx)?;
                let typed = Spanned::new(
                    TypedExpr::new(
                        typed_inner.node.ty.clone(),
                        TypedExprKind::Grouping(Box::new(typed_inner.clone())),
                    ),
                    expr.span,
                );
                let result_repr = self.maybe_string(|| self.format_type(&typed.node.ty));
                let tree =
                    self.make_trace("T-Group", env_context, subject_repr, result_repr, || {
                        vec![inner_trace]
                    });
                Ok((typed, tree))
            }
            Expr::StructLiteral { name, fields } => {
                let info = self.lookup_struct_info(name)?;
                let mut expected = info
                    .fields
                    .iter()
                    .map(|field| (field.name.name.clone(), (field.ty.clone(), field.span)))
                    .collect::<HashMap<_, _>>();
                let mut typed_fields = Vec::with_capacity(fields.len());
                let mut children = Vec::new();
                let mut seen = HashMap::new();

                for field in fields {
                    if let Some(previous_span) = seen.get(&field.name.name) {
                        return Err(TypeError::new(
                            TypeErrorKind::DuplicateStructLiteralField {
                                field_name: field.name.name.clone(),
                            },
                            field.name.span.unwrap_or_else(dummy_span),
                        )
                        .with_primary_message("duplicate")
                        .with_secondary(*previous_span, "first used here"));
                    }
                    seen.insert(
                        field.name.name.clone(),
                        field.name.span.unwrap_or_else(dummy_span),
                    );

                    let (expected_ty, _) = expected.remove(&field.name.name).ok_or_else(|| {
                        TypeError::new(
                            TypeErrorKind::UnknownStructField {
                                struct_name: name.name.clone(),
                                field_name: field.name.name.clone(),
                            },
                            field.name.span.unwrap_or_else(dummy_span),
                        )
                    })?;

                    let (typed_value, value_trace) = self.infer_expr(env, &field.value, ctx)?;
                    let actual_ty = typed_value.node.ty.clone();
                    let (_, unify_trace) = self.unify(
                        actual_ty.clone(),
                        expected_ty.clone(),
                        field.value.span,
                        field.name.span.unwrap_or(field.value.span),
                        TypeErrorKind::GeneralMismatch {
                            expected: expected_ty,
                            found: self.apply(&actual_ty),
                        },
                    )?;
                    children.push(value_trace);
                    children.push(unify_trace);

                    typed_fields.push(TypedStructLiteralField {
                        name: field.name.clone(),
                        value: typed_value,
                    });
                }

                if let Some((field_name, _)) = expected.into_iter().next() {
                    return Err(TypeError::new(
                        TypeErrorKind::MissingStructField {
                            struct_name: name.name.clone(),
                            field_name,
                        },
                        name.span.unwrap_or_else(dummy_span),
                    ));
                }

                let typed = Spanned::new(
                    TypedExpr::new(
                        info.ty.clone(),
                        TypedExprKind::StructLiteral {
                            name: name.clone(),
                            fields: typed_fields,
                        },
                    ),
                    expr.span,
                );
                let result_repr = self.maybe_string(|| self.format_type(&typed.node.ty));
                let tree = self.make_trace(
                    "T-StructLit",
                    env_context,
                    subject_repr,
                    result_repr,
                    || children,
                );
                Ok((typed, tree))
            }
            Expr::FieldAccess { target, field } => {
                let (typed_target, target_trace) = self.infer_expr(env, target, ctx)?;
                let target_ty = self.apply(&typed_target.node.ty);
                let field_ty = match target_ty.clone() {
                    Type::Record(record) => record
                        .fields
                        .into_iter()
                        .find(|entry| entry.name == field.name)
                        .map(|entry| entry.ty)
                        .ok_or_else(|| {
                            TypeError::new(
                                TypeErrorKind::FieldAccessUnknownField {
                                    field_name: field.name.clone(),
                                    ty: target_ty.clone(),
                                },
                                field.span.unwrap_or_else(dummy_span),
                            )
                        })?,
                    _ => {
                        return Err(TypeError::new(
                            TypeErrorKind::FieldAccessNotStruct {
                                found: target_ty.clone(),
                            },
                            target.span,
                        ));
                    }
                };

                let typed = Spanned::new(
                    TypedExpr::new(
                        field_ty.clone(),
                        TypedExprKind::FieldAccess {
                            target: Box::new(typed_target.clone()),
                            field: field.clone(),
                        },
                    ),
                    expr.span,
                );
                let result_repr = self.maybe_string(|| self.format_type(&field_ty));
                let tree = self.make_trace(
                    "T-FieldAccess",
                    env_context,
                    subject_repr,
                    result_repr,
                    || vec![target_trace],
                );
                Ok((typed, tree))
            }
            Expr::EnumConstructor {
                enum_name,
                variant,
                payload,
            } => {
                let info = self.lookup_enum_info(enum_name)?;
                let variant_info = info
                    .variants
                    .iter()
                    .find(|entry| entry.name.name == variant.name)
                    .ok_or_else(|| {
                        TypeError::new(
                            TypeErrorKind::UnknownEnumVariant {
                                enum_name: enum_name.name.clone(),
                                variant_name: variant.name.clone(),
                            },
                            variant.span.unwrap_or_else(dummy_span),
                        )
                    })?;

                let mut children = Vec::new();
                let typed_payload = match (&payload, &variant_info.kind) {
                    (EnumConstructorPayload::Unit, EnumVariantInfoKind::Unit) => {
                        TypedEnumConstructorPayload::Unit
                    }
                    (
                        EnumConstructorPayload::Tuple(values),
                        EnumVariantInfoKind::Tuple(expected),
                    ) => {
                        if values.len() != expected.len() {
                            return Err(TypeError::new(
                                TypeErrorKind::EnumPayloadMismatch {
                                    enum_name: enum_name.name.clone(),
                                    variant_name: variant.name.clone(),
                                    expected: EnumPayloadKind::tuple(expected.len()),
                                    found: EnumPayloadKind::tuple(values.len()),
                                },
                                variant.span.unwrap_or_else(dummy_span),
                            ));
                        }

                        let mut typed_values = Vec::with_capacity(values.len());
                        for (expr, expected_ty) in values.iter().zip(expected.iter()) {
                            let (typed_expr, value_trace) = self.infer_expr(env, expr, ctx)?;
                            let actual_ty = typed_expr.node.ty.clone();
                            let (_, unify_trace) = self.unify(
                                actual_ty.clone(),
                                expected_ty.clone(),
                                expr.span,
                                variant.span.unwrap_or(expr.span),
                                TypeErrorKind::GeneralMismatch {
                                    expected: expected_ty.clone(),
                                    found: self.apply(&actual_ty),
                                },
                            )?;
                            children.push(value_trace);
                            children.push(unify_trace);
                            typed_values.push(typed_expr);
                        }
                        TypedEnumConstructorPayload::Tuple(typed_values)
                    }
                    (
                        EnumConstructorPayload::Struct(fields),
                        EnumVariantInfoKind::Struct(expected),
                    ) => {
                        let mut expected_fields = expected
                            .iter()
                            .map(|field| (field.name.name.clone(), field.clone()))
                            .collect::<HashMap<_, _>>();
                        let mut seen = HashMap::new();
                        let mut typed_fields = Vec::with_capacity(fields.len());
                        let struct_name =
                            format!("{}::{}", enum_name.name.clone(), variant.name.clone());

                        for field in fields {
                            if let Some(previous_span) = seen.get(&field.name.name) {
                                return Err(TypeError::new(
                                    TypeErrorKind::DuplicateStructLiteralField {
                                        field_name: field.name.name.clone(),
                                    },
                                    field.name.span.unwrap_or_else(dummy_span),
                                )
                                .with_primary_message("duplicate")
                                .with_secondary(*previous_span, "first used here"));
                            }
                            seen.insert(
                                field.name.name.clone(),
                                field.name.span.unwrap_or_else(dummy_span),
                            );

                            let expected_field =
                                expected_fields.remove(&field.name.name).ok_or_else(|| {
                                    TypeError::new(
                                        TypeErrorKind::UnknownStructField {
                                            struct_name: struct_name.clone(),
                                            field_name: field.name.name.clone(),
                                        },
                                        field.name.span.unwrap_or_else(dummy_span),
                                    )
                                })?;

                            let (typed_value, value_trace) =
                                self.infer_expr(env, &field.value, ctx)?;
                            let actual_ty = typed_value.node.ty.clone();
                            let (_, unify_trace) = self.unify(
                                actual_ty.clone(),
                                expected_field.ty.clone(),
                                field.value.span,
                                field.name.span.unwrap_or(field.value.span),
                                TypeErrorKind::GeneralMismatch {
                                    expected: expected_field.ty.clone(),
                                    found: self.apply(&actual_ty),
                                },
                            )?;
                            children.push(value_trace);
                            children.push(unify_trace);

                            typed_fields.push(TypedStructLiteralField {
                                name: field.name.clone(),
                                value: typed_value,
                            });
                        }

                        if let Some((missing_field, _)) = expected_fields.into_iter().next() {
                            return Err(TypeError::new(
                                TypeErrorKind::MissingStructField {
                                    struct_name,
                                    field_name: missing_field,
                                },
                                variant.span.unwrap_or_else(dummy_span),
                            ));
                        }

                        TypedEnumConstructorPayload::Struct(typed_fields)
                    }
                    (found_payload, expected_kind) => {
                        return Err(TypeError::new(
                            TypeErrorKind::EnumPayloadMismatch {
                                enum_name: enum_name.name.clone(),
                                variant_name: variant.name.clone(),
                                expected: enum_payload_kind_from_variant(expected_kind),
                                found: enum_payload_kind_from_constructor(found_payload),
                            },
                            variant.span.unwrap_or_else(dummy_span),
                        ));
                    }
                };

                let typed = Spanned::new(
                    TypedExpr::new(
                        info.ty.clone(),
                        TypedExprKind::EnumConstructor {
                            enum_name: enum_name.clone(),
                            variant: variant.clone(),
                            payload: typed_payload,
                        },
                    ),
                    expr.span,
                );
                let result_repr = self.maybe_string(|| self.format_type(&typed.node.ty));
                let tree =
                    self.make_trace("T-EnumCtor", env_context, subject_repr, result_repr, || {
                        children
                    });
                Ok((typed, tree))
            }
            Expr::Block(block) => {
                let (typed_block, block_traces) = self.infer_block(env, block, ctx, false)?;
                let unit_result = self.maybe_string(|| "()".to_string());
                let tree = self.make_trace(
                    "T-Block",
                    env_context.clone(),
                    subject_repr.clone(),
                    unit_result,
                    || block_traces,
                );
                let ty = typed_block
                    .tail_expression
                    .as_ref()
                    .map_or(Type::Unit, |tail| tail.node.ty.clone());
                Ok((
                    Spanned::new(
                        TypedExpr::new(ty, TypedExprKind::Block(Box::new(typed_block))),
                        expr.span,
                    ),
                    tree,
                ))
            }
            Expr::If {
                branches,
                else_branch,
            } => {
                let mut children = Vec::new();
                let mut typed_branches = Vec::with_capacity(branches.len());
                let mut result_ty: Option<Type> = None;

                for (condition, then_branch) in branches {
                    let (typed_condition, cond_trace) = self.infer_expr(env, condition, ctx)?;
                    children.push(cond_trace);
                    let bool_check = self.require_bool(
                        &typed_condition.node.ty,
                        condition.span,
                        ConditionContext::If,
                    )?;
                    children.push(bool_check);

                    let (typed_then, then_traces) =
                        self.infer_block(env, then_branch, ctx, false)?;
                    children.extend(then_traces);

                    let then_ty = typed_then
                        .tail_expression
                        .as_ref()
                        .map_or(Type::Unit, |tail| tail.node.ty.clone());

                    result_ty = if let Some(current) = result_ty {
                        let (merged, unify_trace) = self.unify(
                            current.clone(),
                            then_ty.clone(),
                            typed_then
                                .tail_expression
                                .as_ref()
                                .map(|expr| expr.span)
                                .unwrap_or(expr.span),
                            expr.span,
                            TypeErrorKind::GeneralMismatch {
                                expected: current,
                                found: self.apply(&then_ty),
                            },
                        )?;
                        children.push(unify_trace);
                        Some(merged)
                    } else {
                        Some(then_ty)
                    };

                    typed_branches.push((typed_condition, typed_then));
                }

                let typed_else_block = if let Some(block) = else_branch {
                    let (typed_block, else_traces) = self.infer_block(env, block, ctx, false)?;
                    children.extend(else_traces);
                    Some(typed_block)
                } else {
                    None
                };

                let (else_ty, else_span) = typed_else_block
                    .as_ref()
                    .and_then(|b| {
                        b.tail_expression
                            .as_ref()
                            .map(|tail| (tail.node.ty.clone(), tail.span))
                    })
                    .unwrap_or((Type::Unit, expr.span));
                let result_ty = if let Some(current) = result_ty {
                    let (merged, unify_trace) = self.unify(
                        current.clone(),
                        else_ty.clone(),
                        else_span,
                        expr.span,
                        TypeErrorKind::GeneralMismatch {
                            expected: current,
                            found: self.apply(&else_ty),
                        },
                    )?;
                    children.push(unify_trace);
                    merged
                } else {
                    else_ty
                };

                let unit_result = self.maybe_string(|| "()".to_string());
                let tree = self.make_trace(
                    "T-If",
                    env_context.clone(),
                    subject_repr.clone(),
                    unit_result,
                    || children,
                );

                Ok((
                    Spanned::new(
                        TypedExpr {
                            ty: result_ty,
                            kind: TypedExprKind::If {
                                branches: typed_branches,
                                else_branch: typed_else_block.map(Box::new),
                            },
                        },
                        expr.span,
                    ),
                    tree,
                ))
            }
            Expr::Match { scrutinee, arms } => {
                let (typed_scrutinee, scrutinee_trace) = self.infer_expr(env, scrutinee, ctx)?;
                let mut children = vec![scrutinee_trace];
                let mut typed_arms = Vec::with_capacity(arms.len());
                let mut result_ty: Option<(Type, Span)> = None;

                for arm in arms {
                    env.push_scope();
                    let (typed_pattern, mut pattern_traces) = self.infer_pattern(
                        env,
                        &arm.pattern,
                        typed_scrutinee.node.ty.clone(),
                        scrutinee.span,
                    )?;
                    children.append(&mut pattern_traces);

                    let (typed_block, mut block_traces) =
                        self.infer_block(env, &arm.body, ctx, false)?;
                    children.append(&mut block_traces);
                    env.pop_scope();

                    let arm_ty = typed_block
                        .tail_expression
                        .as_ref()
                        .map(|expr| expr.node.ty.clone())
                        .unwrap_or_else(Type::unit);

                    let arm_span = arm
                        .body
                        .tail_expression
                        .as_ref()
                        .map(|e| e.span)
                        .unwrap_or(expr.span);

                    result_ty = if let Some((first_ty, first_span)) = result_ty {
                        let (merged, unify_trace) = self.unify_match_arms(
                            arm_ty.clone(),
                            first_ty.clone(),
                            arm_span,
                            first_span,
                        )?;
                        children.push(unify_trace);
                        Some((merged, first_span))
                    } else {
                        Some((arm_ty, arm_span))
                    };

                    typed_arms.push(TypedMatchArm {
                        pattern: typed_pattern,
                        body: typed_block,
                    });
                }

                // Check exhaustiveness and redundancy
                let scrutinee_ty = self.apply(&typed_scrutinee.node.ty);
                if let Err(exhaustiveness_errors) =
                    super::exhaustiveness::check_match(&scrutinee_ty, &typed_arms, expr.span)
                {
                    // Return the first exhaustiveness error
                    return Err(exhaustiveness_errors.into_iter().next().unwrap());
                }

                let expr_type = result_ty.map(|(ty, _)| ty).unwrap_or_else(Type::unit);
                let typed = Spanned::new(
                    TypedExpr::new(
                        expr_type.clone(),
                        TypedExprKind::Match {
                            scrutinee: Box::new(typed_scrutinee.clone()),
                            arms: typed_arms,
                        },
                    ),
                    expr.span,
                );
                let result_repr = self.maybe_string(|| self.format_type(&expr_type));
                let tree =
                    self.make_trace("T-Match", env_context, subject_repr, result_repr, || {
                        children
                    });
                Ok((typed, tree))
            }
            Expr::Call { callee, args } => {
                let (typed_callee, callee_trace) = self.infer_expr(env, callee, ctx)?;
                let callee_ty = self.apply(&typed_callee.node.ty);

                let callee_name = if let TypedExprKind::Identifier(Identifier { name, .. }) =
                    &typed_callee.node.kind
                {
                    Some(name.as_str())
                } else {
                    None
                };

                let (param_types, param_spans, return_type) = match &callee_ty {
                    Type::Function(params, ret) => {
                        let spans = callee_name
                            .and_then(|name| self.functions.get(name))
                            .map(|info| info.param_spans.clone())
                            .unwrap_or_default();
                        (params.clone(), spans, (**ret).clone())
                    }
                    _ => {
                        if let Some(name) = callee_name {
                            if let Some(info) = self.functions.get(name) {
                                (
                                    info.param_types.clone(),
                                    info.param_spans.clone(),
                                    info.return_type.clone(),
                                )
                            } else {
                                return Err(TypeError::new(
                                    TypeErrorKind::NotAFunction { found: callee_ty },
                                    callee.span,
                                ));
                            }
                        } else {
                            return Err(TypeError::new(
                                TypeErrorKind::NotAFunction { found: callee_ty },
                                callee.span,
                            ));
                        }
                    }
                };

                if args.len() != param_types.len() {
                    return Err(TypeError::new(
                        TypeErrorKind::ArityMismatch {
                            expected: param_types.len(),
                            found: args.len(),
                        },
                        expr.span,
                    ));
                }

                let mut children = vec![callee_trace];
                let mut typed_args = Vec::with_capacity(args.len());

                for (index, (arg, expected_ty)) in
                    args.iter().zip(param_types.iter()).enumerate()
                {
                    let (typed_arg, arg_trace) = self.infer_expr(env, arg, ctx)?;
                    let actual_ty = typed_arg.node.ty.clone();

                    let param_span = param_spans.get(index).copied();

                    let (_, unify_trace) = self.unify(
                        actual_ty.clone(),
                        expected_ty.clone(),
                        arg.span,
                        arg.span,
                        TypeErrorKind::ArgumentTypeMismatch {
                            expected: expected_ty.clone(),
                            found: self.apply(&actual_ty),
                            position: index + 1,
                            param_span,
                        },
                    )?;

                    children.push(arg_trace);
                    children.push(unify_trace);
                    typed_args.push(typed_arg);
                }

                let typed = Spanned::new(
                    TypedExpr::new(
                        return_type.clone(),
                        TypedExprKind::Call {
                            callee: Box::new(typed_callee),
                            args: typed_args,
                        },
                    ),
                    expr.span,
                );

                let result_repr = self.maybe_string(|| self.format_type(&return_type));

                let tree =
                    self.make_trace("T-Call", env_context, subject_repr, result_repr, || {
                        children
                    });

                Ok((typed, tree))
            }
        }
    }

    fn type_from_annotation(&mut self, annotation: &TypeAnnotation) -> Result<Type, TypeError> {
        if !annotation.generics.is_empty() {
            return Err(TypeError::new(
                TypeErrorKind::UnsupportedTypeFeature {
                    description: "generic type parameters are not supported yet".to_string(),
                },
                annotation.name.span.unwrap_or_else(dummy_span),
            )
            .with_help("remove `<...>` until generics are implemented"));
        }

        match annotation.name.name.as_str() {
            "i64" => Ok(Type::int()),
            "bool" => Ok(Type::bool()),
            "()" => Ok(Type::unit()),
            "_" => Ok(self.fresh_var()),
            other => {
                if let Some(entry) = self.types.get(other) {
                    Ok(entry.ty.clone())
                } else {
                    Err(TypeError::new(
                        TypeErrorKind::UnknownTypeAnnotation {
                            name: other.to_string(),
                        },
                        annotation.name.span.unwrap_or_else(dummy_span),
                    ))
                }
            }
        }
    }

    /// Ensure a type is boolean, emitting a trace entry describing the check.
    fn require_bool(
        &mut self,
        ty: &Type,
        span: Span,
        context: ConditionContext,
    ) -> Result<InferenceTree, TypeError> {
        let applied = self.apply(ty);
        if matches!(&applied, Type::Bool) {
            let subject = self.maybe_string(|| self.format_type(&applied));
            Ok(self.make_trace(
                "Check-Bool",
                None,
                subject,
                Some("ok".to_string()),
                Vec::new,
            ))
        } else {
            Err(TypeError::new(
                TypeErrorKind::ConditionNotBool {
                    context,
                    found: applied,
                },
                span,
            ))
        }
    }

    /// Unify two types, capturing the resulting unification step in the trace.
    fn require_is(
        &mut self,
        actual: &Type,
        expected: Type,
        left_span: Span,
        right_span: Span,
        kind: TypeErrorKind,
    ) -> Result<InferenceTree, TypeError> {
        let (_, tree) = self.unify(actual.clone(), expected, left_span, right_span, kind)?;
        Ok(tree)
    }

    /// Ensure comparison operands are either both integers or both booleans.
    fn require_numeric_or_bool_pair(
        &mut self,
        op: &BinaryOp,
        left: &Spanned<TypedExpr>,
        left_span: Span,
        right: &Spanned<TypedExpr>,
        right_span: Span,
    ) -> Result<InferenceTree, TypeError> {
        let left_ty = self.apply(&left.node.ty);
        let right_ty = self.apply(&right.node.ty);

        if matches!(&left_ty, Type::Int) && matches!(&right_ty, Type::Int) {
            let subject = self.maybe_string(|| {
                format!(
                    "{} vs {}",
                    self.format_type(&left_ty),
                    self.format_type(&right_ty)
                )
            });
            let result = self.maybe_string(|| "ok (int)".to_string());
            Ok(self.make_trace("Check-Compare", None, subject, result, Vec::new))
        } else if matches!(&left_ty, Type::Bool) && matches!(&right_ty, Type::Bool) {
            let subject = self.maybe_string(|| {
                format!(
                    "{} vs {}",
                    self.format_type(&left_ty),
                    self.format_type(&right_ty)
                )
            });
            let result = self.maybe_string(|| "ok (bool)".to_string());
            Ok(self.make_trace("Check-Compare", None, subject, result, Vec::new))
        } else {
            Err(TypeError::new(
                TypeErrorKind::BinaryOperandMismatch {
                    op: *op,
                    left: left_ty.clone(),
                    right: right_ty.clone(),
                },
                left_span,
            )
            .with_primary_message(format!("has type `{}`", left_ty.to_compact_string()))
            .with_secondary(right_span, format!("has type `{}`", right_ty.to_compact_string())))
        }
    }

    /// Ensure equality operands share the same primitive type.
    fn require_same_primitive(
        &mut self,
        op: &BinaryOp,
        left: &Spanned<TypedExpr>,
        left_span: Span,
        right: &Spanned<TypedExpr>,
        right_span: Span,
    ) -> Result<InferenceTree, TypeError> {
        let left_ty = self.apply(&left.node.ty);
        let right_ty = self.apply(&right.node.ty);

        if (matches!(&left_ty, Type::Int) && matches!(&right_ty, Type::Int))
            || (matches!(&left_ty, Type::Bool) && matches!(&right_ty, Type::Bool))
        {
            let subject = self.maybe_string(|| {
                format!(
                    "{} vs {}",
                    self.format_type(&left_ty),
                    self.format_type(&right_ty)
                )
            });
            let result = self.maybe_string(|| "ok".to_string());
            Ok(self.make_trace("Check-Eq", None, subject, result, Vec::new))
        } else {
            Err(TypeError::new(
                TypeErrorKind::BinaryOperandMismatch {
                    op: *op,
                    left: left_ty.clone(),
                    right: right_ty.clone(),
                },
                left_span,
            )
            .with_primary_message(format!("has type `{}`", left_ty.to_compact_string()))
            .with_secondary(right_span, format!("has type `{}`", right_ty.to_compact_string())))
        }
    }

    /// Fully normalize a type by applying the current substitution set.
    fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(id) => match self.subst.get(id) {
                Some(ty) => self.apply(ty),
                None => Type::Var(*id),
            },
            Type::Function(params, result) => Type::Function(
                params.iter().map(|t| self.apply(t)).collect(),
                Box::new(self.apply(result)),
            ),
            Type::Tuple(items) => Type::Tuple(items.iter().map(|t| self.apply(t)).collect()),
            Type::Record(record) => Type::Record(RecordType {
                name: record.name.clone(),
                fields: record
                    .fields
                    .iter()
                    .map(|field| TypeRecordField {
                        name: field.name.clone(),
                        ty: self.apply(&field.ty),
                    })
                    .collect(),
            }),
            Type::Enum(enum_type) => Type::Enum(EnumType {
                name: enum_type.name.clone(),
                variants: enum_type
                    .variants
                    .iter()
                    .map(|variant| TypeEnumVariant {
                        name: variant.name.clone(),
                        kind: match &variant.kind {
                            TypeEnumVariantKind::Unit => TypeEnumVariantKind::Unit,
                            TypeEnumVariantKind::Tuple(payload) => TypeEnumVariantKind::Tuple(
                                payload.iter().map(|ty| self.apply(ty)).collect(),
                            ),
                            TypeEnumVariantKind::Struct(fields) => TypeEnumVariantKind::Struct(
                                fields
                                    .iter()
                                    .map(|field| {
                                        TypeRecordField::new(
                                            field.name.clone(),
                                            self.apply(&field.ty),
                                        )
                                    })
                                    .collect(),
                            ),
                        },
                    })
                    .collect(),
            }),
            Type::Int => Type::Int,
            Type::Bool => Type::Bool,
            Type::Unit => Type::Unit,
        }
    }

    /// Rewrite every definition in the program with normalized types.
    fn apply_substitutions_program(&self, program: &mut TypedProgram) {
        for definition in &mut program.definitions {
            self.apply_definition(definition);
        }
    }

    fn apply_definition(&self, definition: &mut TypedDefinition) {
        match definition {
            TypedDefinition::Function(function) => self.apply_function(function),
            TypedDefinition::Utxo(utxo) => self.apply_utxo(utxo),
            TypedDefinition::Struct(_) | TypedDefinition::Enum(_) => {}
        }
    }

    fn apply_utxo(&self, utxo: &mut TypedUtxoDef) {
        for part in &mut utxo.parts {
            match part {
                TypedUtxoPart::Storage(vars) => {
                    for var in vars {
                        var.ty = self.apply(&var.ty);
                    }
                }
            }
        }
    }

    fn apply_function(&self, function: &mut TypedFunctionDef) {
        function.return_type = self.apply(&function.return_type);
        for param in &mut function.params {
            param.ty = self.apply(&param.ty);
        }
        self.apply_block(&mut function.body);
    }

    /// Visit a single statement and normalize any embedded type annotations.
    fn apply_statement(&self, statement: &mut TypedStatement) {
        match statement {
            TypedStatement::VariableDeclaration { value, .. } => {
                self.apply_expr(value);
            }
            TypedStatement::Assignment { value, .. } => {
                self.apply_expr(value);
            }
            TypedStatement::While { condition, body } => {
                self.apply_expr(condition);
                self.apply_block(body);
            }
            TypedStatement::Expression(expr) => self.apply_expr(expr),
            TypedStatement::Return(Some(expr)) => self.apply_expr(expr),
            TypedStatement::Return(None) => {}
        }
    }

    /// Visit each statement inside a block and normalize its annotations.
    fn apply_block(&self, block: &mut TypedBlock) {
        for statement in &mut block.statements {
            self.apply_statement(statement);
        }
        if let Some(expr) = &mut block.tail_expression {
            self.apply_expr(expr);
        }
    }

    /// Normalize the type attached to an expression and recursively visit its children.
    fn apply_expr(&self, expr: &mut Spanned<TypedExpr>) {
        expr.node.ty = self.apply(&expr.node.ty);
        match &mut expr.node.kind {
            TypedExprKind::Literal(_) | TypedExprKind::Identifier(_) => {}
            TypedExprKind::Unary { expr: inner, .. } => self.apply_expr(inner),
            TypedExprKind::Binary { left, right, .. } => {
                self.apply_expr(left);
                self.apply_expr(right);
            }
            TypedExprKind::Grouping(inner) => self.apply_expr(inner),
            TypedExprKind::StructLiteral { fields, .. } => {
                for field in fields {
                    self.apply_expr(&mut field.value);
                }
            }
            TypedExprKind::FieldAccess { target, .. } => self.apply_expr(target),
            TypedExprKind::EnumConstructor { payload, .. } => match payload {
                TypedEnumConstructorPayload::Unit => {}
                TypedEnumConstructorPayload::Tuple(values) => {
                    for expr in values {
                        self.apply_expr(expr);
                    }
                }
                TypedEnumConstructorPayload::Struct(fields) => {
                    for field in fields {
                        self.apply_expr(&mut field.value);
                    }
                }
            },
            TypedExprKind::Block(block) => self.apply_block(block),
            TypedExprKind::If {
                branches,
                else_branch,
            } => {
                for (condition, then_branch) in branches {
                    self.apply_expr(condition);
                    self.apply_block(then_branch);
                }
                if let Some(block) = else_branch {
                    self.apply_block(block);
                }
            }
            TypedExprKind::Match { scrutinee, arms } => {
                self.apply_expr(scrutinee);
                for arm in arms {
                    self.apply_block(&mut arm.body);
                }
            }
            TypedExprKind::Call { callee, args } => {
                self.apply_expr(callee);

                for arg in args {
                    self.apply_expr(arg);
                }
            }
        }
    }

    /// Quantify over all type variables that are free in `ty` but not in the environment.
    fn generalize(&self, env: &TypeEnv, ty: &Type) -> Scheme {
        let applied = self.apply(ty);
        let mut ty_free = free_type_vars_type(&applied);
        let env_free = env.free_type_vars();
        ty_free.retain(|var| !env_free.contains(var));
        let mut vars: Vec<_> = ty_free.into_iter().collect();
        vars.sort();
        Scheme { vars, ty: applied }
    }

    /// Replace every quantified variable in `scheme` with a fresh type variable.
    fn instantiate(&mut self, scheme: &Scheme) -> Type {
        let mut mapping = HashMap::new();
        for var in &scheme.vars {
            mapping.insert(*var, self.fresh_var());
        }
        substitute_type(&scheme.ty, &mapping)
    }

    /// Allocate a new inference variable unique to this inferencer.
    fn fresh_var(&mut self) -> Type {
        let id = TypeVarId(self.next_type_var);
        self.next_type_var += 1;
        Type::Var(id)
    }

    /// Render the current environment snapshot into a deterministic string.
    fn format_env(&self, env: &TypeEnv) -> String {
        let snapshot = env.snapshot();
        if snapshot.is_empty() {
            "{}".to_string()
        } else {
            let entries = snapshot
                .iter()
                .map(|(name, scheme)| format!("{name}: {scheme}"))
                .collect::<Vec<_>>();
            format!("{{{}}}", entries.join(", "))
        }
    }

    /// Pretty-print a statement for inclusion in the inference trace.
    fn format_statement_src(&self, statement: &Statement) -> String {
        formatter::statement(statement)
            .map(|s| s.trim().to_string())
            .unwrap_or_else(|_| format!("{statement:?}"))
    }

    /// Pretty-print an expression for inclusion in the inference trace.
    fn format_expr_src(&self, expr: &Spanned<Expr>) -> String {
        formatter::expression(&expr.node)
            .map(|s| s.trim().to_string())
            .unwrap_or_else(|_| format!("{:?}", expr.node))
    }

    /// Compute a string lazily, only when capture_traces is enabled.
    fn maybe_string<F>(&self, f: F) -> Option<String>
    where
        F: FnOnce() -> String,
    {
        if self.capture_traces { Some(f()) } else { None }
    }

    /// Assemble an inference tree node when tracing is active; otherwise return the default node.
    fn make_trace<C>(
        &self,
        rule: &str,
        context: Option<String>,
        subject: Option<String>,
        result: Option<String>,
        children: C,
    ) -> InferenceTree
    where
        C: FnOnce() -> Vec<InferenceTree>,
    {
        if !self.capture_traces {
            return InferenceTree::default();
        }

        let context = context.unwrap_or_default();
        let subject = subject.unwrap_or_default();
        let result = result.unwrap_or_default();
        let mut tree = InferenceTree::new(rule, context, subject, result);
        let kids = children();
        if !kids.is_empty() {
            tree = tree.with_children(kids);
        }
        tree
    }

    /// Produce a span sized to the formatted expression, avoiding trailing whitespace.
    fn label_span_for_expr(&self, expr: &Spanned<Expr>) -> Span {
        let formatted = self.format_expr_src(expr);
        let max_len = expr.span.end.saturating_sub(expr.span.start);
        let new_end = expr.span.start + formatted.len().min(max_len);
        Span {
            start: expr.span.start,
            end: new_end,
            context: expr.span.context,
        }
    }

    /// Short helper to format a type compactly for error messages.
    fn format_type(&self, ty: &Type) -> String {
        ty.to_compact_string()
    }

    /// Format the difference between the stored substitution map and a prior snapshot.
    fn format_subst_diff(&self, before: &HashMap<TypeVarId, Type>) -> String {
        let mut entries = Vec::new();
        for (key, value) in &self.subst {
            if before.get(key) != Some(value) {
                entries.push(format!("{}/{}", self.format_type(value), key.as_str()));
            }
        }
        entries.sort();
        format!("{{{}}}", entries.join(", "))
    }

    /// Unify match arm types with custom error labels.
    ///
    /// The `current_arm` is the arm being checked, and `first_arm` is the first arm
    /// that established the expected type. On error, the primary label is on the
    /// current arm, and the secondary label explains that the first arm set the expectation.
    fn unify_match_arms(
        &mut self,
        current_arm_ty: Type,
        first_arm_ty: Type,
        current_arm_span: Span,
        first_arm_span: Span,
    ) -> Result<(Type, InferenceTree), TypeError> {
        let current_ty = self.apply(&current_arm_ty);
        let first_ty = self.apply(&first_arm_ty);

        match self.unify_inner(current_ty.clone(), first_ty.clone()) {
            Ok((result_ty, children, rule)) => {
                let subject = self.maybe_string(|| {
                    format!(
                        "{} ~ {}",
                        self.format_type(&current_ty),
                        self.format_type(&first_ty)
                    )
                });
                let result_repr = self.maybe_string(|| self.format_type(&result_ty));
                let tree = self.make_trace(rule, None, subject, result_repr, || children);
                Ok((result_ty, tree))
            }
            Err(_) => {
                let current_repr = self.format_type(&current_ty);
                let first_repr = self.format_type(&first_ty);
                let err = TypeError::new(
                    TypeErrorKind::GeneralMismatch {
                        expected: first_ty,
                        found: current_ty,
                    },
                    current_arm_span,
                )
                .with_primary_message(format!("has type `{current_repr}`"))
                .with_secondary(
                    first_arm_span,
                    format!("expected `{first_repr}` due to this"),
                );
                Err(err)
            }
        }
    }

    /// Inner unification logic that returns the result without creating error labels.
    fn unify_inner(
        &mut self,
        left: Type,
        right: Type,
    ) -> Result<(Type, Vec<InferenceTree>, &'static str), ()> {
        match (left.clone(), right.clone()) {
            (Type::Int, Type::Int) => Ok((Type::Int, Vec::new(), "Unify-Const")),
            (Type::Bool, Type::Bool) => Ok((Type::Bool, Vec::new(), "Unify-Const")),
            (Type::Unit, Type::Unit) => Ok((Type::Unit, Vec::new(), "Unify-Const")),
            (Type::Tuple(ls), Type::Tuple(rs)) if ls.len() == rs.len() => {
                let mut children = Vec::new();
                for (l, r) in ls.iter().zip(rs.iter()) {
                    let (_, child, _) = self.unify_inner(l.clone(), r.clone())?;
                    children.extend(child);
                }
                Ok((Type::Tuple(ls), children, "Unify-Tuple"))
            }
            (Type::Function(lp, lr), Type::Function(rp, rr)) if lp.len() == rp.len() => {
                let mut children = Vec::new();
                for (l, r) in lp.iter().zip(rp.iter()) {
                    let (_, child, _) = self.unify_inner(l.clone(), r.clone())?;
                    children.extend(child);
                }
                let (_, ret_child, _) = self.unify_inner((*lr).clone(), (*rr).clone())?;
                children.extend(ret_child);
                Ok((Type::Function(lp, lr), children, "Unify-Arrow"))
            }
            (Type::Record(ls), Type::Record(rs))
                if ls.name == rs.name && ls.fields.len() == rs.fields.len() =>
            {
                let mut children = Vec::new();
                for (lf, rf) in ls.fields.iter().zip(rs.fields.iter()) {
                    if lf.name != rf.name {
                        return Err(());
                    }
                    let (_, child, _) = self.unify_inner(lf.ty.clone(), rf.ty.clone())?;
                    children.extend(child);
                }
                Ok((Type::Record(ls), children, "Unify-Record"))
            }
            (Type::Enum(ls), Type::Enum(rs))
                if ls.name == rs.name && ls.variants.len() == rs.variants.len() =>
            {
                // Simplified: just check structural compatibility
                Ok((Type::Enum(ls), Vec::new(), "Unify-Enum"))
            }
            (Type::Var(id), ty) => {
                if ty == Type::Var(id) {
                    return Ok((ty, Vec::new(), "Unify-Var"));
                }
                if occurs_in(id, &ty, &self.subst) {
                    return Err(());
                }
                self.subst.insert(id, ty.clone());
                Ok((ty, Vec::new(), "Unify-Var"))
            }
            (ty, Type::Var(id)) => {
                if ty == Type::Var(id) {
                    return Ok((ty, Vec::new(), "Unify-Var"));
                }
                if occurs_in(id, &ty, &self.subst) {
                    return Err(());
                }
                self.subst.insert(id, ty.clone());
                Ok((ty, Vec::new(), "Unify-Var"))
            }
            _ => Err(()),
        }
    }

    /// Unify two types, updating the substitution set and returning a trace node.
    fn unify(
        &mut self,
        left: Type,
        right: Type,
        left_span: Span,
        right_span: Span,
        error_kind: TypeErrorKind,
    ) -> Result<(Type, InferenceTree), TypeError> {
        let left = self.apply(&left);
        let right = self.apply(&right);
        let subject = self
            .maybe_string(|| format!("{} ~ {}", self.format_type(&left), self.format_type(&right)));
        let before = if self.capture_traces {
            Some(self.subst.clone())
        } else {
            None
        };

        let (result_ty, children, rule) = match (left.clone(), right.clone()) {
            (Type::Int, Type::Int) => (Type::Int, Vec::new(), "Unify-Const"),
            (Type::Bool, Type::Bool) => (Type::Bool, Vec::new(), "Unify-Const"),
            (Type::Unit, Type::Unit) => (Type::Unit, Vec::new(), "Unify-Const"),
            (Type::Tuple(ls), Type::Tuple(rs)) => {
                if ls.len() != rs.len() {
                    return Err(TypeError::new(error_kind, left_span)
                        .with_secondary(right_span, "type mismatch"));
                }

                let mut tuple_children = Vec::new();
                for (l, r) in ls.iter().zip(rs.iter()) {
                    let (_, child) = self.unify(
                        l.clone(),
                        r.clone(),
                        left_span,
                        right_span,
                        TypeErrorKind::GeneralMismatch {
                            expected: l.clone(),
                            found: r.clone(),
                        },
                    )?;
                    tuple_children.push(child);
                }
                (Type::Tuple(ls), tuple_children, "Unify-Tuple")
            }
            (Type::Function(lp, lr), Type::Function(rp, rr)) => {
                if lp.len() != rp.len() {
                    return Err(TypeError::new(error_kind, left_span)
                        .with_secondary(right_span, "function arity mismatch"));
                }

                let mut arrow_children = Vec::new();
                for (l, r) in lp.iter().zip(rp.iter()) {
                    let (_, child) = self.unify(
                        l.clone(),
                        r.clone(),
                        left_span,
                        right_span,
                        TypeErrorKind::GeneralMismatch {
                            expected: l.clone(),
                            found: r.clone(),
                        },
                    )?;
                    arrow_children.push(child);
                }

                let (_, ret_child) = self.unify(
                    (*lr).clone(),
                    (*rr).clone(),
                    left_span,
                    right_span,
                    TypeErrorKind::GeneralMismatch {
                        expected: (*lr).clone(),
                        found: (*rr).clone(),
                    },
                )?;
                arrow_children.push(ret_child);

                (Type::Function(lp, lr), arrow_children, "Unify-Arrow")
            }
            (Type::Record(mut ls), Type::Record(mut rs)) => {
                ls.fields.sort_by(|a, b| a.name.cmp(&b.name));
                rs.fields.sort_by(|a, b| a.name.cmp(&b.name));
                if ls.fields.len() != rs.fields.len()
                    || ls
                        .fields
                        .iter()
                        .zip(rs.fields.iter())
                        .any(|(l, r)| l.name != r.name)
                {
                    return Err(TypeError::new(error_kind, left_span)
                        .with_secondary(right_span, "struct field mismatch"));
                }

                let mut record_children = Vec::new();
                for (left_field, right_field) in ls.fields.iter().zip(rs.fields.iter()) {
                    let (_, trace) = self.unify(
                        left_field.ty.clone(),
                        right_field.ty.clone(),
                        left_span,
                        right_span,
                        TypeErrorKind::GeneralMismatch {
                            expected: left_field.ty.clone(),
                            found: right_field.ty.clone(),
                        },
                    )?;
                    record_children.push(trace);
                }
                (Type::Record(ls), record_children, "Unify-Record")
            }
            (Type::Enum(mut ls), Type::Enum(mut rs)) => {
                ls.variants.sort_by(|a, b| a.name.cmp(&b.name));
                rs.variants.sort_by(|a, b| a.name.cmp(&b.name));
                if ls.variants.len() != rs.variants.len()
                    || ls
                        .variants
                        .iter()
                        .zip(rs.variants.iter())
                        .any(|(l, r)| l.name != r.name)
                {
                    return Err(TypeError::new(error_kind, left_span)
                        .with_secondary(right_span, "enum variant mismatch"));
                }

                let mut enum_children = Vec::new();
                for (left_variant, right_variant) in ls.variants.iter().zip(rs.variants.iter()) {
                    match (&left_variant.kind, &right_variant.kind) {
                        (TypeEnumVariantKind::Unit, TypeEnumVariantKind::Unit) => {}
                        (
                            TypeEnumVariantKind::Tuple(left_payload),
                            TypeEnumVariantKind::Tuple(right_payload),
                        ) => {
                            if left_payload.len() != right_payload.len() {
                                return Err(TypeError::new(error_kind.clone(), left_span)
                                    .with_secondary(right_span, "enum payload mismatch"));
                            }
                            for (left_ty, right_ty) in left_payload.iter().zip(right_payload.iter())
                            {
                                let (_, trace) = self.unify(
                                    left_ty.clone(),
                                    right_ty.clone(),
                                    left_span,
                                    right_span,
                                    TypeErrorKind::GeneralMismatch {
                                        expected: left_ty.clone(),
                                        found: right_ty.clone(),
                                    },
                                )?;
                                enum_children.push(trace);
                            }
                        }
                        (
                            TypeEnumVariantKind::Struct(left_fields),
                            TypeEnumVariantKind::Struct(right_fields),
                        ) => {
                            if left_fields.len() != right_fields.len()
                                || left_fields
                                    .iter()
                                    .zip(right_fields.iter())
                                    .any(|(l, r)| l.name != r.name)
                            {
                                return Err(TypeError::new(error_kind.clone(), left_span)
                                    .with_secondary(right_span, "enum payload mismatch"));
                            }

                            for (left_field, right_field) in
                                left_fields.iter().zip(right_fields.iter())
                            {
                                let (_, trace) = self.unify(
                                    left_field.ty.clone(),
                                    right_field.ty.clone(),
                                    left_span,
                                    right_span,
                                    TypeErrorKind::GeneralMismatch {
                                        expected: left_field.ty.clone(),
                                        found: right_field.ty.clone(),
                                    },
                                )?;
                                enum_children.push(trace);
                            }
                        }
                        _ => {
                            return Err(TypeError::new(error_kind.clone(), left_span)
                                .with_secondary(right_span, "enum payload mismatch"));
                        }
                    }
                }
                (Type::Enum(ls), enum_children, "Unify-Enum")
            }
            (Type::Var(id), ty) => {
                self.bind(id, ty.clone(), left_span, right_span, error_kind.clone())?;
                (ty, Vec::new(), "Unify-Var")
            }
            (ty, Type::Var(id)) => {
                self.bind(id, ty.clone(), right_span, left_span, error_kind.clone())?;
                (ty, Vec::new(), "Unify-Var")
            }
            _ => {
                let mut err = TypeError::new(error_kind, left_span);
                if left_span != right_span {
                    let left_repr = self.format_type(&left);
                    let right_repr = self.format_type(&right);
                    err = err
                        .with_primary_message(format!("has type `{left_repr}`"))
                        .with_secondary(right_span, format!("has type `{right_repr}`"));
                }
                return Err(err);
            }
        };

        let result_repr = if let Some(before) = before.as_ref() {
            self.maybe_string(|| self.format_subst_diff(before))
        } else {
            None
        };
        let tree = self.make_trace(rule, None, subject, result_repr, || children);
        Ok((result_ty, tree))
    }

    fn bind(
        &mut self,
        var: TypeVarId,
        ty: Type,
        var_span: Span,
        other_span: Span,
        kind: TypeErrorKind,
    ) -> Result<(), TypeError> {
        if ty == Type::Var(var) {
            return Ok(());
        }

        if occurs_in(var, &ty, &self.subst) {
            return Err(TypeError::new(kind, var_span)
                .with_secondary(other_span, "would create an infinite type"));
        }

        self.subst.insert(var, ty);
        Ok(())
    }
}

fn dummy_span() -> Span {
    Span {
        start: 0,
        end: 0,
        context: (),
    }
}

/// Recursively replace any variables mentioned in `mapping` within `ty`.
fn substitute_type(ty: &Type, mapping: &HashMap<TypeVarId, Type>) -> Type {
    match ty {
        Type::Var(id) => mapping.get(id).cloned().unwrap_or(Type::Var(*id)),
        Type::Function(params, result) => Type::Function(
            params
                .iter()
                .map(|ty| substitute_type(ty, mapping))
                .collect(),
            Box::new(substitute_type(result, mapping)),
        ),
        Type::Tuple(items) => Type::Tuple(
            items
                .iter()
                .map(|ty| substitute_type(ty, mapping))
                .collect(),
        ),
        Type::Record(record) => Type::Record(RecordType {
            name: record.name.clone(),
            fields: record
                .fields
                .iter()
                .map(|field| TypeRecordField {
                    name: field.name.clone(),
                    ty: substitute_type(&field.ty, mapping),
                })
                .collect(),
        }),
        Type::Enum(enum_type) => Type::Enum(EnumType {
            name: enum_type.name.clone(),
            variants: enum_type
                .variants
                .iter()
                .map(|variant| TypeEnumVariant {
                    name: variant.name.clone(),
                    kind: match &variant.kind {
                        TypeEnumVariantKind::Unit => TypeEnumVariantKind::Unit,
                        TypeEnumVariantKind::Tuple(payload) => TypeEnumVariantKind::Tuple(
                            payload
                                .iter()
                                .map(|ty| substitute_type(ty, mapping))
                                .collect(),
                        ),
                        TypeEnumVariantKind::Struct(fields) => TypeEnumVariantKind::Struct(
                            fields
                                .iter()
                                .map(|field| {
                                    TypeRecordField::new(
                                        field.name.clone(),
                                        substitute_type(&field.ty, mapping),
                                    )
                                })
                                .collect(),
                        ),
                    },
                })
                .collect(),
        }),
        Type::Int => Type::Int,
        Type::Bool => Type::Bool,
        Type::Unit => Type::Unit,
    }
}

/// Return `true` if `var` appears anywhere inside `ty`, expanding substitutions as needed.
fn occurs_in(var: TypeVarId, ty: &Type, subst: &HashMap<TypeVarId, Type>) -> bool {
    match ty {
        Type::Var(id) => {
            if id == &var {
                true
            } else {
                subst
                    .get(id)
                    .map(|ty| occurs_in(var, ty, subst))
                    .unwrap_or(false)
            }
        }
        Type::Function(params, result) => {
            params.iter().any(|t| occurs_in(var, t, subst)) || occurs_in(var, result, subst)
        }
        Type::Tuple(items) => items.iter().any(|t| occurs_in(var, t, subst)),
        Type::Record(record) => record
            .fields
            .iter()
            .any(|field| occurs_in(var, &field.ty, subst)),
        Type::Enum(enum_type) => enum_type
            .variants
            .iter()
            .any(|variant| match &variant.kind {
                TypeEnumVariantKind::Unit => false,
                TypeEnumVariantKind::Tuple(payload) => {
                    payload.iter().any(|ty| occurs_in(var, ty, subst))
                }
                TypeEnumVariantKind::Struct(fields) => {
                    fields.iter().any(|field| occurs_in(var, &field.ty, subst))
                }
            }),
        Type::Int | Type::Bool | Type::Unit => false,
    }
}

/// Collect all free type variables present in `ty`.
pub(crate) fn free_type_vars_type(ty: &Type) -> HashSet<TypeVarId> {
    let mut set = HashSet::new();
    collect_free_type_vars(ty, &mut set);
    set
}

/// Helper for `free_type_vars_type` that walks the type tree.
fn collect_free_type_vars(ty: &Type, set: &mut HashSet<TypeVarId>) {
    match ty {
        Type::Var(id) => {
            set.insert(*id);
        }
        Type::Function(params, result) => {
            for ty in params {
                collect_free_type_vars(ty, set);
            }
            collect_free_type_vars(result, set);
        }
        Type::Tuple(items) => {
            for ty in items {
                collect_free_type_vars(ty, set);
            }
        }
        Type::Record(record) => {
            for field in &record.fields {
                collect_free_type_vars(&field.ty, set);
            }
        }
        Type::Enum(enum_type) => {
            for variant in &enum_type.variants {
                match &variant.kind {
                    TypeEnumVariantKind::Unit => {}
                    TypeEnumVariantKind::Tuple(payload) => {
                        for ty in payload {
                            collect_free_type_vars(ty, set);
                        }
                    }
                    TypeEnumVariantKind::Struct(fields) => {
                        for field in fields {
                            collect_free_type_vars(&field.ty, set);
                        }
                    }
                }
            }
        }
        Type::Int | Type::Bool | Type::Unit => {}
    }
}

fn enum_payload_kind_from_variant(kind: &EnumVariantInfoKind) -> EnumPayloadKind {
    match kind {
        EnumVariantInfoKind::Unit => EnumPayloadKind::unit(),
        EnumVariantInfoKind::Tuple(payload) => EnumPayloadKind::tuple(payload.len()),
        EnumVariantInfoKind::Struct(fields) => EnumPayloadKind::struct_payload(fields.len()),
    }
}

fn enum_payload_kind_from_constructor(payload: &EnumConstructorPayload) -> EnumPayloadKind {
    match payload {
        EnumConstructorPayload::Unit => EnumPayloadKind::unit(),
        EnumConstructorPayload::Tuple(values) => EnumPayloadKind::tuple(values.len()),
        EnumConstructorPayload::Struct(fields) => EnumPayloadKind::struct_payload(fields.len()),
    }
}

fn enum_payload_kind_from_pattern(payload: &EnumPatternPayload) -> EnumPayloadKind {
    match payload {
        EnumPatternPayload::Unit => EnumPayloadKind::unit(),
        EnumPatternPayload::Tuple(patterns) => EnumPayloadKind::tuple(patterns.len()),
        EnumPatternPayload::Struct(fields) => EnumPayloadKind::struct_payload(fields.len()),
    }
}

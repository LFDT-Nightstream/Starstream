use pretty::RcDoc;
use starstream_types::{
    AbiDef, AbiPart, BinaryOp, Block, Comment, CommentMap, Definition, EventDef, Expr,
    FunctionDef, FunctionExport, FunctionParam, Literal, Spanned, Statement, TypeAnnotation,
    UnaryOp, UtxoDef, UtxoGlobal, UtxoPart,
    ast::{
        EnumConstructorPayload, EnumDef, EnumPatternPayload, EnumVariant, EnumVariantPayload,
        Identifier, ImportDef, ImportItems, ImportNamedItem, ImportSource, MatchArm, Pattern,
        Program, StructDef, StructField, StructLiteralField, StructPatternField,
    },
};
use std::fmt;

pub fn program(
    program: &Program,
    source: &str,
    comments: &CommentMap,
) -> Result<String, fmt::Error> {
    let mut out = String::new();

    program_to_doc(program, source, comments).render_fmt(80, &mut out)?;

    Ok(out)
}

pub fn statement(statement: &Statement) -> Result<String, fmt::Error> {
    let mut out = String::new();
    let empty_comments = CommentMap::new();

    statement_to_doc(statement, "", &empty_comments).render_fmt(80, &mut out)?;

    Ok(out)
}

pub fn expression(expr: &Expr) -> Result<String, fmt::Error> {
    let mut out = String::new();
    let empty_comments = CommentMap::new();

    expr_to_doc(expr, "", &empty_comments).render_fmt(80, &mut out)?;

    Ok(out)
}

fn program_to_doc<'a>(
    program: &Program,
    source: &'a str,
    comments: &CommentMap,
) -> RcDoc<'a, ()> {
    // Sort definitions: imports first, then everything else
    let mut definitions: Vec<_> = program.definitions.iter().collect();
    definitions.sort_by_key(|def| !matches!(def.node, Definition::Import(_)));

    let mut result = if let Some(shebang) = &program.shebang {
        comment_to_doc(shebang, source)
    } else {
        RcDoc::nil()
    };

    let mut prev_end: usize = 0;
    let mut prev_had_inline_or_trailing_comment = false;

    for (i, def) in definitions.iter().enumerate() {
        // The span now only covers the definition content (not comments)
        let content_span = def.span;

        // Get comments between previous definition end and this definition's start
        let comments_before_def = comments.comments_between(prev_end, content_span, source);

        // Add separator between definitions
        if i > 0 {
            // If prev had inline comment, comment_to_doc already added a hardline,
            // so only need one more for blank line. Otherwise need two.
            if prev_had_inline_or_trailing_comment {
                result = result.append(RcDoc::hardline());
            } else {
                result = result
                    .append(RcDoc::hardline())
                    .append(RcDoc::hardline());
            }
        }

        // Add comments that belong before this definition
        for c in &comments_before_def {
            result = result.append(comment_to_doc(c, source));
        }

        // Format the definition itself
        result = result.append(definition_to_doc(&def.node, source, comments));

        // Check for inline comment after this definition (on same line)
        let inline = comments.inline_comment_after(content_span, source);
        if let Some(inline_comment) = inline {
            result = result
                .append(RcDoc::space())
                .append(comment_to_doc(inline_comment, source));
            prev_had_inline_or_trailing_comment = true;
        } else {
            prev_had_inline_or_trailing_comment = false;
        }

        // Update prev_end to be after any inline comment, so it's not picked up
        // by comments_between for the next definition
        prev_end = inline
            .map(|c| c.0.end)
            .unwrap_or(def.span.end);
    }

    result.append(RcDoc::hardline())
}

fn comment_to_doc<'a>(comment: &Comment, source: &'a str) -> RcDoc<'a, ()> {
    let text = source.get(comment.0.start..comment.0.end).unwrap_or("");
    // Strip trailing newline - document layout handles newlines
    let text = text.strip_suffix('\n').unwrap_or(text);
    RcDoc::text(text).append(RcDoc::hardline())
}

fn spanned<'a, T, F: FnOnce(&T) -> RcDoc<'a, ()>>(
    spanned: &Spanned<T>,
    _source: &'a str,
    f: F,
) -> RcDoc<'a, ()> {
    // Comments are now handled via CommentMap, not stored in Spanned
    f(&spanned.node)
}

fn definition_to_doc<'a>(
    definition: &Definition,
    source: &'a str,
    comments: &CommentMap,
) -> RcDoc<'a, ()> {
    match definition {
        Definition::Import(import) => import_to_doc(import, source),
        Definition::Function(function) => function_to_doc(function, source, comments),
        Definition::Struct(definition) => struct_definition_to_doc(definition, source),
        Definition::Enum(definition) => enum_definition_to_doc(definition, source),
        Definition::Utxo(definition) => utxo_definition_to_doc(definition, source, comments),
        Definition::Abi(definition) => abi_definition_to_doc(definition, source),
    }
}

fn import_to_doc<'a>(import: &ImportDef, source: &'a str) -> RcDoc<'a, ()> {
    RcDoc::text("import")
        .append(RcDoc::space())
        .append(import_items_to_doc(&import.items, source))
        .append(RcDoc::space())
        .append(RcDoc::text("from"))
        .append(RcDoc::space())
        .append(import_source_to_doc(&import.from, source))
        .append(RcDoc::text(";"))
}

fn import_items_to_doc<'a>(items: &ImportItems, source: &'a str) -> RcDoc<'a, ()> {
    match items {
        ImportItems::Named(named) => {
            if named.is_empty() {
                RcDoc::text("{ }")
            } else {
                RcDoc::text("{ ")
                    .append(RcDoc::intersperse(
                        named
                            .iter()
                            .map(|item| import_named_item_to_doc(item, source)),
                        RcDoc::text(", "),
                    ))
                    .append(RcDoc::text(" }"))
            }
        }
        ImportItems::Namespace(ident) => identifier_to_doc(ident, source),
    }
}

fn import_named_item_to_doc<'a>(item: &ImportNamedItem, source: &'a str) -> RcDoc<'a, ()> {
    if item.imported.name == item.local.name {
        identifier_to_doc(&item.imported, source)
    } else {
        identifier_to_doc(&item.imported, source)
            .append(RcDoc::space())
            .append(RcDoc::text("as"))
            .append(RcDoc::space())
            .append(identifier_to_doc(&item.local, source))
    }
}

fn import_source_to_doc<'a>(source_path: &ImportSource, source: &'a str) -> RcDoc<'a, ()> {
    let mut doc = identifier_to_doc(&source_path.namespace, source)
        .append(RcDoc::text(":"))
        .append(identifier_to_doc(&source_path.package, source));

    if let Some(interface) = &source_path.interface {
        doc = doc
            .append(RcDoc::text("/"))
            .append(identifier_to_doc(interface, source));
    }

    doc
}

fn function_to_doc<'a>(
    function: &FunctionDef,
    source: &'a str,
    comments: &CommentMap,
) -> RcDoc<'a, ()> {
    let params = params_to_doc(&function.params, source);
    let mut doc = if let Some(export) = &function.export {
        function_export_to_doc(export, source).append(RcDoc::space())
    } else {
        RcDoc::nil()
    }
    .append("fn")
    .append(RcDoc::space())
    .append(identifier_to_doc(&function.name, source))
    .append(RcDoc::text("("))
    .append(params)
    .append(RcDoc::text(")"));

    if let Some(return_type) = &function.return_type {
        doc = doc
            .append(RcDoc::space())
            .append(RcDoc::text("->"))
            .append(RcDoc::space())
            .append(type_annotation_to_doc(return_type, source));
    }

    doc.append(RcDoc::space())
        .append(block_to_doc(&function.body, source, comments))
}

fn function_export_to_doc<'a>(export: &FunctionExport, _source: &'a str) -> RcDoc<'a, ()> {
    match export {
        FunctionExport::Script => RcDoc::text("script"),
    }
}

fn struct_definition_to_doc<'a>(definition: &StructDef, source: &'a str) -> RcDoc<'a, ()> {
    RcDoc::text("struct")
        .append(RcDoc::space())
        .append(identifier_to_doc(&definition.name, source))
        .append(RcDoc::space())
        .append(struct_fields_to_doc(&definition.fields, source))
}

fn struct_fields_to_doc<'a>(fields: &[StructField], source: &'a str) -> RcDoc<'a, ()> {
    if fields.is_empty() {
        RcDoc::text("{ }")
    } else {
        let entries = RcDoc::intersperse(
            fields.iter().map(|field| {
                identifier_to_doc(&field.name, source)
                    .append(RcDoc::text(": "))
                    .append(type_annotation_to_doc(&field.ty, source))
                    .append(RcDoc::text(","))
            }),
            RcDoc::line(),
        );

        RcDoc::text("{")
            .append(RcDoc::line().append(entries).nest(INDENT))
            .append(RcDoc::line())
            .append(RcDoc::text("}"))
    }
}

fn enum_definition_to_doc<'a>(definition: &EnumDef, source: &'a str) -> RcDoc<'a, ()> {
    RcDoc::text("enum")
        .append(RcDoc::space())
        .append(identifier_to_doc(&definition.name, source))
        .append(RcDoc::space())
        .append(enum_variants_to_doc(&definition.variants, source))
}

fn enum_variants_to_doc<'a>(variants: &[EnumVariant], source: &'a str) -> RcDoc<'a, ()> {
    if variants.is_empty() {
        RcDoc::text("{ }")
    } else {
        let entries = RcDoc::intersperse(
            variants
                .iter()
                .map(|variant| enum_variant_to_doc(variant, source).append(RcDoc::text(","))),
            RcDoc::line(),
        );

        RcDoc::text("{")
            .append(RcDoc::line().append(entries).nest(INDENT))
            .append(RcDoc::line())
            .append(RcDoc::text("}"))
    }
}

fn enum_variant_to_doc<'a>(variant: &EnumVariant, source: &'a str) -> RcDoc<'a, ()> {
    match &variant.payload {
        EnumVariantPayload::Unit => identifier_to_doc(&variant.name, source),
        EnumVariantPayload::Tuple(types) => {
            if types.is_empty() {
                identifier_to_doc(&variant.name, source).append(RcDoc::text("()"))
            } else {
                let payload = RcDoc::intersperse(
                    types.iter().map(|x| type_annotation_to_doc(x, source)),
                    RcDoc::text(", "),
                );
                identifier_to_doc(&variant.name, source)
                    .append(RcDoc::text("("))
                    .append(payload)
                    .append(RcDoc::text(")"))
            }
        }
        EnumVariantPayload::Struct(fields) => {
            let body = if fields.len() < 3 {
                inline_struct_fields_to_doc(fields, source)
            } else {
                struct_fields_to_doc(fields, source)
            };
            identifier_to_doc(&variant.name, source)
                .append(RcDoc::space())
                .append(body)
        }
    }
}

fn inline_struct_fields_to_doc<'a>(fields: &[StructField], source: &'a str) -> RcDoc<'a, ()> {
    RcDoc::text("{ ")
        .append(RcDoc::intersperse(
            fields.iter().map(|field| {
                identifier_to_doc(&field.name, source)
                    .append(RcDoc::text(": "))
                    .append(type_annotation_to_doc(&field.ty, source))
            }),
            RcDoc::text(", "),
        ))
        .append(RcDoc::text(" }"))
}

fn params_to_doc<'a>(params: &[FunctionParam], source: &'a str) -> RcDoc<'a, ()> {
    if params.is_empty() {
        RcDoc::nil()
    } else {
        RcDoc::intersperse(
            params.iter().map(|param| {
                identifier_to_doc(&param.name, source)
                    .append(RcDoc::text(": "))
                    .append(type_annotation_to_doc(&param.ty, source))
            }),
            RcDoc::text(", "),
        )
    }
}

fn utxo_definition_to_doc<'a>(
    definition: &UtxoDef,
    source: &'a str,
    _comments: &CommentMap,
) -> RcDoc<'a, ()> {
    RcDoc::text("utxo")
        .append(RcDoc::space())
        .append(identifier_to_doc(&definition.name, source))
        .append(RcDoc::space())
        .append("{")
        .append(
            RcDoc::line()
                .append(RcDoc::intersperse(
                    definition.parts.iter().map(|x| utxo_part_to_doc(x, source)),
                    RcDoc::line(),
                ))
                .nest(INDENT),
        )
        .append(RcDoc::line())
        .append("}")
}

fn utxo_part_to_doc<'a>(part: &UtxoPart, source: &'a str) -> RcDoc<'a, ()> {
    match part {
        UtxoPart::Storage(vars) => RcDoc::text("storage")
            .append(RcDoc::space())
            .append("{")
            .append(
                RcDoc::line()
                    .append(RcDoc::intersperse(
                        vars.iter().map(|x| utxo_global_to_doc(x, source)),
                        RcDoc::line(),
                    ))
                    .nest(INDENT),
            )
            .append(RcDoc::line())
            .append("}"),
    }
}

fn utxo_global_to_doc<'a>(decl: &UtxoGlobal, source: &'a str) -> RcDoc<'a, ()> {
    RcDoc::text("let")
        .append(RcDoc::space())
        .append("mut")
        .append(RcDoc::space())
        .append(identifier_to_doc(&decl.name, source))
        .append(":")
        .append(RcDoc::space())
        .append(type_annotation_to_doc(&decl.ty, source))
        .append(RcDoc::text(";"))
}

fn abi_definition_to_doc<'a>(definition: &AbiDef, source: &'a str) -> RcDoc<'a, ()> {
    if definition.parts.is_empty() {
        RcDoc::text("abi")
            .append(RcDoc::space())
            .append(identifier_to_doc(&definition.name, source))
            .append(RcDoc::space())
            .append(RcDoc::text("{ }"))
    } else {
        let parts = RcDoc::intersperse(
            definition.parts.iter().map(|x| abi_part_to_doc(x, source)),
            RcDoc::line(),
        );

        RcDoc::text("abi")
            .append(RcDoc::space())
            .append(identifier_to_doc(&definition.name, source))
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(RcDoc::line().append(parts).nest(INDENT))
            .append(RcDoc::line())
            .append(RcDoc::text("}"))
    }
}

fn abi_part_to_doc<'a>(part: &AbiPart, source: &'a str) -> RcDoc<'a, ()> {
    match part {
        AbiPart::Event(event) => event_definition_to_doc(event, source),
    }
}

fn event_definition_to_doc<'a>(event: &EventDef, source: &'a str) -> RcDoc<'a, ()> {
    let params = params_to_doc(&event.params, source);
    RcDoc::text("event")
        .append(RcDoc::space())
        .append(identifier_to_doc(&event.name, source))
        .append(RcDoc::text("("))
        .append(params)
        .append(RcDoc::text(");"))
}

fn statement_to_doc<'a>(
    statement: &Statement,
    source: &'a str,
    comments: &CommentMap,
) -> RcDoc<'a, ()> {
    match statement {
        Statement::VariableDeclaration {
            mutable,
            name,
            ty,
            value,
        } => RcDoc::text("let")
            .append(RcDoc::space())
            .append(if *mutable {
                RcDoc::text("mut").append(RcDoc::space())
            } else {
                RcDoc::nil()
            })
            .append(identifier_to_doc(name, source))
            .append(if let Some(ty) = ty {
                RcDoc::text(":")
                    .append(RcDoc::space())
                    .append(type_annotation_to_doc(ty, source))
            } else {
                RcDoc::nil()
            })
            .append(RcDoc::space())
            .append(RcDoc::text("="))
            .append(RcDoc::space())
            .append(spanned(value, source, |node| expr_to_doc(node, source, comments)))
            .append(RcDoc::text(";")),
        Statement::Assignment { target, value } => identifier_to_doc(target, source)
            .append(RcDoc::space())
            .append(RcDoc::text("="))
            .append(RcDoc::space())
            .append(spanned(value, source, |node| expr_to_doc(node, source, comments)))
            .append(RcDoc::text(";")),
        Statement::While { condition, body } => RcDoc::text("while")
            .append(RcDoc::space())
            .append(parened_expr(condition, source, comments))
            .append(RcDoc::space())
            .append(block_to_doc(body, source, comments)),
        Statement::Expression(expr) => {
            spanned(expr, source, |node| expr_to_doc(node, source, comments))
                .append(RcDoc::text(";"))
        }
        Statement::Return(Some(expr)) => RcDoc::text("return")
            .append(RcDoc::space())
            .append(spanned(expr, source, |node| expr_to_doc(node, source, comments)))
            .append(RcDoc::text(";")),
        Statement::Return(None) => RcDoc::text("return;"),
    }
}

fn block_to_doc<'a>(block: &Block, source: &'a str, comments: &CommentMap) -> RcDoc<'a, ()> {
    if block.statements.is_empty() && block.tail_expression.is_none() {
        RcDoc::text("{ }")
    } else {
        // Collect all items (statements + optional tail expression) with their spans
        let mut span_items: Vec<(starstream_types::Span, RcDoc<'a, ()>)> = block
            .statements
            .iter()
            .map(|x| {
                (
                    x.span,
                    spanned(x, source, |node| statement_to_doc(node, source, comments)),
                )
            })
            .collect();

        if let Some(expr) = &block.tail_expression {
            span_items.push((
                expr.span,
                spanned(expr, source, |node| expr_to_doc(node, source, comments)),
            ));
        }

        // Build body with comments between items
        let mut body = RcDoc::nil();
        // Use the block's span to find comments at the very start of the block.
        // block.span.start is the position of `{`, so comments after it and before
        // the first statement will be found.
        let mut prev_end: usize = block.span.start;
        let mut first = true;

        for (span, item_doc) in span_items {
            // Find comments between previous item and this one
            let comments_before = comments.comments_between(prev_end, span, source);

            if !first {
                body = body.append(RcDoc::line());
            }

            // Add comments before this item
            for c in comments_before {
                body = body.append(comment_to_doc(c, source));
            }

            body = body.append(item_doc);

            // Check for inline comment after this item (on same line)
            let inline = comments.inline_comment_after(span, source);
            if let Some(inline_comment) = inline {
                let text = source
                    .get(inline_comment.0.start..inline_comment.0.end)
                    .unwrap_or("");
                let text = text.strip_suffix('\n').unwrap_or(text);
                body = body.append(RcDoc::space()).append(RcDoc::text(text));
            }

            // Update prev_end to be after any inline comment
            prev_end = inline.map(|c| c.0.end).unwrap_or(span.end);
            first = false;
        }

        RcDoc::text("{")
            .append(RcDoc::line().append(body).nest(INDENT))
            .append(RcDoc::line())
            .append(RcDoc::text("}"))
    }
}

fn parened_expr<'a>(expr: &Spanned<Expr>, source: &'a str, comments: &CommentMap) -> RcDoc<'a, ()> {
    spanned(expr, source, |node| {
        RcDoc::text("(")
            .append(expr_to_doc(node, source, comments))
            .append(RcDoc::text(")"))
    })
}

fn identifier_to_doc<'a>(identifier: &Identifier, _source: &'a str) -> RcDoc<'a, ()> {
    RcDoc::text(identifier.name.clone())
}

fn type_annotation_to_doc<'a>(annotation: &TypeAnnotation, source: &'a str) -> RcDoc<'a, ()> {
    let mut doc = identifier_to_doc(&annotation.name, source);
    if !annotation.generics.is_empty() {
        let generics = RcDoc::intersperse(
            annotation
                .generics
                .iter()
                .map(|x| type_annotation_to_doc(x, source)),
            RcDoc::text(", "),
        );
        doc = doc
            .append(RcDoc::text("<"))
            .append(generics)
            .append(RcDoc::text(">"));
    }
    doc
}

fn struct_literal_expr_to_doc<'a>(
    name: &Identifier,
    fields: &[StructLiteralField],
    source: &'a str,
    comments: &CommentMap,
) -> RcDoc<'a, ()> {
    identifier_to_doc(name, source)
        .append(RcDoc::space())
        .append(struct_literal_fields_to_doc(fields, source, comments))
}

fn struct_literal_fields_to_doc<'a>(
    fields: &[StructLiteralField],
    source: &'a str,
    comments: &CommentMap,
) -> RcDoc<'a, ()> {
    if fields.is_empty() {
        RcDoc::text("{ }")
    } else {
        let body = RcDoc::intersperse(
            fields.iter().map(|field| {
                identifier_to_doc(&field.name, source)
                    .append(RcDoc::text(": "))
                    .append(spanned(&field.value, source, |node| {
                        expr_to_doc(node, source, comments)
                    }))
                    .append(RcDoc::text(","))
            }),
            RcDoc::line(),
        );

        RcDoc::text("{")
            .append(RcDoc::line().append(body).nest(INDENT))
            .append(RcDoc::line())
            .append(RcDoc::text("}"))
    }
}

fn enum_constructor_to_doc<'a>(
    enum_name: &Identifier,
    variant: &Identifier,
    payload: &EnumConstructorPayload,
    source: &'a str,
    comments: &CommentMap,
) -> RcDoc<'a, ()> {
    let mut doc = identifier_to_doc(enum_name, source)
        .append(RcDoc::text("::"))
        .append(identifier_to_doc(variant, source));

    doc = match payload {
        EnumConstructorPayload::Unit => doc,
        EnumConstructorPayload::Tuple(values) => {
            if values.is_empty() {
                doc.append(RcDoc::text("()"))
            } else {
                let args = RcDoc::intersperse(
                    values.iter().map(|expr| {
                        spanned(expr, source, |node| expr_to_doc(node, source, comments))
                    }),
                    RcDoc::text(", "),
                );
                doc.append(RcDoc::text("("))
                    .append(args)
                    .append(RcDoc::text(")"))
            }
        }
        EnumConstructorPayload::Struct(fields) => doc
            .append(RcDoc::space())
            .append(struct_literal_fields_to_doc(fields, source, comments)),
    };

    doc
}

fn match_expr_to_doc<'a>(
    scrutinee: &Spanned<Expr>,
    arms: &[MatchArm],
    source: &'a str,
    comments: &CommentMap,
) -> RcDoc<'a, ()> {
    let doc = RcDoc::text("match")
        .append(RcDoc::space())
        .append(spanned(scrutinee, source, |node| {
            expr_to_doc(node, source, comments)
        }))
        .append(RcDoc::space());

    if arms.is_empty() {
        doc.append(RcDoc::text("{ }"))
    } else {
        // Build arms with comments between them
        let mut body = RcDoc::nil();
        // Start from after the scrutinee for finding comments before first arm
        let mut prev_end = scrutinee.span.end;
        let mut first = true;

        for arm in arms {
            // Find comments between previous arm and this one
            let comments_before = comments.comments_between(prev_end, arm.span, source);

            if !first {
                body = body.append(RcDoc::line());
            }

            // Add comments before this arm
            for c in comments_before {
                body = body.append(comment_to_doc(c, source));
            }

            body = body
                .append(match_arm_to_doc(arm, source, comments))
                .append(RcDoc::text(","));
            prev_end = arm.span.end;
            first = false;
        }

        doc.append(
            RcDoc::text("{")
                .append(RcDoc::line().append(body).nest(INDENT))
                .append(RcDoc::line())
                .append(RcDoc::text("}")),
        )
    }
}

fn match_arm_to_doc<'a>(arm: &MatchArm, source: &'a str, comments: &CommentMap) -> RcDoc<'a, ()> {
    pattern_to_doc(&arm.pattern, source)
        .append(RcDoc::space())
        .append(RcDoc::text("=>"))
        .append(RcDoc::space())
        .append(block_to_doc(&arm.body, source, comments))
}

fn pattern_to_doc<'a>(pattern: &Pattern, source: &'a str) -> RcDoc<'a, ()> {
    match pattern {
        Pattern::Binding(name) => identifier_to_doc(name, source),
        Pattern::Wildcard { .. } => RcDoc::text("_"),
        Pattern::Literal { value, .. } => literal_to_doc(value, source),
        Pattern::Struct { name, fields } => identifier_to_doc(name, source)
            .append(RcDoc::space())
            .append(struct_pattern_fields_to_doc(fields, source)),
        Pattern::EnumVariant {
            enum_name,
            variant,
            payload,
        } => {
            let doc = identifier_to_doc(enum_name, source)
                .append(RcDoc::text("::"))
                .append(identifier_to_doc(variant, source));
            match payload {
                EnumPatternPayload::Unit => doc,
                EnumPatternPayload::Tuple(items) => {
                    if items.is_empty() {
                        doc.append(RcDoc::text("()"))
                    } else {
                        let inner = RcDoc::intersperse(
                            items.iter().map(|x| pattern_to_doc(x, source)),
                            RcDoc::text(", "),
                        );
                        doc.append(RcDoc::text("("))
                            .append(inner)
                            .append(RcDoc::text(")"))
                    }
                }
                EnumPatternPayload::Struct(fields) => doc
                    .append(RcDoc::space())
                    .append(struct_pattern_fields_to_doc(fields, source)),
            }
        }
    }
}

fn struct_pattern_fields_to_doc<'a>(
    fields: &[StructPatternField],
    source: &'a str,
) -> RcDoc<'a, ()> {
    if fields.is_empty() {
        RcDoc::text("{ }")
    } else {
        let items = RcDoc::intersperse(
            fields.iter().map(|field| {
                if let Pattern::Binding(binding) = field.pattern.as_ref()
                    && binding.name == field.name.name
                {
                    return identifier_to_doc(&field.name, source);
                }

                identifier_to_doc(&field.name, source)
                    .append(RcDoc::text(": "))
                    .append(pattern_to_doc(&field.pattern, source))
            }),
            RcDoc::text(", "),
        );
        RcDoc::text("{")
            .append(RcDoc::space())
            .append(items)
            .append(RcDoc::space())
            .append(RcDoc::text("}"))
    }
}
fn expr_to_doc<'a>(expr: &Expr, source: &'a str, comments: &CommentMap) -> RcDoc<'a, ()> {
    expr_with_prec(expr, PREC_LOWEST, ChildPosition::Top, source, comments)
}

fn expr_with_prec<'a>(
    expr: &Expr,
    parent_prec: u8,
    position: ChildPosition,
    source: &'a str,
    comments: &CommentMap,
) -> RcDoc<'a, ()> {
    match expr {
        Expr::Grouping(inner) => RcDoc::text("(")
            .append(spanned(inner, source, |node| {
                expr_with_prec(node, PREC_LOWEST, ChildPosition::Top, source, comments)
            }))
            .append(RcDoc::text(")")),
        _ => {
            let prec = precedence(expr);
            let doc = match expr {
                Expr::Literal(literal) => literal_to_doc(literal, source),
                Expr::Identifier(identifier) => identifier_to_doc(identifier, source),
                Expr::Unary { op, expr } => {
                    let operand = spanned(expr, source, |node| {
                        expr_with_prec(node, prec, ChildPosition::Unary, source, comments)
                    });

                    RcDoc::text(unary_op_str(op)).append(operand)
                }
                Expr::Binary { op, left, right } => {
                    let left_doc = spanned(left, source, |node| {
                        expr_with_prec(node, prec, ChildPosition::Left, source, comments).group()
                    });
                    let right_doc = spanned(right, source, |node| {
                        expr_with_prec(node, prec, ChildPosition::Right, source, comments).group()
                    });

                    left_doc
                        .append(RcDoc::space())
                        .append(RcDoc::text(binary_op_str(op)))
                        .append(RcDoc::space())
                        .append(right_doc)
                }
                Expr::Grouping(_) => unreachable!(),
                Expr::StructLiteral { name, fields } => {
                    struct_literal_expr_to_doc(name, fields, source, comments)
                }
                Expr::FieldAccess { target, field } => {
                    let receiver = spanned(target, source, |node| {
                        expr_with_prec(node, PREC_PRIMARY, ChildPosition::Top, source, comments)
                    });
                    receiver
                        .append(RcDoc::text("."))
                        .append(identifier_to_doc(field, source))
                }
                Expr::EnumConstructor {
                    enum_name,
                    variant,
                    payload,
                } => enum_constructor_to_doc(enum_name, variant, payload, source, comments),
                Expr::Block(block) => block_to_doc(block, source, comments),
                Expr::If {
                    branches,
                    else_branch,
                } => {
                    let mut out = RcDoc::nil();
                    for (i, (condition, block)) in branches.iter().enumerate() {
                        if i > 0 {
                            out = out
                                .append(RcDoc::space())
                                .append("else")
                                .append(RcDoc::space());
                        }
                        out = out
                            .append("if")
                            .append(RcDoc::space())
                            .append(parened_expr(condition, source, comments))
                            .append(RcDoc::space())
                            .append(block_to_doc(block, source, comments));
                    }

                    if let Some(else_branch) = else_branch {
                        out.append(RcDoc::space())
                            .append(RcDoc::text("else"))
                            .append(RcDoc::space())
                            .append(block_to_doc(else_branch, source, comments))
                    } else {
                        out
                    }
                }
                Expr::Match { scrutinee, arms } => {
                    match_expr_to_doc(scrutinee, arms, source, comments)
                }
                Expr::Call { callee, args } => {
                    let callee_doc = expr_with_prec(
                        &callee.node,
                        PREC_FIELD_ACCESS,
                        ChildPosition::Left,
                        source,
                        comments,
                    );

                    let args_doc = RcDoc::intersperse(
                        args.iter().map(|arg| {
                            expr_with_prec(
                                &arg.node,
                                PREC_LOWEST,
                                ChildPosition::Top,
                                source,
                                comments,
                            )
                        }),
                        RcDoc::text(",").append(RcDoc::space()),
                    );

                    callee_doc
                        .append(RcDoc::text("("))
                        .append(args_doc)
                        .append(RcDoc::text(")"))
                }
                Expr::Emit { event, args } => {
                    let args_doc = args.iter().map(|arg| {
                        expr_with_prec(
                            &arg.node,
                            PREC_LOWEST,
                            ChildPosition::Top,
                            source,
                            comments,
                        )
                    });

                    let args_doc =
                        RcDoc::intersperse(args_doc, RcDoc::text(",").append(RcDoc::space()));

                    RcDoc::text("emit")
                        .append(RcDoc::space())
                        .append(identifier_to_doc(event, source))
                        .append(RcDoc::text("("))
                        .append(args_doc)
                        .append(RcDoc::text(")"))
                }
                Expr::Raise { expr } => RcDoc::text("raise").append(RcDoc::space()).append(
                    spanned(expr, source, |node| {
                        expr_with_prec(node, PREC_LOWEST, ChildPosition::Top, source, comments)
                    }),
                ),
                Expr::Runtime { expr } => {
                    RcDoc::text("runtime")
                        .append(RcDoc::space())
                        .append(spanned(expr, source, |node| {
                            expr_with_prec(node, PREC_LOWEST, ChildPosition::Top, source, comments)
                        }))
                }
            };

            if needs_parentheses(prec, parent_prec, position) {
                RcDoc::text("(").append(doc).append(RcDoc::text(")"))
            } else {
                doc
            }
        }
    }
}

fn literal_to_doc<'a>(literal: &Literal, _source: &'a str) -> RcDoc<'a, ()> {
    match literal {
        Literal::Integer(value) => RcDoc::as_string(*value),
        Literal::Boolean(value) => RcDoc::text(if *value { "true" } else { "false" }),
        Literal::Unit => RcDoc::text("()"),
    }
}

fn binary_op_str(op: &BinaryOp) -> &'static str {
    match op {
        BinaryOp::Multiply => "*",
        BinaryOp::Divide => "/",
        BinaryOp::Remainder => "%",
        BinaryOp::Add => "+",
        BinaryOp::Subtract => "-",
        BinaryOp::Less => "<",
        BinaryOp::LessEqual => "<=",
        BinaryOp::Greater => ">",
        BinaryOp::GreaterEqual => ">=",
        BinaryOp::Equal => "==",
        BinaryOp::NotEqual => "!=",
        BinaryOp::And => "&&",
        BinaryOp::Or => "||",
    }
}

fn unary_op_str(op: &UnaryOp) -> &'static str {
    match op {
        UnaryOp::Negate => "-",
        UnaryOp::Not => "!",
    }
}

fn precedence(expr: &Expr) -> u8 {
    match expr {
        Expr::Literal(_)
        | Expr::Identifier(_)
        | Expr::StructLiteral { .. }
        | Expr::EnumConstructor { .. }
        | Expr::Emit { .. }
        | Expr::Raise { .. }
        | Expr::Runtime { .. } => PREC_PRIMARY,
        Expr::Grouping(inner) => precedence(&inner.node),
        Expr::Unary { .. } => PREC_UNARY,
        Expr::Binary { op, .. } => precedence_binary(op),
        Expr::FieldAccess { .. } | Expr::Call { .. } => PREC_FIELD_ACCESS,
        Expr::If { .. } | Expr::Block { .. } | Expr::Match { .. } => PREC_LOWEST,
    }
}

fn precedence_binary(op: &BinaryOp) -> u8 {
    match op {
        BinaryOp::Or => PREC_OR,
        BinaryOp::And => PREC_AND,
        BinaryOp::Equal | BinaryOp::NotEqual => PREC_EQUALITY,
        BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual => {
            PREC_COMPARISON
        }
        BinaryOp::Add | BinaryOp::Subtract => PREC_ADDITIVE,
        BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Remainder => PREC_MULTIPLICATIVE,
    }
}

fn needs_parentheses(prec: u8, parent_prec: u8, position: ChildPosition) -> bool {
    if matches!(position, ChildPosition::Top) {
        return false;
    }

    if prec < parent_prec {
        return true;
    }

    if prec == parent_prec {
        matches!(position, ChildPosition::Right)
    } else {
        false
    }
}

#[derive(Clone, Copy, Debug)]
enum ChildPosition {
    Top,
    Left,
    Right,
    Unary,
}

const PREC_LOWEST: u8 = 0;
const PREC_OR: u8 = 1;
const PREC_AND: u8 = 2;
const PREC_EQUALITY: u8 = 3;
const PREC_COMPARISON: u8 = 4;
const PREC_ADDITIVE: u8 = 5;
const PREC_MULTIPLICATIVE: u8 = 6;
const PREC_UNARY: u8 = 7;
const PREC_FIELD_ACCESS: u8 = 8;
const PREC_PRIMARY: u8 = 9;
const INDENT: isize = 4;

#[cfg(test)]
mod tests {
    use crate::parser;
    use indoc::indoc;

    fn formatted(code: &str) -> String {
        let parse_output = parser::parse_program(code);
        assert!(
            parse_output.errors().is_empty(),
            "program should parse without errors: {:?}",
            parse_output.errors,
        );

        let comments = parse_output.comment_map();
        let ast = parse_output.program.expect("program should parse");

        super::program(&ast, code, &comments).expect("formatting succeeds")
    }

    macro_rules! assert_format_snapshot {
        ($code:expr $(,)?) => {{
            let code = indoc! { $code };
            let formatted = formatted(code);

            insta::with_settings!({
                description => format!("Code:\n\n{}", code),
                omit_expression => true,
                prepend_module_to_snapshot => true,
            }, {
                insta::assert_snapshot!(formatted);
            });
        }};
    }

    #[test]
    fn control_flow() {
        assert_format_snapshot!(
            r#"
            fn main() {
                    let flag = true;
                if (    flag) {
                    let mut answer = 42;
                    while (answer < 100) {
                        answer = answer + 1;
                    }
                }else if( ! flag && true ){
                    answer = 17;
                } else {
                    answer = 0;
                }
            }
            "#,
        );
    }

    #[test]
    fn expressions() {
        assert_format_snapshot!(
            r#"
            fn main() {
                let mut value      = -(-5);
                value = (1 + 2) * (3 - 4) / 5;
                value = value + (10 / (3 + 2));
                result = (1 + 2 == 3) && !(false || true);
            }
            "#,
        );
    }

    #[test]
    fn nested_blocks() {
        assert_format_snapshot!(
            r#"
            fn main() {
                {
                    let x = 1;
                    {
                        let mut y = x + 2;

                        y = y * (x + y);
                    }
                }
            }
            "#,
        );
    }

    #[test]
    fn struct_field_access() {
        assert_format_snapshot!(
            r#"
            struct Point { x: i64 }

            fn add(a: Point, b: i64) -> i64 {
                a.x + b
            }
            "#,
        );
    }

    #[test]
    fn enum_variants_comma_separated() {
        assert_format_snapshot!(
            r#"
            enum Message {
                Ping,
                Pong { x: i64     },
            }
            "#,
        );
    }

    #[test]
    fn struct_definition_commas() {
        assert_format_snapshot!(
            r#"
            struct Point {
                x:   i64,
                y: i64
            }
            "#,
        );
    }

    #[test]
    fn enum_struct_variant_commas() {
        assert_format_snapshot!(
            r#"
            enum Message {
                Ping,
                Pong {
                    x: i64,
                    y: i64
                }
            }
            "#,
        );
    }

    #[test]
    fn match_arms_with_commas_and_punning() {
        assert_format_snapshot!(
            r#"
            struct Point {
                x: i64,
            }

            enum Message {
                Ping,
                Pong {
                    x: i64,
                },
            }

            fn add(a: Point, b: Message) -> i64 {
                match b {
                    Message::Ping => {
                        a.x
                    },
                    Message::Pong { x } => {
                        x + a.x
                    }
                }
            }
            "#,
        );
    }

    #[test]
    fn comments() {
        assert_format_snapshot!(
            r#"
            #!/usr/bin/env starstream
            fn main() {}
            "#,
        );
    }

    #[test]
    fn doc_comment_on_function() {
        assert_format_snapshot!(
            r#"
            struct Thing { x: i64 } // some comment

            /// Adds two integers together.
            fn add(a: i64, b: i64) -> i64 {
                a + b
            }
            "#,
        );
    }

    #[test]
    fn function_call_simple() {
        assert_format_snapshot!(
            r#"
            fn add(a: i64, b: i64) -> i64 {
                a + b
            }

            fn main() {
                let result = add(1, 2);
            }
            "#,
        );
    }

    #[test]
    fn function_call_no_args() {
        assert_format_snapshot!(
            r#"
            fn greet() -> i64 {
                42
            }

            fn main() {
                let value = greet();
            }
            "#,
        );
    }

    #[test]
    fn function_call_chained() {
        assert_format_snapshot!(
            r#"
            fn double(n: i64) -> i64 {
                n + n
            }

            fn main() {
                let value = double(double(5));
            }
            "#,
        );
    }

    #[test]
    fn function_call_with_field_access() {
        assert_format_snapshot!(
            r#"
            struct Point {
                x: i64,
            }

            fn get_x(p: Point) -> i64 {
                p.x
            }

            fn main() {
                let p = Point { x: 10 };
                let value = get_x(p);
            }
            "#,
        );
    }

    #[test]
    fn method_style_call_chain() {
        assert_format_snapshot!(
            r#"
            struct Builder {
                value: i64,
            }

            fn build(b: Builder) -> i64 {
                b.value
            }

            fn main() {
                let b = Builder { value: 5 };
                let result = build(b);
            }
            "#,
        );
    }

    #[test]
    fn import_named() {
        assert_format_snapshot!(
            r#"
            import {   blockHeight,   now   } from starstream:std/cardano;

            fn main() {}
            "#,
        );
    }

    #[test]
    fn import_with_alias() {
        assert_format_snapshot!(
            r#"
            import {   blockHeight   as   height   } from starstream:std/cardano;

            fn main() {}
            "#,
        );
    }

    #[test]
    fn import_namespace() {
        assert_format_snapshot!(
            r#"
            import context from starstream:std;

            fn main() {}
            "#,
        );
    }

    #[test]
    fn raise_expression() {
        assert_format_snapshot!(
            r#"
            fn main() {
                let height = raise   blockHeight();
            }
            "#,
        );
    }

    // Tests using CommentMap-based formatting

    fn formatted_with_comment_map(code: &str) -> String {
        let parse_output = parser::parse_program(code);
        assert!(
            parse_output.errors().is_empty(),
            "program should parse without errors: {:?}",
            parse_output.errors,
        );

        let comments = parse_output.comment_map();
        let ast = parse_output.program.expect("program should parse");

        super::program(&ast, code, &comments).expect("formatting succeeds")
    }

    #[test]
    fn comment_map_doc_comment_on_function() {
        let code = indoc! { r#"
            struct Thing { x: i64 } // some comment

            /// Adds two integers together.
            fn add(a: i64, b: i64) -> i64 {
                a + b
            }
            "#
        };

        let formatted = formatted_with_comment_map(code);

        insta::with_settings!({
            description => format!("Code:\n\n{}", code),
            omit_expression => true,
            prepend_module_to_snapshot => true,
        }, {
            insta::assert_snapshot!(formatted);
        });
    }

    #[test]
    fn comment_map_inline_and_block_comments() {
        let code = indoc! { r#"
            // Header comment
            struct A { x: i64 } // inline on A

            // Comment before B
            struct B { y: i64 }
            "#
        };

        let formatted = formatted_with_comment_map(code);

        insta::with_settings!({
            description => format!("Code:\n\n{}", code),
            omit_expression => true,
            prepend_module_to_snapshot => true,
        }, {
            insta::assert_snapshot!(formatted);
        });
    }

    #[test]
    fn comments_inside_function_body() {
        let code = indoc! { r#"
            fn foo() -> i64 {
                // This is a comment inside the body
                let x = 1;
                // Another comment
                x + 1
            }
            "#
        };

        let formatted = formatted_with_comment_map(code);

        insta::with_settings!({
            description => format!("Code:\n\n{}", code),
            omit_expression => true,
            prepend_module_to_snapshot => true,
        }, {
            insta::assert_snapshot!(formatted);
        });
    }

    #[test]
    fn inline_comment_no_extra_newline() {
        let code = indoc! { r#"
            struct A { x: i64 } // inline comment

            struct B { y: i64 }
            "#
        };

        let formatted = formatted_with_comment_map(code);

        insta::with_settings!({
            description => format!("Code:\n\n{}", code),
            omit_expression => true,
            prepend_module_to_snapshot => true,
        }, {
            insta::assert_snapshot!(formatted);
        });
    }

    #[test]
    fn multiple_functions_single_blank_line() {
        let code = indoc! { r#"
            fn foo() -> i64 {
                1
            }

            fn bar() -> i64 {
                2
            }

            fn baz() -> i64 {
                3
            }
            "#
        };

        let formatted = formatted_with_comment_map(code);

        insta::with_settings!({
            description => format!("Code:\n\n{}", code),
            omit_expression => true,
            prepend_module_to_snapshot => true,
        }, {
            insta::assert_snapshot!(formatted);
        });
    }

    #[test]
    fn doc_comment_then_function() {
        let code = indoc! { r#"
            /// This is documentation
            fn foo() -> i64 {
                1
            }

            /// More docs
            fn bar() -> i64 {
                2
            }
            "#
        };

        let formatted = formatted_with_comment_map(code);

        insta::with_settings!({
            description => format!("Code:\n\n{}", code),
            omit_expression => true,
            prepend_module_to_snapshot => true,
        }, {
            insta::assert_snapshot!(formatted);
        });
    }

    #[test]
    fn comment_at_start_of_function_body() {
        let code = indoc! { r#"
            fn foo() -> i64 {
                // Comment at the very start
                let x = 1;
                x + 1
            }
            "#
        };

        let formatted = formatted_with_comment_map(code);

        insta::with_settings!({
            description => format!("Code:\n\n{}", code),
            omit_expression => true,
            prepend_module_to_snapshot => true,
        }, {
            insta::assert_snapshot!(formatted);
        });
    }

    #[test]
    fn multiple_comments_at_start_of_body() {
        let code = indoc! { r#"
            fn foo() -> i64 {
                // First comment
                // Second comment
                let x = 1;
                x
            }
            "#
        };

        let formatted = formatted_with_comment_map(code);

        insta::with_settings!({
            description => format!("Code:\n\n{}", code),
            omit_expression => true,
            prepend_module_to_snapshot => true,
        }, {
            insta::assert_snapshot!(formatted);
        });
    }

    #[test]
    fn inline_comment_after_statement() {
        let code = indoc! { r#"
            fn foo() -> i64 {
                let x = 1; // initialize x
                let y = 2; // initialize y
                x + y
            }
            "#
        };

        let formatted = formatted_with_comment_map(code);

        insta::with_settings!({
            description => format!("Code:\n\n{}", code),
            omit_expression => true,
            prepend_module_to_snapshot => true,
        }, {
            insta::assert_snapshot!(formatted);
        });
    }

    #[test]
    fn doc_comment_on_struct() {
        let code = indoc! { r#"
            /// A point in 2D space.
            struct Point {
                x: i64,
                y: i64,
            }

            fn main() {}
            "#
        };

        let formatted = formatted_with_comment_map(code);

        insta::with_settings!({
            description => format!("Code:\n\n{}", code),
            omit_expression => true,
            prepend_module_to_snapshot => true,
        }, {
            insta::assert_snapshot!(formatted);
        });
    }

    #[test]
    fn doc_comment_on_enum() {
        let code = indoc! { r#"
            /// Represents a message type.
            enum Message {
                Ping,
                Pong,
            }

            fn main() {}
            "#
        };

        let formatted = formatted_with_comment_map(code);

        insta::with_settings!({
            description => format!("Code:\n\n{}", code),
            omit_expression => true,
            prepend_module_to_snapshot => true,
        }, {
            insta::assert_snapshot!(formatted);
        });
    }

    #[test]
    fn comments_in_if_block() {
        let code = indoc! { r#"
            fn foo(x: i64) -> i64 {
                if (x > 0) {
                    // positive case
                    let result = x + 1;
                    result
                } else {
                    // negative case
                    0
                }
            }
            "#
        };

        let formatted = formatted_with_comment_map(code);

        insta::with_settings!({
            description => format!("Code:\n\n{}", code),
            omit_expression => true,
            prepend_module_to_snapshot => true,
        }, {
            insta::assert_snapshot!(formatted);
        });
    }

    #[test]
    fn comments_in_while_block() {
        let code = indoc! { r#"
            fn foo() -> i64 {
                let mut x = 0;
                while (x < 10) {
                    // increment x
                    x = x + 1;
                }
                x
            }
            "#
        };

        let formatted = formatted_with_comment_map(code);

        insta::with_settings!({
            description => format!("Code:\n\n{}", code),
            omit_expression => true,
            prepend_module_to_snapshot => true,
        }, {
            insta::assert_snapshot!(formatted);
        });
    }

    #[test]
    fn comments_in_match_arms() {
        let code = indoc! { r#"
            enum Status {
                Active,
                Inactive,
            }

            fn check(s: Status) -> i64 {
                match s {
                    // when active
                    Status::Active => {
                        1
                    },
                    // when inactive
                    Status::Inactive => {
                        0
                    },
                }
            }
            "#
        };

        let formatted = formatted_with_comment_map(code);

        insta::with_settings!({
            description => format!("Code:\n\n{}", code),
            omit_expression => true,
            prepend_module_to_snapshot => true,
        }, {
            insta::assert_snapshot!(formatted);
        });
    }

    #[test]
    fn block_comment_before_function() {
        let code = indoc! { r#"
            /* This is a block comment
               describing the function */
            fn foo() -> i64 {
                1
            }
            "#
        };

        let formatted = formatted_with_comment_map(code);

        insta::with_settings!({
            description => format!("Code:\n\n{}", code),
            omit_expression => true,
            prepend_module_to_snapshot => true,
        }, {
            insta::assert_snapshot!(formatted);
        });
    }

    #[test]
    fn block_comment_inside_function() {
        let code = indoc! { r#"
            fn foo() -> i64 {
                /* compute the result */
                let x = 1;
                x
            }
            "#
        };

        let formatted = formatted_with_comment_map(code);

        insta::with_settings!({
            description => format!("Code:\n\n{}", code),
            omit_expression => true,
            prepend_module_to_snapshot => true,
        }, {
            insta::assert_snapshot!(formatted);
        });
    }
}

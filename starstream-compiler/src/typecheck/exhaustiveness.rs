//! Exhaustiveness and redundancy checking for pattern matching.
//!
//! This module implements the Maranget algorithm for checking:
//! 1. Exhaustiveness: whether all possible values are covered by the patterns
//! 2. Usefulness: whether each pattern adds coverage (detecting redundant patterns)
//!
//! The algorithm operates on a simplified pattern IR (`SimplePat`) that is derived
//! from the typed AST patterns. This separation keeps the algorithm clean while
//! allowing the AST to maintain richer information for formatting and error reporting.
//!
//! Reference: "Warnings for pattern matching" by Luc Maranget
//! http://moscova.inria.fr/~maranget/papers/warn/warn.pdf

use starstream_types::{
    Span,
    ast::Literal,
    typed_ast::{TypedEnumPatternPayload, TypedMatchArm, TypedPattern},
    types::{EnumType, EnumVariantKind, EnumVariantType, RecordType, Type},
};

use super::errors::{TypeError, TypeErrorKind};

/// Simplified pattern used internally by the exhaustiveness checker.
/// This is derived from `TypedPattern` and contains only what the algorithm needs.
#[derive(Clone, Debug, PartialEq)]
pub enum SimplePat {
    /// Matches any value (corresponds to Binding or Wildcard in the AST)
    Wildcard,
    /// A constructor pattern with a fixed arity
    Ctor { ctor: Ctor, args: Vec<SimplePat> },
    /// A literal pattern (integer, boolean, unit)
    Literal(SimpleLiteral),
}

/// Simplified literal for exhaustiveness checking
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SimpleLiteral {
    Int(i64),
    Bool(bool),
    Unit,
}

/// Represents a constructor in the pattern matching sense.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Ctor {
    /// An enum variant constructor
    EnumVariant {
        enum_name: String,
        variant_index: usize,
        variant_name: String,
    },
    /// A record/struct constructor (records have exactly one constructor)
    Record { record_name: String },
}

/// Information about the set of constructors for a type
#[derive(Clone, Debug)]
pub struct CtorSet {
    pub ctors: Vec<CtorInfo>,
}

/// Information about a single constructor
#[derive(Clone, Debug)]
pub struct CtorInfo {
    pub ctor: Ctor,
    pub arity: usize,
    pub field_types: Vec<Type>,
}

impl CtorSet {
    /// Creates a constructor set for a given type.
    /// For types without constructors (like i64, Unit), we return an "infinite" set
    /// which means wildcards always cover them.
    pub fn for_type(ty: &Type) -> Option<Self> {
        match ty {
            Type::Enum(enum_type) => Some(Self::for_enum(enum_type)),
            Type::Record(record_type) => Some(Self::for_record(record_type)),
            Type::Bool => Some(Self::for_bool()),
            // For primitive types (Int, Unit) and type variables, there are no constructors.
            // A wildcard pattern always covers these types completely.
            // We represent this as an empty constructor set.
            Type::Int | Type::Unit | Type::Var(_) => Some(Self::infinite()),
            Type::Function(_, _) | Type::Tuple(_) => Some(Self::infinite()),
        }
    }

    /// Creates an "infinite" constructor set for types that cannot be
    /// deconstructed with patterns (primitives, functions, etc.)
    /// An infinite set means no finite set of patterns can be complete.
    fn infinite() -> Self {
        Self { ctors: vec![] }
    }

    fn for_enum(enum_type: &EnumType) -> Self {
        let ctors = enum_type
            .variants
            .iter()
            .enumerate()
            .map(|(index, variant)| {
                let (arity, field_types) = match &variant.kind {
                    EnumVariantKind::Unit => (0, vec![]),
                    EnumVariantKind::Tuple(types) => (types.len(), types.clone()),
                    EnumVariantKind::Struct(fields) => {
                        (fields.len(), fields.iter().map(|f| f.ty.clone()).collect())
                    }
                };
                CtorInfo {
                    ctor: Ctor::EnumVariant {
                        enum_name: enum_type.name.clone(),
                        variant_index: index,
                        variant_name: variant.name.clone(),
                    },
                    arity,
                    field_types,
                }
            })
            .collect();

        Self { ctors }
    }

    fn for_record(record_type: &RecordType) -> Self {
        let field_types: Vec<Type> = record_type.fields.iter().map(|f| f.ty.clone()).collect();
        Self {
            ctors: vec![CtorInfo {
                ctor: Ctor::Record {
                    record_name: record_type.name.clone(),
                },
                arity: record_type.fields.len(),
                field_types,
            }],
        }
    }

    fn for_bool() -> Self {
        Self {
            ctors: vec![
                CtorInfo {
                    ctor: Ctor::EnumVariant {
                        enum_name: "bool".to_string(),
                        variant_index: 0,
                        variant_name: "false".to_string(),
                    },
                    arity: 0,
                    field_types: vec![],
                },
                CtorInfo {
                    ctor: Ctor::EnumVariant {
                        enum_name: "bool".to_string(),
                        variant_index: 1,
                        variant_name: "true".to_string(),
                    },
                    arity: 0,
                    field_types: vec![],
                },
            ],
        }
    }

    /// Returns the number of constructors in this set
    pub fn len(&self) -> usize {
        self.ctors.len()
    }

    /// Gets constructor info by reference to the Ctor
    pub fn get_info(&self, ctor: &Ctor) -> Option<&CtorInfo> {
        self.ctors.iter().find(|info| &info.ctor == ctor)
    }
}

/// A row in the pattern matrix
type PatternRow = Vec<SimplePat>;

/// The pattern matrix used by the Maranget algorithm
#[derive(Clone, Debug)]
pub struct Matrix {
    rows: Vec<PatternRow>,
    /// Type of each column (used to determine constructor sets)
    col_types: Vec<Type>,
}

impl Matrix {
    /// Creates an empty matrix with the given column types
    pub fn new(col_types: Vec<Type>) -> Self {
        Self {
            rows: Vec::new(),
            col_types,
        }
    }

    /// Adds a row to the matrix
    pub fn push(&mut self, row: PatternRow) {
        debug_assert_eq!(row.len(), self.col_types.len());
        self.rows.push(row);
    }

    /// Returns true if the matrix has no rows
    pub fn is_empty(&self) -> bool {
        self.rows.is_empty()
    }

    /// Gets the type of a column
    pub fn col_type(&self, index: usize) -> Option<&Type> {
        self.col_types.get(index)
    }

    /// Collects all constructors that appear in the first column
    fn collect_first_column_ctors(&self) -> Vec<Ctor> {
        let mut ctors = Vec::new();

        for row in &self.rows {
            if let Some(SimplePat::Ctor { ctor, .. }) = row.first()
                && !ctors.contains(ctor)
            {
                ctors.push(ctor.clone());
            }
        }

        ctors
    }

    /// Collects all literals that appear in the first column
    fn collect_first_column_literals(&self) -> Vec<SimpleLiteral> {
        let mut literals = Vec::new();

        for row in &self.rows {
            if let Some(SimplePat::Literal(lit)) = row.first()
                && !literals.contains(lit)
            {
                literals.push(lit.clone());
            }
        }

        literals
    }

    /// Checks if the first column contains any literal patterns
    fn has_literals_in_first_column(&self) -> bool {
        self.rows
            .iter()
            .any(|row| matches!(row.first(), Some(SimplePat::Literal(_))))
    }

    /// Specializes the matrix for a given constructor.
    /// This filters rows that match the constructor and expands their arguments.
    fn specialize(&self, ctor: &Ctor, ctor_info: &CtorInfo) -> Matrix {
        let arity = ctor_info.arity;

        // New column types: the field types of the constructor + remaining columns
        let mut new_col_types = ctor_info.field_types.clone();
        new_col_types.extend(self.col_types.iter().skip(1).cloned());

        let mut new_matrix = Matrix::new(new_col_types);

        for row in &self.rows {
            if row.is_empty() {
                continue;
            }

            match &row[0] {
                SimplePat::Wildcard => {
                    // Wildcard matches this constructor: expand with wildcards
                    let mut new_row = vec![SimplePat::Wildcard; arity];
                    new_row.extend(row.iter().skip(1).cloned());
                    new_matrix.push(new_row);
                }
                SimplePat::Ctor {
                    ctor: row_ctor,
                    args,
                } if row_ctor == ctor => {
                    // Constructor matches: use its arguments
                    let mut new_row = args.clone();
                    new_row.extend(row.iter().skip(1).cloned());
                    new_matrix.push(new_row);
                }
                _ => {
                    // Different constructor: skip this row
                }
            }
        }

        new_matrix
    }

    /// Default matrix: removes the first column, keeping only wildcard rows
    fn default_matrix(&self) -> Matrix {
        let new_col_types = self.col_types.iter().skip(1).cloned().collect();
        let mut new_matrix = Matrix::new(new_col_types);

        for row in &self.rows {
            if row.is_empty() {
                continue;
            }

            if matches!(&row[0], SimplePat::Wildcard) {
                let new_row: Vec<_> = row.iter().skip(1).cloned().collect();
                new_matrix.push(new_row);
            }
        }

        new_matrix
    }

    /// Specializes the matrix for a given literal.
    /// This filters rows that match the literal (wildcards also match).
    fn specialize_literal(&self, lit: &SimpleLiteral) -> Matrix {
        let new_col_types = self.col_types.iter().skip(1).cloned().collect();
        let mut new_matrix = Matrix::new(new_col_types);

        for row in &self.rows {
            if row.is_empty() {
                continue;
            }

            match &row[0] {
                SimplePat::Wildcard => {
                    // Wildcard matches any literal
                    let new_row: Vec<_> = row.iter().skip(1).cloned().collect();
                    new_matrix.push(new_row);
                }
                SimplePat::Literal(row_lit) if row_lit == lit => {
                    // Literal matches
                    let new_row: Vec<_> = row.iter().skip(1).cloned().collect();
                    new_matrix.push(new_row);
                }
                _ => {
                    // Different literal or constructor: skip
                }
            }
        }

        new_matrix
    }
}

/// Checks if a pattern vector is useful with respect to a matrix.
/// A pattern is useful if there exists a value that it matches but no row in the matrix matches.
pub fn is_useful(matrix: &Matrix, pattern: &[SimplePat]) -> bool {
    // Base case: empty pattern vector
    if pattern.is_empty() {
        // Useful iff the matrix has no rows
        return matrix.is_empty();
    }

    // Get the first column type to determine the constructor set
    let first_col_type = match matrix.col_type(0) {
        Some(ty) => ty,
        None => return matrix.is_empty(),
    };

    let ctor_set = match CtorSet::for_type(first_col_type) {
        Some(set) => set,
        None => {
            // Unknown type: conservatively say the pattern is useful
            return true;
        }
    };

    // Collect constructors that appear in the first column of the matrix
    let seen_ctors = matrix.collect_first_column_ctors();

    match &pattern[0] {
        SimplePat::Ctor { ctor, args } => {
            // Check if this constructor pattern is useful
            let ctor_info = match ctor_set.get_info(ctor) {
                Some(info) => info,
                None => return true, // Unknown constructor
            };

            let specialized = matrix.specialize(ctor, ctor_info);
            let mut new_pattern: Vec<_> = args.clone();
            new_pattern.extend(pattern.iter().skip(1).cloned());

            is_useful(&specialized, &new_pattern)
        }
        SimplePat::Wildcard => {
            // Check if there are literals in the first column
            if matrix.has_literals_in_first_column() {
                let seen_literals = matrix.collect_first_column_literals();

                // For booleans, check if all values are covered
                if matches!(first_col_type, Type::Bool) {
                    let has_true = seen_literals.contains(&SimpleLiteral::Bool(true));
                    let has_false = seen_literals.contains(&SimpleLiteral::Bool(false));

                    if has_true && has_false {
                        // Both values are covered, check remaining patterns
                        // The wildcard is useful only if one of the specializations is useful
                        let true_specialized =
                            matrix.specialize_literal(&SimpleLiteral::Bool(true));
                        let false_specialized =
                            matrix.specialize_literal(&SimpleLiteral::Bool(false));
                        let tail: Vec<_> = pattern.iter().skip(1).cloned().collect();

                        return is_useful(&true_specialized, &tail)
                            || is_useful(&false_specialized, &tail);
                    }
                }

                // For non-bool types or incomplete bool coverage, use default matrix
                let default = matrix.default_matrix();
                let new_pattern: Vec<_> = pattern.iter().skip(1).cloned().collect();

                return is_useful(&default, &new_pattern);
            }

            // For types with no constructors (primitives without literals), use the default matrix
            if ctor_set.ctors.is_empty() {
                let default = matrix.default_matrix();
                let new_pattern: Vec<_> = pattern.iter().skip(1).cloned().collect();
                return is_useful(&default, &new_pattern);
            }

            // Check if the signature is complete
            let is_complete = seen_ctors.len() == ctor_set.len();

            if is_complete {
                // Signature is complete: check all constructors
                ctor_set.ctors.iter().any(|ctor_info| {
                    let specialized = matrix.specialize(&ctor_info.ctor, ctor_info);
                    let mut new_pattern = vec![SimplePat::Wildcard; ctor_info.arity];
                    new_pattern.extend(pattern.iter().skip(1).cloned());
                    is_useful(&specialized, &new_pattern)
                })
            } else {
                // Signature is incomplete: use default matrix
                let default = matrix.default_matrix();
                let new_pattern: Vec<_> = pattern.iter().skip(1).cloned().collect();
                is_useful(&default, &new_pattern)
            }
        }
        SimplePat::Literal(lit) => {
            // Check if this specific literal is useful
            let specialized = matrix.specialize_literal(lit);
            let new_pattern: Vec<_> = pattern.iter().skip(1).cloned().collect();
            is_useful(&specialized, &new_pattern)
        }
    }
}

/// Collects missing patterns for exhaustiveness error messages.
/// Returns a list of pattern representations that are not covered.
pub fn collect_missing_patterns(matrix: &Matrix, n: usize) -> Vec<Vec<SimplePat>> {
    // Base case: no columns
    if n == 0 {
        if matrix.is_empty() {
            return vec![vec![]];
        } else {
            return vec![];
        }
    }

    // Get the first column type
    let first_col_type = match matrix.col_type(0) {
        Some(ty) => ty,
        None => return vec![vec![SimplePat::Wildcard; n]],
    };

    // Handle literal patterns specially for booleans
    if matrix.has_literals_in_first_column() && matches!(first_col_type, Type::Bool) {
        let seen_literals = matrix.collect_first_column_literals();

        let has_true = seen_literals.contains(&SimpleLiteral::Bool(true));
        let has_false = seen_literals.contains(&SimpleLiteral::Bool(false));

        if has_true && has_false {
            // Both values covered, recurse into each
            let mut result = Vec::new();

            for lit in [SimpleLiteral::Bool(true), SimpleLiteral::Bool(false)] {
                let specialized = matrix.specialize_literal(&lit);
                let missing = collect_missing_patterns(&specialized, n - 1);

                for mut pat_row in missing {
                    pat_row.insert(0, SimplePat::Literal(lit.clone()));
                    result.push(pat_row);
                }
            }

            return result;
        } else {
            // Missing one or both - report the missing literal(s)
            let default = matrix.default_matrix();
            let tail_missing = collect_missing_patterns(&default, n - 1);

            let mut result = Vec::new();
            if !has_false {
                for tail in &tail_missing {
                    let mut pattern = vec![SimplePat::Literal(SimpleLiteral::Bool(false))];

                    pattern.extend(tail.iter().cloned());

                    result.push(pattern);
                }
            }

            if !has_true {
                for tail in &tail_missing {
                    let mut pattern = vec![SimplePat::Literal(SimpleLiteral::Bool(true))];

                    pattern.extend(tail.iter().cloned());

                    result.push(pattern);
                }
            }

            return result;
        }
    }

    let ctor_set = match CtorSet::for_type(first_col_type) {
        Some(set) => set,
        None => return vec![vec![SimplePat::Wildcard; n]],
    };

    let seen_ctors = matrix.collect_first_column_ctors();
    let num_seen = seen_ctors.len();

    if num_seen == 0 {
        // No constructors seen: use default matrix
        let default = matrix.default_matrix();
        let missing = collect_missing_patterns(&default, n - 1);

        missing
            .into_iter()
            .map(|mut rest| {
                rest.insert(0, SimplePat::Wildcard);
                rest
            })
            .collect()
    } else if num_seen < ctor_set.len() {
        // Incomplete signature: find missing constructors
        let default = matrix.default_matrix();
        let tail_missing = collect_missing_patterns(&default, n - 1);

        let mut result = Vec::new();
        for ctor_info in &ctor_set.ctors {
            if !seen_ctors.contains(&ctor_info.ctor) {
                // This constructor is missing
                for tail in &tail_missing {
                    let mut pattern = vec![SimplePat::Ctor {
                        ctor: ctor_info.ctor.clone(),
                        args: vec![SimplePat::Wildcard; ctor_info.arity],
                    }];

                    pattern.extend(tail.iter().cloned());

                    result.push(pattern);
                }
            }
        }
        result
    } else {
        // Complete signature: recurse into each constructor
        let mut result = Vec::new();
        for ctor_info in &ctor_set.ctors {
            let specialized = matrix.specialize(&ctor_info.ctor, ctor_info);
            let missing = collect_missing_patterns(&specialized, ctor_info.arity + n - 1);

            for pat_row in missing {
                let (args, rest) = pat_row.split_at(ctor_info.arity);

                let mut pattern = vec![SimplePat::Ctor {
                    ctor: ctor_info.ctor.clone(),
                    args: args.to_vec(),
                }];

                pattern.extend(rest.iter().cloned());

                result.push(pattern);
            }
        }

        result
    }
}

/// Converts a TypedPattern to a SimplePat
pub fn lower_pattern(pattern: &TypedPattern, ty: &Type) -> SimplePat {
    match pattern {
        TypedPattern::Binding(_) | TypedPattern::Wildcard => SimplePat::Wildcard,
        TypedPattern::Literal(lit) => {
            let simple_lit = match lit {
                Literal::Integer(n) => SimpleLiteral::Int(*n),
                Literal::Boolean(b) => SimpleLiteral::Bool(*b),
                Literal::Unit => SimpleLiteral::Unit,
            };
            SimplePat::Literal(simple_lit)
        }
        TypedPattern::EnumVariant {
            enum_name,
            variant,
            payload,
        } => {
            // Find the variant index
            let (variant_index, variant_info) = match ty {
                Type::Enum(enum_type) => enum_type
                    .variants
                    .iter()
                    .enumerate()
                    .find(|(_, v)| v.name == variant.name)
                    .map(|(i, v)| (i, v.clone())),
                _ => None,
            }
            .unwrap_or((0, EnumVariantType::unit(&variant.name)));

            let args = match payload {
                TypedEnumPatternPayload::Unit => vec![],
                TypedEnumPatternPayload::Tuple(patterns) => {
                    let field_types = match &variant_info.kind {
                        EnumVariantKind::Tuple(types) => types.clone(),
                        _ => vec![Type::Unit; patterns.len()],
                    };
                    patterns
                        .iter()
                        .zip(field_types.iter())
                        .map(|(p, t)| lower_pattern(p, t))
                        .collect()
                }
                TypedEnumPatternPayload::Struct(fields) => {
                    // Get field order from the type definition
                    let type_fields = match &variant_info.kind {
                        EnumVariantKind::Struct(fs) => fs.clone(),
                        _ => vec![],
                    };

                    // Build args in type-definition order
                    type_fields
                        .iter()
                        .map(|type_field| {
                            fields
                                .iter()
                                .find(|f| f.name.name == type_field.name)
                                .map(|f| lower_pattern(&f.pattern, &type_field.ty))
                                .unwrap_or(SimplePat::Wildcard)
                        })
                        .collect()
                }
            };

            SimplePat::Ctor {
                ctor: Ctor::EnumVariant {
                    enum_name: enum_name.name.clone(),
                    variant_index,
                    variant_name: variant.name.clone(),
                },
                args,
            }
        }
        TypedPattern::Struct { name, fields } => {
            // Get field order from the type definition
            let type_fields = match ty {
                Type::Record(record) => record.fields.clone(),
                _ => vec![],
            };

            // Build args in type-definition order
            let args = type_fields
                .iter()
                .map(|type_field| {
                    fields
                        .iter()
                        .find(|f| f.name.name == type_field.name)
                        .map(|f| lower_pattern(&f.pattern, &type_field.ty))
                        .unwrap_or(SimplePat::Wildcard)
                })
                .collect();

            SimplePat::Ctor {
                ctor: Ctor::Record {
                    record_name: name.name.clone(),
                },
                args,
            }
        }
    }
}

/// Converts a SimplePat back to a user-friendly string representation
pub fn pattern_to_string(pattern: &SimplePat) -> String {
    match pattern {
        SimplePat::Wildcard => "_".to_string(),
        SimplePat::Literal(lit) => match lit {
            SimpleLiteral::Int(n) => n.to_string(),
            SimpleLiteral::Bool(b) => b.to_string(),
            SimpleLiteral::Unit => "()".to_string(),
        },
        SimplePat::Ctor { ctor, args } => match ctor {
            Ctor::EnumVariant { variant_name, .. } => {
                if args.is_empty() {
                    variant_name.clone()
                } else {
                    let args_str: Vec<_> = args.iter().map(pattern_to_string).collect();
                    format!("{}({})", variant_name, args_str.join(", "))
                }
            }
            Ctor::Record { record_name } => {
                if args.is_empty() {
                    format!("{} {{ }}", record_name)
                } else {
                    let args_str: Vec<_> = args.iter().map(pattern_to_string).collect();
                    format!("{} {{ {} }}", record_name, args_str.join(", "))
                }
            }
        },
    }
}

/// Check exhaustiveness and usefulness for a match expression.
/// Returns errors if any patterns are unreachable or if the match is not exhaustive.
pub fn check_match(
    scrutinee_type: &Type,
    arms: &[TypedMatchArm],
    match_span: Span,
) -> Result<(), Vec<TypeError>> {
    let mut matrix = Matrix::new(vec![scrutinee_type.clone()]);
    let mut errors = Vec::new();

    // Check each arm for usefulness (redundancy detection)
    for arm in arms {
        let pattern = lower_pattern(&arm.pattern, scrutinee_type);
        let pattern_row = vec![pattern.clone()];

        if !is_useful(&matrix, &pattern_row) {
            // Pattern is not useful (unreachable)
            let span = pattern_span(&arm.pattern).unwrap_or(match_span);
            errors.push(
                TypeError::new(TypeErrorKind::UnreachablePattern, span)
                    .with_help("this pattern is already covered by previous patterns"),
            );
        }

        // Add the pattern to the matrix for subsequent checks
        matrix.push(pattern_row);
    }

    // Check exhaustiveness
    let wildcard_pattern = vec![SimplePat::Wildcard];
    if is_useful(&matrix, &wildcard_pattern) {
        // Match is not exhaustive
        let missing = collect_missing_patterns(&matrix, 1);
        let missing_patterns: Vec<_> = missing
            .iter()
            .filter_map(|row| row.first().map(pattern_to_string))
            .take(5) // Limit to 5 examples
            .collect();

        // Format help message with missing patterns as a list
        // Special case: if the only missing pattern is a wildcard, simplify the message
        let help = if missing_patterns.len() == 1 && missing_patterns[0] == "_" {
            "add a wildcard `_` pattern to cover remaining cases".to_string()
        } else {
            let mut help = String::from("missing patterns:\n");
            for pattern in &missing_patterns {
                help.push_str(&format!("- `{}`\n", pattern));
            }
            help.push_str("ensure all cases are covered or add a wildcard `_` pattern");
            help
        };

        errors.push(
            TypeError::new(
                TypeErrorKind::NonExhaustiveMatch {
                    missing_patterns: missing_patterns.clone(),
                },
                match_span,
            )
            .with_help(help),
        );
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// Gets the span of a TypedPattern for error reporting.
/// For compound patterns, returns a span covering the entire pattern.
fn pattern_span(pattern: &TypedPattern) -> Option<Span> {
    match pattern {
        TypedPattern::Binding(id) => id.span,
        TypedPattern::Wildcard => None,
        TypedPattern::Literal(_) => None, // Literals don't have spans in TypedPattern
        TypedPattern::Struct { name, fields } => {
            // Try to span from name to the last field
            let start = name.span?;
            let end = fields
                .last()
                .and_then(|f| pattern_span(&f.pattern))
                .unwrap_or(start);
            Some(merge_spans(start, end))
        }
        TypedPattern::EnumVariant {
            enum_name,
            variant,
            payload,
        } => {
            let start = enum_name.span?;
            // Find the end span: payload fields/patterns, or variant name
            let end = match payload {
                TypedEnumPatternPayload::Unit => variant.span.unwrap_or(start),
                TypedEnumPatternPayload::Tuple(patterns) => patterns
                    .last()
                    .and_then(pattern_span)
                    .unwrap_or_else(|| variant.span.unwrap_or(start)),
                TypedEnumPatternPayload::Struct(fields) => fields
                    .last()
                    .and_then(|f| pattern_span(&f.pattern))
                    .unwrap_or_else(|| variant.span.unwrap_or(start)),
            };
            Some(merge_spans(start, end))
        }
    }
}

/// Merges two spans into one that covers both
fn merge_spans(a: Span, b: Span) -> Span {
    use chumsky::span::Span as _;
    Span::new((), a.start.min(b.start)..a.end.max(b.end))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_enum_type(name: &str, variants: Vec<(&str, usize)>) -> Type {
        Type::Enum(EnumType {
            name: name.to_string(),
            variants: variants
                .into_iter()
                .map(|(vname, arity)| {
                    if arity == 0 {
                        EnumVariantType::unit(vname)
                    } else {
                        EnumVariantType::tuple(vname, vec![Type::Int; arity])
                    }
                })
                .collect(),
        })
    }

    #[test]
    fn test_exhaustive_single_wildcard() {
        let ty = make_enum_type("Option", vec![("None", 0), ("Some", 1)]);
        let mut matrix = Matrix::new(vec![ty]);
        matrix.push(vec![SimplePat::Wildcard]);

        // Wildcard covers everything, so another wildcard is not useful
        assert!(!is_useful(&matrix, &[SimplePat::Wildcard]));
    }

    #[test]
    fn test_non_exhaustive_missing_variant() {
        let ty = make_enum_type("Option", vec![("None", 0), ("Some", 1)]);
        let mut matrix = Matrix::new(vec![ty]);

        // Only cover "None"
        matrix.push(vec![SimplePat::Ctor {
            ctor: Ctor::EnumVariant {
                enum_name: "Option".to_string(),
                variant_index: 0,
                variant_name: "None".to_string(),
            },
            args: vec![],
        }]);

        // Wildcard should be useful because Some is not covered
        assert!(is_useful(&matrix, &[SimplePat::Wildcard]));
    }

    #[test]
    fn test_exhaustive_all_variants() {
        let ty = make_enum_type("Option", vec![("None", 0), ("Some", 1)]);
        let mut matrix = Matrix::new(vec![ty]);

        // Cover "None"
        matrix.push(vec![SimplePat::Ctor {
            ctor: Ctor::EnumVariant {
                enum_name: "Option".to_string(),
                variant_index: 0,
                variant_name: "None".to_string(),
            },
            args: vec![],
        }]);

        // Cover "Some(_)"
        matrix.push(vec![SimplePat::Ctor {
            ctor: Ctor::EnumVariant {
                enum_name: "Option".to_string(),
                variant_index: 1,
                variant_name: "Some".to_string(),
            },
            args: vec![SimplePat::Wildcard],
        }]);

        // Wildcard should not be useful because all variants are covered
        assert!(!is_useful(&matrix, &[SimplePat::Wildcard]));
    }

    #[test]
    fn test_redundant_pattern() {
        let ty = make_enum_type("Option", vec![("None", 0), ("Some", 1)]);
        let mut matrix = Matrix::new(vec![ty]);

        // Cover everything with wildcard
        matrix.push(vec![SimplePat::Wildcard]);

        // Any specific pattern after wildcard is redundant
        let none_pattern = vec![SimplePat::Ctor {
            ctor: Ctor::EnumVariant {
                enum_name: "Option".to_string(),
                variant_index: 0,
                variant_name: "None".to_string(),
            },
            args: vec![],
        }];

        assert!(!is_useful(&matrix, &none_pattern));
    }
}

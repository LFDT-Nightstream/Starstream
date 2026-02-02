//! Decision tree compilation for pattern matching.
//!
//! Implements the CC algorithm from Maranget's "Compiling Pattern Matching
//! to Good Decision Trees" (2008). Patterns are simplified into a matrix
//! representation, compiled to a [`DecisionTree`], then lowered to Wasm by
//! the main codegen module.

use starstream_types::types::{EnumType, EnumVariantKind, Type};

/// Simplified pattern for the matrix (Section 2 of the paper).
/// Variables/bindings are represented as Wildcard (optionally with a binding).
#[derive(Clone, Debug)]
pub enum Pat {
    /// Wildcard pattern. If `binding` is Some, this position should be
    /// bound to a variable in the given arm.
    Wildcard {
        binding: Option<(usize, String)>, // (arm_index, variable_name)
    },
    Ctor {
        ctor: Ctor,
        args: Vec<Pat>,
    },
}

/// Constructor tag — identifies which constructor was used.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Ctor {
    EnumVariant { variant_index: usize },
    BoolTrue,
    BoolFalse,
    IntLiteral(i64),
    Struct,
    Unit,
}

impl Ctor {
    /// The arity (number of sub-patterns) for a constructor given the column type.
    pub fn arity(&self, col_type: &Type) -> usize {
        match self {
            Ctor::EnumVariant { variant_index } => {
                let Type::Enum(e) = col_type else {
                    return 0;
                };
                match &e.variants[*variant_index].kind {
                    EnumVariantKind::Unit => 0,
                    EnumVariantKind::Tuple(fields) => fields.len(),
                    EnumVariantKind::Struct(fields) => fields.len(),
                }
            }
            Ctor::BoolTrue | Ctor::BoolFalse => 0,
            Ctor::IntLiteral(_) => 0,
            Ctor::Struct => {
                let Type::Record(r) = col_type else {
                    return 0;
                };
                r.fields.len()
            }
            Ctor::Unit => 0,
        }
    }

    /// Return the types of sub-fields for this constructor.
    pub fn field_types(&self, col_type: &Type) -> Vec<Type> {
        match self {
            Ctor::EnumVariant { variant_index } => {
                let Type::Enum(e) = col_type else {
                    return vec![];
                };
                match &e.variants[*variant_index].kind {
                    EnumVariantKind::Unit => vec![],
                    EnumVariantKind::Tuple(fields) => fields.clone(),
                    EnumVariantKind::Struct(fields) => {
                        fields.iter().map(|f| f.ty.clone()).collect()
                    }
                }
            }
            Ctor::Struct => {
                let Type::Record(r) = col_type else {
                    return vec![];
                };
                r.fields.iter().map(|f| f.ty.clone()).collect()
            }
            Ctor::BoolTrue | Ctor::BoolFalse | Ctor::IntLiteral(_) | Ctor::Unit => vec![],
        }
    }
}

/// Row of the clause matrix: pattern vector + action index.
#[derive(Clone, Debug)]
pub struct Row {
    pub pats: Vec<Pat>,
    pub action: usize,
}

/// The clause matrix P → A from the paper.
#[derive(Clone, Debug)]
pub struct Matrix {
    pub rows: Vec<Row>,
}

/// A binding extracted from a Leaf node: (arm_index, name, column_index).
#[derive(Clone, Debug)]
pub struct LeafBinding {
    pub action: usize,
    pub name: String,
    pub column: usize,
}

/// Decision tree (Section 3 of the paper).
#[derive(Clone, Debug)]
pub enum DecisionTree {
    /// Match succeeded, execute arm `action`.
    /// `bindings` maps variable names to column indices at the point of the leaf.
    Leaf {
        action: usize,
        bindings: Vec<LeafBinding>,
    },
    /// No match (should be unreachable after exhaustiveness check).
    Fail,
    /// Test column `column` against constructors.
    Switch {
        column: usize,
        cases: Vec<(Ctor, DecisionTree)>,
        default: Option<Box<DecisionTree>>,
    },
}

/// Extract bindings from the first row of the matrix (used when creating a Leaf).
fn extract_leaf_bindings(row: &Row) -> Vec<LeafBinding> {
    let mut bindings = Vec::new();
    for (col, pat) in row.pats.iter().enumerate() {
        if let Pat::Wildcard {
            binding: Some((action, name)),
        } = pat
        {
            bindings.push(LeafBinding {
                action: *action,
                name: name.clone(),
                column: col,
            });
        }
    }
    bindings
}

/// Main entry point. Compiles the pattern matrix into a decision tree.
pub fn compile(matrix: &Matrix, col_types: &[Type]) -> DecisionTree {
    // 1. No rows → Fail
    if matrix.rows.is_empty() {
        return DecisionTree::Fail;
    }
    // 2. First row all wildcards → Leaf
    if matrix.rows[0]
        .pats
        .iter()
        .all(|p| matches!(p, Pat::Wildcard { .. }))
    {
        let bindings = extract_leaf_bindings(&matrix.rows[0]);
        return DecisionTree::Leaf {
            action: matrix.rows[0].action,
            bindings,
        };
    }
    // 3. Select column i with heuristic q, decompose
    let i = select_column(matrix);
    let sigma = head_constructors(matrix, i);
    let complete = is_complete_signature(&sigma, &col_types[i]);

    let cases: Vec<(Ctor, DecisionTree)> = sigma
        .iter()
        .map(|ctor| {
            let specialized = specialize(matrix, i, ctor, &col_types[i]);
            let new_col_types = specialize_col_types(col_types, i, ctor, &col_types[i]);
            let subtree = compile(&specialized, &new_col_types);
            (ctor.clone(), subtree)
        })
        .collect();

    let default = if !complete {
        let def_matrix = default_matrix(matrix, i);
        let def_col_types = default_col_types(col_types, i);
        Some(Box::new(compile(&def_matrix, &def_col_types)))
    } else {
        None
    };

    DecisionTree::Switch {
        column: i,
        cases,
        default,
    }
}

/// Specialize the matrix for column `col` and constructor `ctor`.
/// S(c, P→A) from Section 2.2 / Figure 1.
fn specialize(matrix: &Matrix, col: usize, ctor: &Ctor, col_type: &Type) -> Matrix {
    let arity = ctor.arity(col_type);
    let mut rows = Vec::new();
    for row in &matrix.rows {
        match &row.pats[col] {
            Pat::Ctor {
                ctor: row_ctor,
                args,
            } => {
                if row_ctor == ctor {
                    let mut new_pats = Vec::with_capacity(row.pats.len() - 1 + arity);
                    new_pats.extend_from_slice(&row.pats[..col]);
                    new_pats.extend(args.iter().cloned());
                    new_pats.extend_from_slice(&row.pats[col + 1..]);
                    rows.push(Row {
                        pats: new_pats,
                        action: row.action,
                    });
                }
            }
            Pat::Wildcard { binding } => {
                // Replace pats[col] with `arity` wildcards (no bindings on expanded wildcards)
                // If the wildcard had a binding, it binds the whole scrutinee at this column,
                // but after expansion we lose that — however, the binding for the whole value
                // isn't useful for sub-columns. The binding system handles this through
                // the pattern lowering phase where bindings on the outer value are recorded
                // separately from sub-patterns.
                let _ = binding;
                let mut new_pats = Vec::with_capacity(row.pats.len() - 1 + arity);
                new_pats.extend_from_slice(&row.pats[..col]);
                for _ in 0..arity {
                    new_pats.push(Pat::Wildcard { binding: None });
                }
                new_pats.extend_from_slice(&row.pats[col + 1..]);
                rows.push(Row {
                    pats: new_pats,
                    action: row.action,
                });
            }
        }
    }
    Matrix { rows }
}

/// Default matrix D(P→A) for column `col`.
fn default_matrix(matrix: &Matrix, col: usize) -> Matrix {
    let mut rows = Vec::new();
    for row in &matrix.rows {
        if matches!(row.pats[col], Pat::Wildcard { .. }) {
            let mut new_pats = Vec::with_capacity(row.pats.len() - 1);
            new_pats.extend_from_slice(&row.pats[..col]);
            new_pats.extend_from_slice(&row.pats[col + 1..]);
            rows.push(Row {
                pats: new_pats,
                action: row.action,
            });
        }
    }
    Matrix { rows }
}

/// Update column types after specialization: remove col `i`, insert field types of `ctor`.
fn specialize_col_types(col_types: &[Type], col: usize, ctor: &Ctor, col_type: &Type) -> Vec<Type> {
    let field_types = ctor.field_types(col_type);
    let mut new_types = Vec::with_capacity(col_types.len() - 1 + field_types.len());
    new_types.extend_from_slice(&col_types[..col]);
    new_types.extend(field_types);
    new_types.extend_from_slice(&col_types[col + 1..]);
    new_types
}

/// Update column types after default: just remove column `i`.
fn default_col_types(col_types: &[Type], col: usize) -> Vec<Type> {
    let mut new_types = Vec::with_capacity(col_types.len() - 1);
    new_types.extend_from_slice(&col_types[..col]);
    new_types.extend_from_slice(&col_types[col + 1..]);
    new_types
}

/// Heuristic q: select the column with the longest constructor prefix.
/// Ties broken by leftmost column. Falls back to first column with any non-wildcard.
fn select_column(matrix: &Matrix) -> usize {
    let n = matrix.rows[0].pats.len();
    let mut best_col = 0;
    let mut best_score = 0;
    for col in 0..n {
        let mut score = 0;
        for row in &matrix.rows {
            if matches!(row.pats[col], Pat::Ctor { .. }) {
                score += 1;
            } else {
                break;
            }
        }
        if score > best_score {
            best_score = score;
            best_col = col;
        }
    }
    // Fallback: first column with any non-wildcard
    if best_score == 0 {
        for col in 0..n {
            if matrix
                .rows
                .iter()
                .any(|r| matches!(r.pats[col], Pat::Ctor { .. }))
            {
                return col;
            }
        }
    }
    best_col
}

/// Collect unique constructors appearing in column `col`.
fn head_constructors(matrix: &Matrix, col: usize) -> Vec<Ctor> {
    let mut ctors = Vec::new();
    for row in &matrix.rows {
        if let Pat::Ctor { ctor, .. } = &row.pats[col]
            && !ctors.contains(ctor) {
                ctors.push(ctor.clone());
            }
    }
    ctors
}

/// Check if the set of constructors forms a complete signature for the type.
fn is_complete_signature(ctors: &[Ctor], ty: &Type) -> bool {
    match ty {
        Type::Bool => ctors.contains(&Ctor::BoolTrue) && ctors.contains(&Ctor::BoolFalse),
        Type::Enum(EnumType { variants, .. }) => {
            !variants.is_empty()
                && (0..variants.len())
                    .all(|i| ctors.contains(&Ctor::EnumVariant { variant_index: i }))
        }
        Type::Record(_) => true,
        Type::Unit => true,
        Type::Int => false,
        _ => false,
    }
}

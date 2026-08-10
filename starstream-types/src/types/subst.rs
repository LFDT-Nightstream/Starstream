use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    sync::Arc,
};

use crate::{
    AbiType, EnumType, EnumVariantKind, EnumVariantType, FunctionType, RecordFieldType, RecordType,
    TokenType, TypedFunctionParam, UtxoType,
};

use super::{Type, TypeVarId};

/// Types which contain [Type]s which can be recursively substituted.
pub trait SubstituteType: Sized + Clone {
    /// Return `true` if `var` appears anywhere inside `self`, expanding substitutions as needed.
    fn contains_var(&self, var: TypeVarId, subst: &HashMap<TypeVarId, Type>) -> bool;

    /// Collect all free type variables present in `self`.
    fn collect_free_type_vars(&self, set: &mut HashSet<TypeVarId>);

    /// Recursively replace any variables mentioned in `subst` within `self`.
    ///
    /// Variables in `int_vars` are replaced with [Type::int].
    ///
    /// Returns `Cow::Borrowed(self)` if no substitution was necessary.
    fn substitute_type(
        &self,
        subst: &HashMap<TypeVarId, Type>,
        int_vars: &HashSet<TypeVarId>,
    ) -> Cow<'_, Self>;

    // ------------------------------------------------------------------------

    /// Like [Self::substitute_type], but in place.
    fn substitute_in_place(
        &mut self,
        subst: &HashMap<TypeVarId, Type>,
        int_vars: &HashSet<TypeVarId>,
    ) {
        if let Cow::Owned(replacement) = self.substitute_type(subst, int_vars) {
            *self = replacement;
        }
    }

    /// Get all free type variables present in `self`.
    fn free_type_vars(&self) -> HashSet<TypeVarId> {
        let mut set = HashSet::new();
        self.collect_free_type_vars(&mut set);
        set
    }
}

impl<T: Sized + Clone + SubstituteType> SubstituteType for Arc<T> {
    fn contains_var(&self, var: TypeVarId, subst: &HashMap<TypeVarId, Type>) -> bool {
        (**self).contains_var(var, subst)
    }

    fn collect_free_type_vars(&self, set: &mut HashSet<TypeVarId>) {
        (**self).collect_free_type_vars(set);
    }

    fn substitute_type(
        &self,
        subst: &HashMap<TypeVarId, Type>,
        int_vars: &HashSet<TypeVarId>,
    ) -> Cow<'_, Self> {
        match (**self).substitute_type(subst, int_vars) {
            Cow::Borrowed(_) => Cow::Borrowed(self),
            // Can be inefficient; prefer substitute_in_place when possible.
            Cow::Owned(owned) => Cow::Owned(Arc::new(owned)),
        }
    }

    // Override to reuse Arc allocation if it's unique.
    fn substitute_in_place(
        &mut self,
        subst: &HashMap<TypeVarId, Type>,
        int_vars: &HashSet<TypeVarId>,
    ) {
        if let Cow::Owned(replacement) = (**self).substitute_type(subst, int_vars) {
            match Arc::get_mut(self) {
                Some(reuse) => *reuse = replacement,
                None => *self = Arc::new(replacement),
            }
        }
    }
}

// TODO: avoid unconditional into_owned calls below, instead returning Borrowed
// if all children returned Borrowed.

// TODO: add anti-infinite-recursion (perhaps HashSet<usize> for seen pointers)
// when needed.

impl SubstituteType for Type {
    fn contains_var(&self, var: TypeVarId, subst: &HashMap<TypeVarId, Type>) -> bool {
        match self {
            Type::Var(id) if *id == var => true,
            Type::Var(id) => subst.get(id).is_some_and(|ty| ty.contains_var(var, subst)),
            Type::Tuple(items) => items.iter().any(|ty| ty.contains_var(var, subst)),
            Type::Function(function_type) => function_type.contains_var(var, subst),
            Type::Record(record_type) => record_type.contains_var(var, subst),
            Type::Enum(enum_type) => enum_type.contains_var(var, subst),
            Type::Utxo(utxo_type) => utxo_type.contains_var(var, subst),
            Type::Token(token_type) => token_type.contains_var(var, subst),
            Type::Abi(abi_type) => abi_type.contains_var(var, subst),
            Type::Unit | Type::Bool | Type::Int(_) | Type::UtxoAny | Type::TokenAny => false,
        }
    }

    fn collect_free_type_vars(&self, set: &mut HashSet<TypeVarId>) {
        match self {
            Type::Var(id) => {
                set.insert(*id);
            }
            Type::Tuple(items) => {
                for ty in items.iter() {
                    ty.collect_free_type_vars(set);
                }
            }
            Type::Function(function_type) => function_type.collect_free_type_vars(set),
            Type::Record(record_type) => record_type.collect_free_type_vars(set),
            Type::Enum(enum_type) => enum_type.collect_free_type_vars(set),
            Type::Utxo(utxo_type) => utxo_type.collect_free_type_vars(set),
            Type::Token(token_type) => token_type.collect_free_type_vars(set),
            Type::Abi(abi_type) => abi_type.collect_free_type_vars(set),
            Type::Unit | Type::Bool | Type::Int(_) | Type::UtxoAny | Type::TokenAny => {}
        }
    }

    fn substitute_type(
        &self,
        subst: &HashMap<TypeVarId, Type>,
        int_vars: &HashSet<TypeVarId>,
    ) -> Cow<'_, Self> {
        match self {
            Type::Var(id) => match subst.get(id) {
                // NB: substitutions must always become Owned
                Some(ty) => Cow::Owned(ty.substitute_type(subst, int_vars).into_owned()),
                None if int_vars.contains(id) => Cow::Owned(Type::int()),
                None => Cow::Borrowed(self),
            },
            Type::Function(func) => match func.substitute_type(subst, int_vars) {
                Cow::Borrowed(_) => Cow::Borrowed(self),
                Cow::Owned(owned) => Cow::Owned(Type::Function(owned)),
            },
            Type::Tuple(items) => match items.substitute_type(subst, int_vars) {
                Cow::Borrowed(_) => Cow::Borrowed(self),
                Cow::Owned(owned) => Cow::Owned(Type::Tuple(owned)),
            },
            Type::Record(record) => match record.substitute_type(subst, int_vars) {
                Cow::Borrowed(_) => Cow::Borrowed(self),
                Cow::Owned(owned) => Cow::Owned(Type::Record(owned)),
            },
            Type::Enum(enum_type) => match enum_type.substitute_type(subst, int_vars) {
                Cow::Borrowed(_) => Cow::Borrowed(self),
                Cow::Owned(owned) => Cow::Owned(Type::Enum(owned)),
            },
            Type::Utxo(utxo_type) => match utxo_type.substitute_type(subst, int_vars) {
                Cow::Borrowed(_) => Cow::Borrowed(self),
                Cow::Owned(owned) => Cow::Owned(Type::Utxo(owned)),
            },
            Type::Token(token_type) => match token_type.substitute_type(subst, int_vars) {
                Cow::Borrowed(_) => Cow::Borrowed(self),
                Cow::Owned(owned) => Cow::Owned(Type::Token(owned)),
            },
            Type::Abi(abi_type) => match abi_type.substitute_type(subst, int_vars) {
                Cow::Borrowed(_) => Cow::Borrowed(self),
                Cow::Owned(owned) => Cow::Owned(Type::Abi(owned)),
            },
            Type::Unit | Type::Bool | Type::Int(_) | Type::UtxoAny | Type::TokenAny => {
                Cow::Borrowed(self)
            }
        }
    }

    // TODO: substitute_in_place that forwards to Arc::substitute_in_place
}

impl SubstituteType for FunctionType {
    fn contains_var(&self, var: TypeVarId, subst: &HashMap<TypeVarId, Type>) -> bool {
        self.params.contains_var(var, subst) || self.result.contains_var(var, subst)
    }

    fn collect_free_type_vars(&self, set: &mut HashSet<TypeVarId>) {
        self.params.collect_free_type_vars(set);
        self.result.collect_free_type_vars(set);
    }

    fn substitute_type(
        &self,
        subst: &HashMap<TypeVarId, Type>,
        int_vars: &HashSet<TypeVarId>,
    ) -> Cow<'_, Self> {
        Cow::Owned(FunctionType {
            params: self.params.substitute_type(subst, int_vars).into_owned(),
            result: self.result.substitute_type(subst, int_vars).into_owned(),
            kind: self.kind,
            name_span: self.name_span,
            callee: self.callee.clone(),
        })
    }
}

impl SubstituteType for TypedFunctionParam {
    fn contains_var(&self, var: TypeVarId, subst: &HashMap<TypeVarId, Type>) -> bool {
        self.ty.contains_var(var, subst)
    }

    fn collect_free_type_vars(&self, set: &mut HashSet<TypeVarId>) {
        self.ty.collect_free_type_vars(set);
    }

    fn substitute_type(
        &self,
        subst: &HashMap<TypeVarId, Type>,
        int_vars: &HashSet<TypeVarId>,
    ) -> Cow<'_, Self> {
        match self.ty.substitute_type(subst, int_vars) {
            Cow::Borrowed(_) => Cow::Borrowed(self),
            Cow::Owned(owned) => Cow::Owned(TypedFunctionParam {
                public: self.public,
                name: self.name.clone(),
                ty: owned,
                ty_span: self.ty_span,
            }),
        }
    }
}

impl SubstituteType for Vec<TypedFunctionParam> {
    fn contains_var(&self, var: TypeVarId, subst: &HashMap<TypeVarId, Type>) -> bool {
        self.iter().any(|ty| ty.contains_var(var, subst))
    }

    fn collect_free_type_vars(&self, set: &mut HashSet<TypeVarId>) {
        for ty in self.iter() {
            ty.collect_free_type_vars(set);
        }
    }

    fn substitute_type(
        &self,
        subst: &HashMap<TypeVarId, Type>,
        int_vars: &HashSet<TypeVarId>,
    ) -> Cow<'_, Self> {
        Cow::Owned(
            self.iter()
                .map(|ty| ty.substitute_type(subst, int_vars).into_owned())
                .collect(),
        )
    }
}

impl SubstituteType for Vec<Type> {
    fn contains_var(&self, var: TypeVarId, subst: &HashMap<TypeVarId, Type>) -> bool {
        self.iter().any(|ty| ty.contains_var(var, subst))
    }

    fn collect_free_type_vars(&self, set: &mut HashSet<TypeVarId>) {
        for ty in self.iter() {
            ty.collect_free_type_vars(set);
        }
    }

    fn substitute_type(
        &self,
        subst: &HashMap<TypeVarId, Type>,
        int_vars: &HashSet<TypeVarId>,
    ) -> Cow<'_, Self> {
        Cow::Owned(
            self.iter()
                .map(|ty| ty.substitute_type(subst, int_vars).into_owned())
                .collect(),
        )
    }
}

impl SubstituteType for RecordType {
    fn contains_var(&self, var: TypeVarId, subst: &HashMap<TypeVarId, Type>) -> bool {
        self.fields
            .iter()
            .any(|field| field.ty.contains_var(var, subst))
    }

    fn collect_free_type_vars(&self, set: &mut HashSet<TypeVarId>) {
        for field in &self.fields {
            field.ty.collect_free_type_vars(set);
        }
    }

    fn substitute_type(
        &self,
        subst: &HashMap<TypeVarId, Type>,
        int_vars: &HashSet<TypeVarId>,
    ) -> Cow<'_, Self> {
        match self.fields.substitute_type(subst, int_vars) {
            Cow::Borrowed(_) => Cow::Borrowed(self),
            Cow::Owned(owned) => Cow::Owned(RecordType {
                name: self.name.clone(),
                fields: owned,
            }),
        }
    }
}

impl SubstituteType for Vec<RecordFieldType> {
    fn contains_var(&self, var: TypeVarId, subst: &HashMap<TypeVarId, Type>) -> bool {
        self.iter().any(|field| field.ty.contains_var(var, subst))
    }

    fn collect_free_type_vars(&self, set: &mut HashSet<TypeVarId>) {
        for field in self.iter() {
            field.ty.collect_free_type_vars(set);
        }
    }

    fn substitute_type(
        &self,
        subst: &HashMap<TypeVarId, Type>,
        int_vars: &HashSet<TypeVarId>,
    ) -> Cow<'_, Self> {
        Cow::Owned(
            self.iter()
                .map(|field| RecordFieldType {
                    name: field.name.clone(),
                    ty: field.ty.substitute_type(subst, int_vars).into_owned(),
                })
                .collect(),
        )
    }
}

impl SubstituteType for EnumType {
    fn contains_var(&self, var: TypeVarId, subst: &HashMap<TypeVarId, Type>) -> bool {
        self.variants.iter().any(|v| v.contains_var(var, subst))
    }

    fn collect_free_type_vars(&self, set: &mut HashSet<TypeVarId>) {
        for v in self.variants.iter() {
            v.collect_free_type_vars(set);
        }
    }

    fn substitute_type(
        &self,
        subst: &HashMap<TypeVarId, Type>,
        int_vars: &HashSet<TypeVarId>,
    ) -> Cow<'_, Self> {
        Cow::Owned(EnumType {
            name: self.name.clone(),
            variants: self
                .variants
                .iter()
                .map(|variant| variant.substitute_type(subst, int_vars).into_owned())
                .collect(),
            type_args: self
                .type_args
                .iter()
                .map(|ty| ty.substitute_type(subst, int_vars).into_owned())
                .collect(),
        })
    }
}

impl SubstituteType for EnumVariantType {
    fn contains_var(&self, var: TypeVarId, subst: &HashMap<TypeVarId, Type>) -> bool {
        self.kind.contains_var(var, subst)
    }

    fn collect_free_type_vars(&self, set: &mut HashSet<TypeVarId>) {
        self.kind.collect_free_type_vars(set);
    }

    fn substitute_type(
        &self,
        subst: &HashMap<TypeVarId, Type>,
        int_vars: &HashSet<TypeVarId>,
    ) -> Cow<'_, Self> {
        match self.kind.substitute_type(subst, int_vars) {
            Cow::Borrowed(_) => Cow::Borrowed(self),
            Cow::Owned(owned) => Cow::Owned(EnumVariantType {
                name: self.name.clone(),
                kind: owned,
            }),
        }
    }
}

impl SubstituteType for EnumVariantKind {
    fn contains_var(&self, var: TypeVarId, subst: &HashMap<TypeVarId, Type>) -> bool {
        match self {
            EnumVariantKind::Unit => false,
            EnumVariantKind::Tuple(items) => items.contains_var(var, subst),
            EnumVariantKind::Struct(record_field_types) => {
                record_field_types.contains_var(var, subst)
            }
        }
    }

    fn collect_free_type_vars(&self, set: &mut HashSet<TypeVarId>) {
        match self {
            EnumVariantKind::Unit => {}
            EnumVariantKind::Tuple(items) => items.collect_free_type_vars(set),
            EnumVariantKind::Struct(record_field_types) => {
                record_field_types.collect_free_type_vars(set)
            }
        }
    }

    fn substitute_type(
        &self,
        subst: &HashMap<TypeVarId, Type>,
        int_vars: &HashSet<TypeVarId>,
    ) -> Cow<'_, Self> {
        match self {
            EnumVariantKind::Unit => Cow::Borrowed(self),
            EnumVariantKind::Tuple(items) => match items.substitute_type(subst, int_vars) {
                Cow::Borrowed(_) => Cow::Borrowed(self),
                Cow::Owned(owned) => Cow::Owned(EnumVariantKind::Tuple(owned)),
            },
            EnumVariantKind::Struct(record_field_types) => {
                match record_field_types.substitute_type(subst, int_vars) {
                    Cow::Borrowed(_) => Cow::Borrowed(self),
                    Cow::Owned(owned) => Cow::Owned(EnumVariantKind::Struct(owned)),
                }
            }
        }
    }
}

// TODO: non-dummy implementation needed?
impl SubstituteType for UtxoType {
    fn contains_var(&self, _var: TypeVarId, _subst: &HashMap<TypeVarId, Type>) -> bool {
        false
    }

    fn collect_free_type_vars(&self, _set: &mut HashSet<TypeVarId>) {}

    fn substitute_type(
        &self,
        _subst: &HashMap<TypeVarId, Type>,
        _int_vars: &HashSet<TypeVarId>,
    ) -> Cow<'_, Self> {
        Cow::Borrowed(self)
    }
}

impl SubstituteType for TokenType {
    fn contains_var(&self, _var: TypeVarId, _subst: &HashMap<TypeVarId, Type>) -> bool {
        false
    }

    fn collect_free_type_vars(&self, _set: &mut HashSet<TypeVarId>) {}

    fn substitute_type(
        &self,
        _subst: &HashMap<TypeVarId, Type>,
        _int_vars: &HashSet<TypeVarId>,
    ) -> Cow<'_, Self> {
        Cow::Borrowed(self)
    }
}

// TODO: non-dummy implementation needed?
impl SubstituteType for AbiType {
    fn contains_var(&self, _var: TypeVarId, _subst: &HashMap<TypeVarId, Type>) -> bool {
        false
    }

    fn collect_free_type_vars(&self, _set: &mut HashSet<TypeVarId>) {}

    fn substitute_type(
        &self,
        _subst: &HashMap<TypeVarId, Type>,
        _int_vars: &HashSet<TypeVarId>,
    ) -> Cow<'_, Self> {
        Cow::Borrowed(self)
    }
}

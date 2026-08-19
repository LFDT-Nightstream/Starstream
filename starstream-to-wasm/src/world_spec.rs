//! This module expresses the [Starstream WIT world spec][wit-worlds]
//! in a format for the compiler's consumption.
//!
//! Functions which are explicitly imported need not be described here.
//!
//! [wit-worlds]: https://starstream.nightstream.dev/wit-worlds

use std::{collections::BTreeMap, rc::Rc};

use starstream_types::{Identifier, Type, TypedFunctionParam};
use wasm_encoder::{FuncType, InstanceType, ValType};

use crate::{
    Compiler,
    component_abi::{ComponentAbiType, Resource},
    component_encoder::TypeBuilder,
    to_kebab_case,
};

/// Builtins imported from `starstream:std/builtins` and friends.
#[derive(Default)]
pub struct Builtins {
    pub utxo_context_resource: Option<Rc<Resource>>,
    pub utxo_context_drop: Option<u32>,
    pub resume: Option<u32>,
    pub implements_method: Option<u32>,
}

impl Compiler {
    /// Implicit `starstream:std/builtin` import.
    pub fn import_builtin(&mut self) {
        let name = "starstream:std/builtin";
        if self.world_type.has_imported(name) {
            return;
        }

        let mut builtin = TypeBuilder::new_interface();
        self.star_to_component.insert(
            Type::UtxoAny,
            Rc::new(ComponentAbiType::Borrow {
                resource: builtin.fresh_resource("utxo", "s-utxo"),
            }),
        );
        self.star_to_component.insert(
            Type::TokenAny,
            Rc::new(ComponentAbiType::Borrow {
                resource: builtin.fresh_resource("token", "s-token"),
            }),
        );
        self.world_type.import_interface(name, &builtin);
    }

    /// Implicit `starstream:std/utxo-context` import.
    pub fn import_utxo_context(&mut self) {
        let name = "starstream:std/utxo-context";
        if self.world_type.has_imported(name) {
            return;
        }

        let mut utxo_context = TypeBuilder::new_interface();

        let utxo_context_resource = utxo_context.fresh_resource("utxo-context", "s-utxo-context");
        self.builtins.utxo_context_resource = Some(utxo_context_resource.clone());
        let utxo_context_type = Rc::new(ComponentAbiType::Borrow {
            resource: utxo_context_resource,
        });
        self.builtins.utxo_context_drop = Some(self.import_function(
            name,
            "[resource-drop]utxo-context",
            &FuncType::new([ValType::I32], []),
        ));

        self.builtins.resume = Some(self.import_function(
            name,
            "[method]utxo-context.resume",
            &FuncType::new([ValType::I32], []),
        ));
        utxo_context.export_fn_2(
            "[method]utxo-context.resume",
            [("self", utxo_context_type.clone())],
            None,
        );

        self.builtins.implements_method = Some(self.import_function(
            name,
            "[method]utxo-context.implements-method",
            &FuncType::new(
                [
                    ValType::I32,
                    ValType::I64,
                    ValType::I64,
                    ValType::I64,
                    ValType::I64,
                ],
                [],
            ),
        ));
        let u64_ty = Rc::new(ComponentAbiType::U64);
        let tuple = Rc::new(ComponentAbiType::Tuple {
            fields: vec![u64_ty; 4],
        });
        utxo_context.export_fn_2(
            "[method]utxo-context.implements-method",
            [("self", utxo_context_type), ("hash", tuple)],
            None,
        );

        self.world_type.import_interface(name, &utxo_context);
    }

    /// Declare an event per spec.
    pub fn declare_event(
        &mut self,
        imported_interfaces: &mut BTreeMap<String, TypeBuilder<InstanceType>>,
        abi_name: &Identifier,
        event_name: &Identifier,
        params: &[TypedFunctionParam],
    ) -> u32 {
        let mut core_params = Vec::with_capacity(16);
        for p in params {
            _ = self.star_to_core_types(event_name.span, &mut core_params, &p.ty);
        }

        let interface = format!("starstream:events/{}", to_kebab_case(abi_name.as_str()));
        let kebab = to_kebab_case(event_name.as_str());

        // Core import
        let func_idx = self.import_function(
            &interface,
            &kebab,
            &FuncType::new(core_params.iter().copied(), std::iter::empty()),
        );

        // Component import
        let sig = self.star_to_component_signature(None, params, &Type::Unit);
        let iface = imported_interfaces.entry(interface).or_default();
        iface.export_fn(&kebab, &sig);

        func_idx
    }

    /// Declare an effect per spec.
    pub fn declare_effect(
        &mut self,
        imported_interfaces: &mut BTreeMap<String, TypeBuilder<InstanceType>>,
        abi_name: &Identifier,
        effect_name: &Identifier,
        params: &[TypedFunctionParam],
        result: &Type,
    ) -> u32 {
        let mut core_params = Vec::with_capacity(16);
        for p in params {
            _ = self.star_to_core_types(effect_name.span, &mut core_params, &p.ty);
        }
        let mut core_results = Vec::new();
        _ = self.star_to_core_types(effect_name.span, &mut core_results, result);

        let interface = format!("starstream:effects/{}", to_kebab_case(abi_name.as_str()));
        let kebab = to_kebab_case(effect_name.as_str());

        // Core import
        let func_idx = self.import_function(
            &interface,
            &kebab,
            &FuncType::new(core_params.iter().copied(), core_results),
        );

        // Component import
        let sig = self.star_to_component_signature(None, params, result);
        let iface = imported_interfaces.entry(interface).or_default();
        iface.export_fn(&kebab, &sig);

        func_idx
    }
}

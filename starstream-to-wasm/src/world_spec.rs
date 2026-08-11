//! This module expresses the [Starstream WIT world spec][wit-worlds]
//! in a format for the compiler's consumption.
//!
//! Functions which are explicitly imported need not be described here.
//!
//! [wit-worlds]: https://starstream.nightstream.dev/wit-worlds

use starstream_types::{Identifier, Type, TypedFunctionParam};
use wasm_encoder::FuncType;

use crate::{Compiler, to_kebab_case};

impl Compiler {
    pub fn declare_event(
        &mut self,
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
        let core_fn_ty = self.add_core_func_type(&FuncType::new(
            core_params.iter().copied(),
            std::iter::empty(),
        ));
        let func_idx = self.import_function(&interface, &kebab, core_fn_ty);

        // Component import
        let comp_params = params
            .iter()
            .filter_map(|p| {
                self.star_to_component_type(&p.ty)
                    .map(|t| (p.name.as_str(), t))
            })
            .collect::<Vec<_>>();
        let comp_result = None;
        let iface = self.imported_interfaces.entry(interface).or_default();
        iface.export_fn_2(&kebab, comp_params.into_iter(), comp_result.as_ref());

        func_idx
    }

    pub fn declare_effect(
        &mut self,
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
        let core_fn_ty =
            self.add_core_func_type(&FuncType::new(core_params.iter().copied(), core_results));
        let func_idx = self.import_function(&interface, &kebab, core_fn_ty);

        // Component import
        let comp_params = params
            .iter()
            .filter_map(|p| {
                self.star_to_component_type(&p.ty)
                    .map(|t| (p.name.as_str(), t))
            })
            .collect::<Vec<_>>();
        let comp_result = self.star_to_component_type(result);
        let iface = self.imported_interfaces.entry(interface).or_default();
        iface.export_fn_2(&kebab, comp_params.into_iter(), comp_result.as_ref());

        func_idx
    }
}

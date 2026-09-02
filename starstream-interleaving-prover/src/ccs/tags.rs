use neo_application::ConstraintTag;

use crate::opcode::Opcode;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum ConstraintScope {
    Always,
    Opcode(Opcode),
    Opcodes(Box<[Opcode]>),
}

pub(super) fn always(arg: &'static str) -> ConstraintTag<ConstraintScope> {
    // TODO: maybe just expose the fields
    ConstraintTag::new(arg, ConstraintScope::Always)
}

pub(super) fn opcode_tag(label: &'static str, opcode: Opcode) -> ConstraintTag<ConstraintScope> {
    ConstraintTag::new(label, ConstraintScope::Opcode(opcode))
}

pub(super) fn opcode_tags(
    label: &'static str,
    opcodes: &[Opcode],
) -> ConstraintTag<ConstraintScope> {
    ConstraintTag::new(
        label,
        ConstraintScope::Opcodes(opcodes.to_vec().into_boxed_slice()),
    )
}

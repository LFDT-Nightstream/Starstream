mod builtins;
mod diagnostic;
mod env;
mod errors;
mod exhaustiveness;
mod infer;
mod tree;
mod warnings;

pub use errors::{TypeError, TypeErrorKind};
pub use exhaustiveness::check_match;
pub use infer::{
    TypecheckFailure, TypecheckModulesFailure, TypecheckOptions, TypecheckSuccess, TypedModule,
    TypedModuleGraph, typecheck_modules, typecheck_program,
};
pub use tree::InferenceTree;
pub use warnings::{TypeWarning, TypeWarningKind};

#[cfg(test)]
mod tests;

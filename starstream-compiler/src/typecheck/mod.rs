mod env;
mod errors;
mod infer;
mod tree;

pub use errors::{TypeError, TypeErrorKind};
pub use infer::{TypecheckOptions, TypecheckSuccess, typecheck_program};
pub use tree::InferenceTree;

#[cfg(test)]
mod tests;

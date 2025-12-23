mod builtins;
mod env;
mod errors;
mod exhaustiveness;
mod infer;
mod tree;

pub use errors::{TypeError, TypeErrorKind};
pub use exhaustiveness::check_match;
pub use infer::{TypecheckOptions, TypecheckSuccess, typecheck_program};
pub use tree::InferenceTree;

#[cfg(test)]
mod tests;

//! Error types for the Starstream DSL

/// An error code with both a name and embedded documentation attached.
#[derive(Clone, Copy)]
pub struct ErrorCode {
    pub name: &'static str,
    pub docs: &'static str,
}

impl std::fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name)
    }
}

inventory::collect!(ErrorCode);

impl ErrorCode {
    /// Iterate over all error codes registered with [`error_code`].
    pub fn iter() -> impl Iterator<Item = &'static ErrorCode> {
        inventory::iter::<ErrorCode>()
    }
}

/// A warning code with both a name and embedded documentation attached.
#[derive(Clone, Copy)]
pub struct WarningCode {
    pub name: &'static str,
    pub docs: &'static str,
}

impl std::fmt::Display for WarningCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name)
    }
}

inventory::collect!(WarningCode);

impl WarningCode {
    /// Iterate over all warning codes registered with [`warning_code`].
    pub fn iter() -> impl Iterator<Item = &'static WarningCode> {
        inventory::iter::<WarningCode>()
    }
}

/// Create an error code, attaching its docs from the associated file in the
/// `docs` folder and registering it with [`ErrorCode::iter`].
#[macro_export]
macro_rules! error_code {
    ($id:ident) => {{
        static $id: ErrorCode = ErrorCode {
            name: stringify!($id),
            docs: include_str!(concat!(
                // TODO: assumes all crates are at the top level
                env!("CARGO_MANIFEST_DIR"),
                "/../docs/errors/",
                stringify!($id),
                ".md",
            )),
        };
        $crate::__inventory::submit!($id);
        &$id
    }};
}

/// Create a warning code, attaching its docs from the associated file in the
/// `docs` folder and registering it with [`WarningCode::iter`].
#[macro_export]
macro_rules! warning_code {
    ($id:ident) => {{
        static $id: WarningCode = WarningCode {
            name: stringify!($id),
            docs: include_str!(concat!(
                // TODO: assumes all crates are at the top level
                env!("CARGO_MANIFEST_DIR"),
                "/../docs/warnings/",
                stringify!($id),
                ".md",
            )),
        };
        $crate::__inventory::submit!($id);
        &$id
    }};
}

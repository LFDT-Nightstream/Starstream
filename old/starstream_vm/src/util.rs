/// Formatting helper for hex strings.
pub struct DisplayHex<'a>(pub &'a [u8]);

impl<'a> std::fmt::Display for DisplayHex<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for &v in self.0 {
            write!(f, "{:02x}", v)?;
        }
        Ok(())
    }
}

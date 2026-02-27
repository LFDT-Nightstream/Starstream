use clap::Args;
use starstream_types::ErrorCode;

/// Start the Starstream language server
#[derive(Args, Debug)]
#[clap(version(starstream_language_server::VERSION))]
pub struct Explain {
    /// The error code to explain, like `E0001`. If absent, lists known error codes.
    error_code: Option<String>,
}

impl Explain {
    pub fn exec(self) -> miette::Result<()> {
        if let Some(error_code) = self.error_code {
            // Show specific error docs.
            for each in ErrorCode::iter() {
                if each.name == error_code {
                    println!("{}", each.docs);
                    return Ok(());
                }
            }
            Err(miette::Error::msg(format!(
                "{} is not a valid error code",
                error_code,
            )))
        } else {
            // List known error codes.
            let mut names = ErrorCode::iter().map(|ec| ec.name).collect::<Vec<_>>();
            names.sort();
            for name in names {
                println!("{name}");
            }
            Ok(())
        }
    }
}

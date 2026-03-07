use clap::Args;
use starstream_types::{ErrorCode, WarningCode};

/// Explain Starstream diagnostics
#[derive(Args, Debug)]
#[clap(version(starstream_language_server::VERSION))]
pub struct Explain {
    /// The diagnostic code to explain, like `E0001` or `W0001`.
    diagnostic_code: Option<String>,
}

impl Explain {
    pub fn exec(self) -> miette::Result<()> {
        if let Some(diagnostic_code) = self.diagnostic_code {
            for each in ErrorCode::iter() {
                if each.name == diagnostic_code {
                    println!("{}", each.docs);
                    return Ok(());
                }
            }
            for each in WarningCode::iter() {
                if each.name == diagnostic_code {
                    println!("{}", each.docs);
                    return Ok(());
                }
            }
            Err(miette::Error::msg(format!(
                "{} is not a valid diagnostic code",
                diagnostic_code,
            )))
        } else {
            let mut names = ErrorCode::iter().map(|ec| ec.name).collect::<Vec<_>>();
            names.extend(WarningCode::iter().map(|wc| wc.name));
            names.sort();
            for name in names {
                println!("{name}");
            }
            Ok(())
        }
    }
}

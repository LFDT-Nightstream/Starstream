use clap::Args;
use tracing::info;

use crate::InputArg;
use crate::diagnostics::print_report;

/// Run `test` blocks in the given `.wasm` file or contracts.
#[derive(Args, Debug)]
pub struct Test {
    #[clap(flatten)]
    pub input: InputArg,
}

impl Test {
    pub async fn exec(self) -> miette::Result<()> {
        let wasms = if let Some(ext) = self.input.input.extension()
            && ext == "wasm"
        {
            vec![self.input.input.clone()]
        } else {
            crate::Build { input: self.input }.exec()?.component_wasm
        };

        let mut errors = 0;

        for (i, wasm) in wasms.iter().enumerate() {
            let (before, after) = (&wasms[..i], &wasms[i + 1..]);
            info!(file = wasm.display().to_string(), "Testing contract");
            match crate::run::load(
                wasm,
                &mut before.iter().chain(after.iter()).map(|p| p.as_path()),
            )
            .await
            {
                Ok((engine, contract)) => {
                    for (name, export) in contract.component().component_type().exports(&engine) {
                        if let Some(id) = export.external_id
                            && let Some(test_name) = id.strip_prefix("starstream:test:")
                        {
                            info!(test = test_name, "Running test");
                            match crate::run::call(
                                &engine,
                                &contract,
                                name,
                                &mut std::iter::empty(),
                            )
                            .await
                            {
                                Ok(output) => info!(test = test_name, output = output),
                                Err(err) => {
                                    errors += 1;
                                    print_report(miette::Report::msg(err).wrap_err(format!(
                                        "{wasm}: {test_name:?} failed",
                                        wasm = wasm.display()
                                    )))?;
                                }
                            }
                        }
                    }
                }
                Err(err) => {
                    errors += 1;
                    print_report(
                        miette::Report::msg(err)
                            .wrap_err(format!("could not load {wasm}", wasm = wasm.display())),
                    )?;
                }
            }
        }

        if errors > 0 {
            std::process::exit(1);
        }

        Ok(())
    }
}

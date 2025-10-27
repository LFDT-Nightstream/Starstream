use crate::{ProgramIdx, Transaction};
use core::fmt::Write;

impl Transaction {
    /// Generate a Mermaid-syntax sequence diagram of the transaction.
    pub fn to_mermaid_diagram(&self) -> String {
        let inner = self.store.data();
        let mut output = String::new();

        // https://mermaid.js.org/syntax/sequenceDiagram
        let _ = writeln!(output, "sequenceDiagram");
        let _ = writeln!(
            output,
            "participant {} as {:?}",
            ProgramIdx::Root.0,
            ProgramIdx::Root
        );

        for witness in inner.witnesses.iter() {
            if witness.is_create {
                let program = &inner.programs[witness.to_program.0];
                let _ = writeln!(
                    output,
                    "create participant {} as {}",
                    witness.to_program.0, program.entry_point
                );
            }
            if witness.is_destroy {
                let _ = writeln!(output, "destroy {}", witness.from_program.0);
            }
            let _ = writeln!(
                output,
                "{}->>{}: {:?}",
                witness.from_program.0, witness.to_program.0, witness.values
            );
        }

        output
    }
}

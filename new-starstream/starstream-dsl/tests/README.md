We're using a mix of ordinary integration tests, snapshot tests, and property tests.

Snapshot tests using `insta`:
- Parser outputs expected AST for certain known inputs.
- Compiler outputs expected opcodes for certain known AST.
- Interpreter outputs expected result for certain program execution

Property tests using `quickcheck`:
- Formatter (prettier) still gives the same opcodes
- Sugaring and desugaring still gives the same opcodes
- Debug builds gives the same result as production builds
- No crashes (arbitrary small program works)
- Renaming gives the same opcodes
- Determinism (running a program twice gives the same result)
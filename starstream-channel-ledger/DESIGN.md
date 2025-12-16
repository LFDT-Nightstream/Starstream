The ledger is a ledger of UTXOs.

Each transaction still consumes outputs of previous transactions as inputs
and has its own outputs, and each output may be consumed only once,
except for reference inputs which are observed without consumption.

However, programs aren't spending conditions, and instead model
state machines/coroutines as WASM programs. Programs interact with the "outside"
world via channels.

Channels can transport either other channels or an i32.
Channels can not be copied and always have only two ends.

Each UTXO has a list of channels it has open, each with an index.
The list can be sparse.

The program is
- the WASM module
- hash of the "initial" transaction
- index of "init'ed" programs within the initial transaction
- list of (hashes of) referenced programs

The initial transaction is the transaction that contains the "initial"
instance of a program. The initial instance knows it's initial, that is
the only difference.

The imports given to the program are as such:
```rust
fn read_input(index: u32);
fn write_output(index: u32, value: u32);
fn length_input() -> u32;
fn set_length_output(length: u32);
fn is_initial() -> bool;
fn exit();
fn connect(at: u32); // create channel at index to other program doing `connect`
fn sync(at: u32, value: u32); // on channel, "synchronize" with other side with a matching i32
fn swap(at: u32, swapped_at: u32); // on channel, pass through other (invalid) channel, and get (invalid) channel back.
fn handshake_any(at: u32); // perform a handshake on channel without checking anything
fn handshake_self(at: u32); // perform a handshake on channel, checking that the other side is also the same program
fn handshake(at: u32, program_index: u32); // perform a handshake on channel, checking that the other side is the specified program
fn ping(at: u32); // perform a ping on channel, must be matched with a pong from the other side
fn pong(at: u32); // perform a pong on channel, must be matched with a ping from the other side
fn connect_own(at: (u32, u32)); // same as `connect`, but done with self (effectively channel creation)
fn swap_own(at: (u32, u32)); // same as `swap`, but done with self (effectively channel index reordering)
fn close(at: u32); // close channenl, making the other end invalid implicitly
fn release(at: u32); // give up channel, letting anyone take it
fn fail(); // not provable, invalid
fn witness() -> u32;
```

The module should export one function:
```rust
fn main();
```

Before exiting (either by `exit` or by returning at the top-level), it should
write the output length, and one byte at each valid index for the output.
In the case that the UTXO has just been made, the input is empty.
If the UTXO is to be destroyed, the output should be empty, which is effectively
the same.
If the UTXO is the initial one, then `is_initial` returns true.
There can ever only be one initial UTXO for a program (as defined above).

The interface is a bit peculiar to simplify the implementation of Starstream.

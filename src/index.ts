// example.js
import binaryen from "binaryen";

// Create a module from text.
import { readFile } from 'fs/promises';
const data = await readFile("build/release.wasm");
console.log('input', data.byteLength);
const ir = binaryen.readBinary(data);

// Run the Asyncify pass, with (minor) optimizations.
binaryen.setOptimizeLevel(4);
binaryen.setPassArgument("asyncify-imports", "env.emit");
let sz = 1;
ir.setMemory(sz, sz);
ir.setFeatures(binaryen.Features.All);
ir.runPasses(['asyncify']);

const State = {
  NORMAL: 0,
  UNWIND: 1,
  REWIND: 2,
};

// Get a WebAssembly binary and compile it to an instance.
const binary = ir.emitBinary();
console.log('output', binary.byteLength);
const compiled = new WebAssembly.Module(binary);
const instance = new WebAssembly.Instance(compiled, {
  env: {
    abort: function () {
      throw 'ABORT';
    },
    sleep: function(ms) {
      if (!sleeping) {
        // We are called in order to start a sleep/unwind.
        console.log('sleep...');
        // Fill in the data structure. The first value has the stack location,
        // which for simplicity we can start right after the data structure itself.
        view[DATA_ADDR >> 2] = DATA_ADDR + 8;
        // The end of the stack will not be reached here anyhow.
        view[DATA_ADDR + 4 >> 2] = 1024;
        wasmExports.asyncify_start_unwind(DATA_ADDR);
        sleeping = true;
        // Resume after the proper delay.
        setTimeout(function() {
          console.log('timeout ended, starting to rewind the stack');
          wasmExports.asyncify_start_rewind(DATA_ADDR);
          // The code is now ready to rewind; to start the process, enter the
          // first function that should be on the call stack.
          wasmExports.main();
        }, ms);
      } else {
        // We are called as part of a resume/rewind. Stop sleeping.
        console.log('...resume');
        wasmExports.asyncify_stop_rewind();
        sleeping = false;
      }
    },
    emit: function (dbgVal) {
      if (wasmExports.asyncify_get_state() == State.NORMAL) {
        console.log('emit', dbgVal);

        view[DATA_ADDR >> 2] = DATA_ADDR + 8;
        view[(DATA_ADDR + 4) >> 2] = 1024;
        wasmExports.asyncify_start_unwind(DATA_ADDR);
        sleeping = true;
      } else {
        console.log('...resume', dbgVal);
        wasmExports.asyncify_stop_rewind();
        sleeping = false;
      }
    },
  }
});
const wasmExports = instance.exports as any;
const view = new Int32Array(wasmExports.memory.buffer);

// Global state for running the program.
const DATA_ADDR = 16; // Where the unwind/rewind data structure will live.
let sleeping = false;

// Run the program. When it pauses control flow gets to here, as the
// stack has unwound.
while (true) {
  console.log('r', wasmExports.main());
  console.log(wasmExports.memory.buffer.byteLength);
  for (let i = wasmExports.memory.buffer.byteLength - 1; i >= 0; --i) {
    if (new Uint8Array(wasmExports.memory.buffer)[i] != 0) {
      console.log('ends at', i, new Uint8Array(wasmExports.memory.buffer)[i]);
      break;
    }
  }
  if (!sleeping) {
    break;
  }
  wasmExports.asyncify_stop_unwind();
  console.log('stack unwound');
  // we can do whatever we want now...
  wasmExports.asyncify_start_rewind(sleeping);
}

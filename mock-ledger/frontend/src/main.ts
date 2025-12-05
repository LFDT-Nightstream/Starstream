import './style.css'
import { init, Terminal } from 'ghostty-web';
import { generate } from '@bytecodealliance/jco/component';

// @ts-ignore
import foo from './builtin/foo.wasm?raw' with { type: "text" };
import { Session } from './session';
const transpiledResult = await generate(new TextEncoder().encode(foo), {
  name: 'adsf',
  // outDir: `./zxcv`
});
console.log(transpiledResult);

// Initialize ghostty-web
await init();

// Create terminal instance
const term = new Terminal({
  fontSize: 14, // 1:100, 14:800, 15:900
  cols: 100, // cols * 8
  rows: 50, // height = fontSize * rows
  theme: {
    background: '#1a1b26',
    foreground: '#a9b1d6',
  },
  cursorBlink: true,
  cursorStyle: 'block',
});

// Open terminal in the DOM
const terminalElement = document.getElementById('terminal-content');
if (!terminalElement) {
  throw new Error('Terminal element not found');
}
term.open(terminalElement);

// Write initial prompt
term.write('Component Terminal - Type "help" for available commands\r\n');
term.write('$ ');

const session = new Session(term);

// Command buffer to handle multi-character input
let commandBuffer = '';
let isProcessing = false;

// Handle user input
term.onData((data) => {
  // Handle special keys
  if (data === '\r' || data === '\n') {
    // Enter key - execute command
    if (!isProcessing && commandBuffer.trim()) {
      isProcessing = true;
      term.write('\r\n');
      
      session.executeCommand(term, commandBuffer)
        .then((result) => {
          if (result.output) {
            term.write(result.output);
            if (!result.output.endsWith('\n')) {
              term.write('\r\n');
            }
          }
          // Write prompt
          term.write('$ ');
          commandBuffer = '';
          isProcessing = false;
        })
        .catch((error) => {
          term.write(`Error: ${error instanceof Error ? error.message : String(error)}\r\n`);
          term.write('$ ');
          commandBuffer = '';
          isProcessing = false;
        });
    } else if (!isProcessing) {
      // Empty line, just show prompt
      term.write('$ ');
    }
  } else if (data === '\x7f' || data === '\b') {
    // Backspace
    if (!isProcessing && commandBuffer.length > 0) {
      commandBuffer = commandBuffer.slice(0, -1);
      term.write('\b \b');
    }
  } else if (data === '\x03') {
    // Ctrl+C - cancel current command
    if (isProcessing) {
      isProcessing = false;
      commandBuffer = '';
      term.write('^C\r\n$ ');
    } else {
      commandBuffer = '';
      term.write('^C\r\n$ ');
    }
  } else if (data >= ' ' && data <= '~') {
    // Printable characters
    if (!isProcessing) {
      commandBuffer += data;
      term.write(data);
    }
  } else {
    // Other control characters - just write them (handles arrow keys, etc.)
    if (!isProcessing) {
      term.write(data);
    }
  }
});

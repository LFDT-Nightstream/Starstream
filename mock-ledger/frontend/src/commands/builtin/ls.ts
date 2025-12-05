import type { RegisteredCommand } from '../command-handler.ts';

const command: RegisteredCommand = {
  name: 'ls',
  execute: async (_session, _args) => {
    // Simple ls implementation
    const files = ['README.md', 'package.json', 'index.ts'];
    return {
      output: files.join('  '),
      exitCode: 0,
    };
  },
};

export default command;

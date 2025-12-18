import type { RegisteredCommand } from '../command-handler.ts';

const command: RegisteredCommand = {
  name: 'whoami',
  execute: async (_session, _args) => {
    // TODO: return wallet
    return {
      output: 'user',
      exitCode: 0,
    };
  },
};

export default command;

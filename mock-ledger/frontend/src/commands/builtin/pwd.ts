import type { RegisteredCommand } from '../command-handler.ts';

const command: RegisteredCommand = {
  name: 'pwd',
  execute: async (_session, _args) => {
    return {
      output: '/',
      exitCode: 0,
    };
  },
};

export default command;

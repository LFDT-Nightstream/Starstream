import type { RegisteredCommand } from '../command-handler.ts';

const command: RegisteredCommand = {
  name: 'echo',
  execute: async (_session, args) => {
    return {
      output: args.join(' '),
      exitCode: 0,
    };
  },
};

export default command;

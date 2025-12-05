import type { Terminal } from "ghostty-web";
import { CommandRegistry, parseCommand, type CommandResult } from "./commands/command-handler";
import type { State } from "./state";
import builtins from "./commands/builtin";

export class Session {

  /**
   * Global command registry instance
   */
  commandRegistry = new CommandRegistry();
  state: State = {
    cwd: '/',
  };
  terminal: Terminal;
  constructor(terminal: Terminal) {
    this.terminal = terminal;
    builtins.forEach(builtin => this.commandRegistry.register(builtin));
  }

  async executeCommand(terminal: Terminal, line: string): Promise<CommandResult> {
    const { command, args } = parseCommand(line);
    if (!command) {
      return { output: '', exitCode: 0 };
    }

    // Handle built-in commands
    if (command === 'help') {
      const commands = this.commandRegistry.list();
      return {
        output: `Available commands:\n${commands.map(cmd => `  ${cmd}`).join('\n')}\n\nUse 'help <command>' for more information about a specific command.`,
        exitCode: 0,
      };
    }

    if (command === 'clear') {
      // This is handled by the terminal itself, but we can acknowledge it
      terminal.clear();
      return { output: '', exitCode: 0 };
    }

    // Try to find and execute the command
    const cmd = this.commandRegistry.get(command);
    if (!cmd) {
      return {
        output: `Command not found: ${command}\nType 'help' to see available commands.`,
        exitCode: 1,
      };
    }

    try {
      return await cmd.execute(this, args);
    } catch (error) {
      return {
        output: `Error executing ${command}: ${error instanceof Error ? error.message : String(error)}`,
        exitCode: 1,
      };
    }
  }
}

import type { Session } from "../session";

export interface CommandResult {
  output: string;
  exitCode: number;
}

export interface RegisteredCommand {
  name: string;
  execute: (session: Session, args: string[]) => Promise<CommandResult>;
}

// type WasmComponent = WebAssembly.Module

/**
 * Registry of available WASM commands
 */
export class CommandRegistry {
  private commands: Map<string, RegisteredCommand> = new Map();
  // private wasmComponents: Map<string, WebAssembly.Module> = new Map();

  /**
   * Register a WASM command
   */
  register(command: RegisteredCommand): void {
    this.commands.set(command.name, command);
  }

  /**
   * Register a WASM component that can be loaded and executed
   */
  // async registerComponent(
  //   name: string,
  //   wasmUrl: string,
  //   executeFn: (module: WebAssembly.Instance, args: string[]) => Promise<CommandResult>
  // ): Promise<void> {
  //   const response = await fetch(wasmUrl);
  //   const bytes = await response.arrayBuffer();
  //   const module = await WebAssembly.compile(bytes);
  //   this.wasmComponents.set(name, module);

  //   this.register({
  //     name,
  //     execute: async (args: string[]) => {
  //       const instance = await WebAssembly.instantiate(module);
  //       return executeFn(instance, args);
  //     },
  //   });
  // }

  /**
   * Get a command by name
   */
  get(name: string): RegisteredCommand | undefined {
    return this.commands.get(name);
  }

  /**
   * List all available commands
   */
  list(): string[] {
    return Array.from(this.commands.keys());
  }

  /**
   * Check if a command exists
   */
  has(name: string): boolean {
    return this.commands.has(name);
  }
}

/**
 * Parse a command line into command name and arguments
 */
export function parseCommand(line: string): { command: string; args: string[] } {
  const trimmed = line.trim();
  if (!trimmed) {
    return { command: '', args: [] };
  }

  const parts = trimmed.split(/\s+/);
  return {
    command: parts[0],
    args: parts.slice(1),
  };
}


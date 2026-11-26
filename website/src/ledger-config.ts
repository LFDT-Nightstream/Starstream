/** Options of the in-browser ledger, as entered in the form. */
export interface NodeConfig {
  network: string;
  /** Paths of the contracts the genesis script imports. */
  imports: string[];
  /** Path of a contract whose script to run at genesis, imported as well. */
  genesisContract: string;
  genesisScript: string;
  /** WAVE-encoded arguments of `genesisScript`. */
  genesisArgs: string[];
  fuel: string;
  maxRequests: string;
  cardanoBlockHeight: string;
  cardanoCurrentSlot: string;
}

export const DEFAULT_CONFIG: NodeConfig = {
  network: "dev",
  imports: [],
  genesisContract: "",
  genesisScript: "",
  genesisArgs: [],
  fuel: "1000000000",
  maxRequests: "65535",
  cardanoBlockHeight: "0",
  cardanoCurrentSlot: "0",
};

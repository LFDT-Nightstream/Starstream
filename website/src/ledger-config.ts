/** Options of the in-browser ledger, as entered in the form. */
export interface NodeConfig {
  network: string;
  /** Hex-encoded admin public key, by default the one of `admin.key`. */
  admin: string;
  /** Path of the TOML genesis, if any. */
  genesis: string;
  /** Paths of the contracts the genesis imports. */
  imports: string[];
  /** Path of a contract whose script to run at genesis, imported as well. */
  genesisContract: string;
  genesisScript: string;
  /** WAVE-encoded arguments of `genesisScript`, as typed in the terminal. */
  genesisArgs: string;
  fuel: string;
  maxRequests: string;
  cardanoBlockHeight: string;
  cardanoCurrentSlot: string;
}

export const DEFAULT_CONFIG: NodeConfig = {
  network: "dev",
  admin: "",
  genesis: "",
  imports: [],
  genesisContract: "",
  genesisScript: "",
  genesisArgs: "",
  fuel: "",
  maxRequests: "",
  cardanoBlockHeight: "",
  cardanoCurrentSlot: "",
};

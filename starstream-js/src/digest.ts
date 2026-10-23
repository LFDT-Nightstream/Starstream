/** Compute the ledger digest of `data`: the multibase base32-lower sha2-256 multihash. */
export async function digest(data: Uint8Array<ArrayBuffer>): Promise<string> {
  const hash = new Uint8Array(await crypto.subtle.digest("sha-256", data));
  return "b" + base32Lower(Uint8Array.of(0x12, 0x20, ...hash));
}

function base32Lower(data: Uint8Array): string {
  const alphabet = "abcdefghijklmnopqrstuvwxyz234567";
  let s = "";
  let bits = 0;
  let acc = 0;
  for (const byte of data) {
    acc = (acc << 8) | byte;
    bits += 8;
    while (bits >= 5) {
      bits -= 5;
      s += alphabet[(acc >> bits) & 31];
    }
  }
  if (bits > 0) {
    s += alphabet[(acc << (5 - bits)) & 31];
  }
  return s;
}

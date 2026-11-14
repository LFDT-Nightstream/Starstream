import type {
  GlobalValue,
  Transaction,
} from "./bindings/starstream/ledger/types.js";
import { Writer } from "@bytecodealliance/wrpc";

const TRANSACTION_CONTEXT = 2;

const COSE_SIGN_TAG = 98;
const COSE_HEADER_ALG = 1;
const COSE_HEADER_KID = 4;
const COSE_ALG_EDDSA = -8;

const GLOBAL_VALUE_INDEX = { i32: 0, i64: 1, f32: 2, f64: 3, v128: 4 };

const encoder = new TextEncoder();

/** Ed25519 signer of ledger envelopes. */
export interface Signer {
  /** Raw 32-byte Ed25519 public key. */
  publicKey: Uint8Array;
  /** Sign `data` with the Ed25519 secret key of `publicKey`. */
  sign(data: Uint8Array<ArrayBuffer>): Promise<Uint8Array>;
}

function head(w: Writer, major: number, n: number | bigint): void {
  const v = BigInt(n);
  if (v !== BigInt.asUintN(64, v)) {
    throw new Error(`${v} does not fit in 64 bits`);
  }
  if (v < 24n) {
    w.u8((major << 5) | Number(v));
    return;
  }
  const len = v < 0x100n ? 1 : v < 0x10000n ? 2 : v < 0x100000000n ? 4 : 8;
  w.u8((major << 5) | (24 + Math.log2(len)));
  for (let i = len - 1; i >= 0; i--) {
    w.u8(Number((v >> BigInt(8 * i)) & 0xffn));
  }
}

function int(w: Writer, v: number | bigint): void {
  const n = BigInt(v);
  if (n < 0n) {
    head(w, 1, -1n - n);
  } else {
    head(w, 0, n);
  }
}

function bytes(w: Writer, v: Uint8Array): void {
  head(w, 2, v.length);
  w.bytes(v);
}

function text(w: Writer, v: string): void {
  const buf = encoder.encode(v);
  head(w, 3, buf.length);
  w.bytes(buf);
}

function array(w: Writer, n: number): void {
  head(w, 4, n);
}

function map(w: Writer, n: number): void {
  head(w, 5, n);
}

function encodeGlobalValue(w: Writer, global: GlobalValue): void {
  array(w, 2);
  int(w, GLOBAL_VALUE_INDEX[global.kind]);
  array(w, 1);
  if (global.kind === "v128") {
    const buf = new Uint8Array(16);
    const view = new DataView(buf.buffer);
    view.setBigUint64(0, global.value[0], true);
    view.setBigUint64(8, global.value[1], true);
    bytes(w, buf);
  } else {
    int(w, global.value);
  }
}

function encodeTransaction(w: Writer, tx: Transaction): void {
  array(w, 4);
  array(w, tx.inputs.length);
  for (const input of tx.inputs) {
    array(w, 2);
    text(w, input.transaction);
    int(w, input.index);
  }
  array(w, tx.outputs.length);
  for (const output of tx.outputs) {
    array(w, 5);
    text(w, output.contract);
    text(w, output.instance);
    array(w, output.methods.length);
    for (const method of output.methods) {
      array(w, 4);
      method.forEach((v) => int(w, v));
    }
    bytes(w, output.storage);
    array(w, output.state.length);
    for (const state of output.state) {
      array(w, 3);
      array(w, state.memories.length);
      state.memories.forEach((v) => int(w, v));
      array(w, state.globals.length);
      state.globals.forEach((global) => encodeGlobalValue(w, global));
      array(w, state.data.length);
      for (const data of state.data) {
        array(w, 3);
        int(w, data.memoryIndex);
        int(w, data.offset);
        bytes(w, data.data);
      }
    }
  }
  array(w, tx.events.length);
  for (const event of tx.events) {
    array(w, 3);
    text(w, event.abiName);
    text(w, event.name);
    bytes(w, event.params);
  }
  bytes(w, tx.proof);
}

/**
 * Build a `COSE_Sign` envelope of `tx` for `network`, signed by `signer`,
 * encoded like `starstream_ledger::client::build_transaction_envelope`.
 */
export async function buildTransactionEnvelope(
  signer: Signer,
  network: string,
  tx: Transaction,
): Promise<Uint8Array<ArrayBuffer>> {
  const payload = new Writer();
  array(payload, 3);
  text(payload, network);
  int(payload, TRANSACTION_CONTEXT);
  encodeTransaction(payload, tx);
  const payloadBytes = payload.finish();

  const protectedHeader = new Writer();
  map(protectedHeader, 2);
  int(protectedHeader, COSE_HEADER_ALG);
  int(protectedHeader, COSE_ALG_EDDSA);
  int(protectedHeader, COSE_HEADER_KID);
  bytes(protectedHeader, signer.publicKey);
  const protectedBytes = protectedHeader.finish();

  const toBeSigned = new Writer();
  array(toBeSigned, 5);
  text(toBeSigned, "Signature");
  bytes(toBeSigned, new Uint8Array());
  bytes(toBeSigned, protectedBytes);
  bytes(toBeSigned, new Uint8Array());
  bytes(toBeSigned, payloadBytes);
  const signature = await signer.sign(
    toBeSigned.finish() as Uint8Array<ArrayBuffer>,
  );

  const envelope = new Writer();
  head(envelope, 6, COSE_SIGN_TAG);
  array(envelope, 4);
  bytes(envelope, new Uint8Array());
  map(envelope, 0);
  bytes(envelope, payloadBytes);
  array(envelope, 1);
  array(envelope, 3);
  bytes(envelope, protectedBytes);
  map(envelope, 0);
  bytes(envelope, signature);
  return envelope.finish() as Uint8Array<ArrayBuffer>;
}

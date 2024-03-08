import { readFileSync } from "node:fs";

export async function load(wasmFileName) {
  const wasmBuffer = readFileSync(wasmFileName);
  const wasmModule = await WebAssembly.instantiate(wasmBuffer);
  return wasmModule.instance.exports;
}

import { readFileSync } from "node:fs";

const scalaJSHelpers = {
  // Box int to java.lang.Integer (upcast)
  bI: (x) => x,
  // Unbox any to int (downcast)
  uI: (x) => x | 0, // | 0 turns `null` into 0
  // Unbox any to int|null (downcast)
  uIN: (x) => (x !== null) ? (x | 0) : null,
  // Test whethere an any is an int (type test)
  tI: (x) => typeof x === 'number' && Object.is(x | 0, x),
}

export async function load(wasmFileName) {
  const wasmBuffer = readFileSync(wasmFileName);
  const wasmModule = await WebAssembly.instantiate(wasmBuffer, {
    "__scalaJSHelpers": scalaJSHelpers,
  });
  return wasmModule.instance.exports;
}

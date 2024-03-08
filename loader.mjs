import { readFileSync } from "node:fs";

const scalaJSHelpers = {
  // BinaryOp.===
  is: Object.is,
  // Box int to java.lang.Integer (upcast)
  bI: (x) => x,
  // Unbox any to int (downcast)
  uI: (x) => x | 0, // | 0 turns `null` into 0
  // Unbox any to int|null (downcast)
  uIN: (x) => (x !== null) ? (x | 0) : null,
  // Test whethere an any is an int (type test)
  tI: (x) => typeof x === 'number' && Object.is(x | 0, x),

  // Strings
  emptyString: () => "",
  stringLength: (s) => s.length,
  stringCharAt: (s, i) => s.charCodeAt(i),
  jsValueToString: (x) => "" + x,
  booleanToString: (b) => b ? "true" : "false",
  charToString: (c) => String.fromCharCode(c),
  intToString: (i) => "" + i,
  doubleToString: (d) => "" + d,
  stringConcat: (x, y) => ("" + x) + y, // the added "" is for the case where x === y === null
  isString: (x) => typeof x === 'string',
}

export async function load(wasmFileName) {
  const wasmBuffer = readFileSync(wasmFileName);
  const wasmModule = await WebAssembly.instantiate(wasmBuffer, {
    "__scalaJSHelpers": scalaJSHelpers,
  });
  return wasmModule.instance.exports;
}

import { readFileSync } from "node:fs";

const scalaJSHelpers = {
  // BinaryOp.===
  is: Object.is,

  // undefined
  undef: () => void 0,
  isUndef: (x) => x === (void 0),

  // Boxes (upcast) -- most are identity at the JS level but with different types in Wasm
  bZ: (x) => x !== 0,
  bB: (x) => x,
  bS: (x) => x,
  bI: (x) => x,
  bF: (x) => x,
  bD: (x) => x,

  // Unboxes (downcast, null is converted to the zero of the type)
  uZ: (x) => x | 0,
  uB: (x) => (x << 24) >> 24,
  uS: (x) => (x << 16) >> 16,
  uI: (x) => x | 0,
  uF: (x) => Math.fround(x),
  uD: (x) => +x,

  // Unboxes to primitive or null (downcast to the boxed classes)
  uNZ: (x) => (x !== null) ? (x | 0) : null,
  uNB: (x) => (x !== null) ? ((x << 24) >> 24) : null,
  uNS: (x) => (x !== null) ? ((x << 16) >> 16) : null,
  uNI: (x) => (x !== null) ? (x | 0) : null,
  uNF: (x) => (x !== null) ? Math.fround(x) : null,
  uND: (x) => (x !== null) ? +x : null,

  // Type tests
  tZ: (x) => typeof x === 'boolean',
  tB: (x) => typeof x === 'number' && Object.is((x << 24) >> 24, x),
  tS: (x) => typeof x === 'number' && Object.is((x << 16) >> 16, x),
  tI: (x) => typeof x === 'number' && Object.is(x | 0, x),
  tF: (x) => typeof x === 'number' && (Math.fround(x) === x || x !== x),
  tD: (x) => typeof x === 'number',

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
